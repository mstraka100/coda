open Core
open Async
open Protocols.Coda_transition_frontier
open Cache_lib
open Pipe_lib
open Coda_base

(** [Ledger_catchup] is a procedure that connects a foreign external transition
    into a transition frontier by requesting a path of external_transitions
    from its peer. It receives the state_hash to catchup from
    [Catchup_scheduler]. With that state_hash, it will ask its peers for
    a path of external_transitions from their root to the state_hash it is
    asking for. It will then perform the following validations on each
    external_transition:

    1. The root should exist in the frontier. The frontier should not be
    missing too many external_transitions, so the querying node should have the
    root in its transition_frontier.

    2. Each transition is checked through [Transition_processor.Validator] and
    [Protocol_state_validator]

    If any of the external_transitions is invalid,
    1) the sender is punished;
    2) those external_transitions that already passed validation would be
       invalidated.
    Otherwise, [Ledger_catchup] will build a corresponding breadcrumb path from
    the path of external_transitions. A breadcrumb from the path is built using
    its corresponding external_transition staged_ledger_diff and applying it to
    its preceding breadcrumb staged_ledger to obtain its corresponding
    staged_ledger. If there was an error in building the breadcrumbs, then
    1) catchup will punish the sender for sending a faulty staged_ledger_diff;
    2) catchup would invalidate the cached transitions.
    After building the breadcrumb path, [Ledger_catchup] will then send it to
    the [Processor] via writing them to catchup_breadcrumbs_writer. *)

module Make (Inputs : Inputs.S) :
  Catchup_intf
  with type external_transition_verified :=
              Inputs.External_transition.Verified.t
   and type unprocessed_transition_cache :=
              Inputs.Unprocessed_transition_cache.t
   and type transition_frontier := Inputs.Transition_frontier.t
   and type transition_frontier_breadcrumb :=
              Inputs.Transition_frontier.Breadcrumb.t
   and type state_hash := State_hash.t
   and type network := Inputs.Network.t
   and type trust_system := Trust_system.t = struct
  open Inputs

  type verification_error =
    [ `In_frontier of State_hash.t
    | `In_process of State_hash.t Cache_lib.Intf.final_state
    | `Invalid_proof
    | `Disconnected ]

  type 'a verification_result = ('a, verification_error) Result.t

  let verify_transition ~logger ~trust_system ~frontier
      ~unprocessed_transition_cache transition_enveloped =
    let cached_verified_transition =
      let open Deferred.Result.Let_syntax in
      let transition = Envelope.Incoming.data transition_enveloped in
      let%bind (_ : External_transition.Proof_verified.t) =
        ( Protocol_state_validator.validate_proof transition
          :> External_transition.Proof_verified.t verification_result
             Deferred.t )
      in
      let verified_transition_with_hash_enveloped =
        Envelope.Incoming.map transition_enveloped ~f:(fun transition ->
            (* We need to coerce the transition from a proof_verified
         transition to a fully verified in
         order to add the transition to be added to the
         transition frontier and to be fed through the
         transition_handler_validator. *)
            let (`I_swear_this_is_safe_see_my_comment verified_transition) =
              External_transition.to_verified transition
            in
            With_hash.of_data verified_transition
              ~hash_data:
                (Fn.compose Consensus.Protocol_state.hash
                   External_transition.Verified.protocol_state) )
      in
      Deferred.return
      @@ ( Transition_handler_validator.validate_transition ~logger ~frontier
             ~unprocessed_transition_cache
             verified_transition_with_hash_enveloped
           :> ( (External_transition.Verified.t, State_hash.t) With_hash.t
                Envelope.Incoming.t
              , State_hash.t )
              Cached.t
              verification_result )
    in
    let open Deferred.Let_syntax in
    let sender = Envelope.Incoming.sender transition_enveloped in
    match%bind cached_verified_transition with
    | Ok x ->
        Deferred.return @@ Ok (Either.Second x)
    | Error (`In_frontier hash) ->
        Logger.trace logger ~module_:__MODULE__ ~location:__LOC__
          "transition queried during ledger catchup has already been seen" ;
        Deferred.return @@ Ok (Either.First hash)
    | Error (`In_process consumed_state) -> (
        Logger.trace logger ~module_:__MODULE__ ~location:__LOC__
          "transition queried during ledger catchup is still in process in \
           one of the components in transition_frontier" ;
        match%map Ivar.read consumed_state with
        | `Failed ->
            Logger.trace logger ~module_:__MODULE__ ~location:__LOC__
              "transition queried during ledger catchup failed" ;
            Error (Error.of_string "Previous transition failed")
        | `Success hash ->
            Ok (Either.First hash) )
    | Error `Invalid_proof ->
        let%map () =
          Trust_system.record_envelope_sender trust_system logger sender
            ( Trust_system.Actions.Gossiped_invalid_transition
            , Some ("invalid proof", []) )
        in
        Error (Error.of_string "invalid proof")
    | Error `Disconnected ->
        let%map () =
          Trust_system.record_envelope_sender trust_system logger sender
            (Trust_system.Actions.Disconnected_chain, None)
        in
        Error (Error.of_string "disconnected chain")

  let take_while_map_result_rev ~f list =
    let open Deferred.Or_error.Let_syntax in
    let%map result, initial_state_hash =
      Deferred.Or_error.List.fold list ~init:([], None)
        ~f:(fun (acc, initial_state_hash) elem ->
          let open Deferred.Let_syntax in
          if Option.is_some initial_state_hash then
            Deferred.Or_error.return (acc, initial_state_hash)
          else
            match%bind f elem with
            | Error e ->
                List.iter acc
                  ~f:(Fn.compose ignore Cached.invalidate_with_failure) ;
                Deferred.return (Error e)
            | Ok (Either.First hash) ->
                Deferred.Or_error.return (acc, Some hash)
            | Ok (Either.Second transition) ->
                Deferred.Or_error.return (transition :: acc, None) )
    in
    (result, Option.value_exn initial_state_hash)

  let get_transitions_and_compute_breadcrumbs ~logger ~trust_system ~network
      ~frontier ~num_peers ~unprocessed_transition_cache ~target_forest =
    let peers = Network.random_peers network num_peers in
    let target_hash, subtrees = target_forest in
    let open Deferred.Or_error.Let_syntax in
    Logger.trace logger "doing a catchup job with target $target_hash"
      ~module_:__MODULE__ ~location:__LOC__
      ~metadata:[("target_hash", State_hash.to_yojson target_hash)] ;
    Deferred.Or_error.find_map_ok peers ~f:(fun peer ->
        O1trace.trace_recurring_task "ledger catchup" (fun () ->
            match%bind Network.catchup_transition network peer target_hash with
            | None ->
                Deferred.return
                @@ Or_error.errorf
                     !"Peer %{sexp:Network_peer.Peer.t} did not have transition"
                     peer
            | Some queried_transitions -> (
                let rev_queried_transitions =
                  Non_empty_list.rev queried_transitions
                in
                let last = Non_empty_list.head rev_queried_transitions in
                let%bind () =
                  if
                    State_hash.equal
                      (Consensus.Protocol_state.hash
                         (External_transition.protocol_state last))
                      target_hash
                  then return ()
                  else (
                    ignore
                      Trust_system.(
                        record trust_system logger peer.host
                          Actions.
                            ( Violated_protocol
                            , Some
                                ( "Peer returned a different target \
                                   transition than requested"
                                , [] ) )) ;
                    Deferred.return (Error (Error.of_string "")) )
                in
                let%bind verified_transitions, initial_state_hash =
                  take_while_map_result_rev
                    Non_empty_list.(to_list rev_queried_transitions)
                    ~f:(fun transition ->
                      verify_transition ~logger ~trust_system ~frontier
                        ~unprocessed_transition_cache
                        (Envelope.Incoming.wrap ~data:transition
                           ~sender:(Envelope.Sender.Remote peer.host)) )
                in
                let split_last xs =
                  let init = List.take xs (List.length xs - 1) in
                  let last = List.last_exn xs in
                  (init, last)
                in
                let subtrees_of_transitions =
                  if List.length verified_transitions > 0 then
                    let rest, target_transition =
                      split_last verified_transitions
                    in
                    [ List.fold_right rest
                        ~init:(Rose_tree.T (target_transition, subtrees))
                        ~f:(fun transition acc ->
                          Rose_tree.T (transition, [acc]) ) ]
                  else subtrees
                in
                let open Deferred.Let_syntax in
                match%bind
                  Breadcrumb_builder.build_subtrees_of_breadcrumbs ~logger
                    ~trust_system ~frontier ~initial_hash:initial_state_hash
                    subtrees_of_transitions
                with
                | Ok result ->
                    Deferred.Or_error.return result
                | error ->
                    List.iter verified_transitions
                      ~f:(Fn.compose ignore Cached.invalidate_with_failure) ;
                    Deferred.return error ) ) )

  let run ~logger ~trust_system ~network ~frontier ~catchup_job_reader
      ~catchup_breadcrumbs_writer ~unprocessed_transition_cache =
    Strict_pipe.Reader.iter catchup_job_reader ~f:(fun (hash, subtrees) ->
        ( match%bind
            get_transitions_and_compute_breadcrumbs ~logger ~trust_system
              ~network ~frontier ~num_peers:8 ~unprocessed_transition_cache
              ~target_forest:(hash, subtrees)
          with
        | Ok trees ->
            Logger.trace logger ~module_:__MODULE__ ~location:__LOC__
              "about to write to the catchup breadcrumbs pipe" ;
            if Strict_pipe.Writer.is_closed catchup_breadcrumbs_writer then (
              Logger.trace logger ~module_:__MODULE__ ~location:__LOC__
                "catchup breadcrumbs pipe was closed; attempt to write to \
                 closed pipe" ;
              Deferred.unit )
            else Strict_pipe.Writer.write catchup_breadcrumbs_writer trees
        | Error e ->
            Logger.info logger ~module_:__MODULE__ ~location:__LOC__
              !"All peers either sent us bad data, didn't have the info, or \
                our transition frontier moved too fast: %s"
              (Error.to_string_hum e) ;
            List.iter subtrees ~f:(fun subtree ->
                Rose_tree.iter subtree ~f:(fun cached_transition ->
                    Cached.invalidate_with_failure cached_transition |> ignore
                ) ) ;
            Logger.trace logger ~module_:__MODULE__ ~location:__LOC__
              "garbage collected failed cached transitions" ;
            Deferred.unit )
        |> don't_wait_for ;
        Deferred.unit )
    |> don't_wait_for
end
