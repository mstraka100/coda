open Core
open Async
open Pipe_lib
open O1trace
module Time = Coda_base.Block_time

module type Inputs_intf = sig
  include Protocols.Coda_pow.Inputs_intf

  module Pending_coinbase_witness :
    Protocols.Coda_pow.Pending_coinbase_witness_intf
    with type pending_coinbases := Pending_coinbase.t

  module Ledger_db : sig
    type t
  end

  module Masked_ledger : sig
    type t
  end

  module Diff_hash : Protocols.Coda_transition_frontier.Diff_hash

  module Diff_mutant :
    Protocols.Coda_transition_frontier.Diff_mutant
    with type external_transition := External_transition.Stable.Latest.t
     and type state_hash := Coda_base.State_hash.t
     and type scan_state := Staged_ledger.Scan_state.t
     and type hash := Diff_hash.t
     and type consensus_state := Consensus.Consensus_state.Value.Stable.V1.t
     and type pending_coinbases := Pending_coinbase.t

  module Transition_frontier :
    Protocols.Coda_transition_frontier.Transition_frontier_intf
    with type state_hash := Protocol_state_hash.t
     and type external_transition_verified := External_transition.Verified.t
     and type ledger_database := Ledger_db.t
     and type staged_ledger := Staged_ledger.t
     and type staged_ledger_diff := Staged_ledger_diff.t
     and type transaction_snark_scan_state := Staged_ledger.Scan_state.t
     and type masked_ledger := Masked_ledger.t
     and type consensus_local_state := Consensus.Local_state.t
     and type user_command := User_command.t
     and type diff_mutant :=
                ( External_transition.Stable.Latest.t
                , Coda_base.State_hash.Stable.Latest.t )
                With_hash.t
                Diff_mutant.E.t

  module Transaction_pool :
    Coda_lib.Transaction_pool_read_intf
    with type transaction_with_valid_signature :=
                User_command.With_valid_signature.t

  module Prover : sig
    val prove :
         prev_state:Consensus_mechanism.Protocol_state.Value.t
      -> prev_state_proof:Protocol_state_proof.t
      -> next_state:Consensus_mechanism.Protocol_state.Value.t
      -> Internal_transition.t
      -> Pending_coinbase_witness.t
      -> Protocol_state_proof.t Deferred.Or_error.t
  end
end

module Agent : sig
  type 'a t

  val create : f:('a -> 'b) -> 'a Linear_pipe.Reader.t -> 'b t

  val get : 'a t -> 'a option

  val with_value : f:('a -> unit) -> 'a t -> unit
end = struct
  type 'a t = {signal: unit Ivar.t; mutable value: 'a option}

  let create ~(f : 'a -> 'b) (reader : 'a Linear_pipe.Reader.t) : 'b t =
    let t = {signal= Ivar.create (); value= None} in
    don't_wait_for
      (Linear_pipe.iter reader ~f:(fun x ->
           let old_value = t.value in
           t.value <- Some (f x) ;
           if old_value = None then Ivar.fill t.signal () ;
           return () )) ;
    t

  let get t = t.value

  let rec with_value ~f t =
    match t.value with
    | Some x ->
        f x
    | None ->
        don't_wait_for (Ivar.read t.signal >>| fun () -> with_value ~f t)
end

module Singleton_supervisor : sig
  type ('data, 'a) t

  val create :
    task:(unit Ivar.t -> 'data -> ('a, unit) Interruptible.t) -> ('data, 'a) t

  val cancel : (_, _) t -> unit

  val dispatch : ('data, 'a) t -> 'data -> ('a, unit) Interruptible.t
end = struct
  type ('data, 'a) t =
    { mutable task: (unit Ivar.t * ('a, unit) Interruptible.t) option
    ; f: unit Ivar.t -> 'data -> ('a, unit) Interruptible.t }

  let create ~task = {task= None; f= task}

  let cancel t =
    match t.task with
    | Some (ivar, _) ->
        Ivar.fill ivar () ;
        t.task <- None
    | None ->
        ()

  let dispatch t data =
    cancel t ;
    let ivar = Ivar.create () in
    let interruptible =
      let open Interruptible.Let_syntax in
      t.f ivar data
      >>| fun x ->
      t.task <- None ;
      x
    in
    t.task <- Some (ivar, interruptible) ;
    interruptible
end

module Make (Inputs : Inputs_intf) :
  Coda_lib.Proposer_intf
  with type external_transition := Inputs.External_transition.t
   and type external_transition_verified :=
              Inputs.External_transition.Verified.t
   and type state_hash := Inputs.Protocol_state_hash.t
   and type ledger_hash := Inputs.Ledger_hash.t
   and type staged_ledger := Inputs.Staged_ledger.t
   and type transaction := Inputs.User_command.With_valid_signature.t
   and type protocol_state := Inputs.Consensus_mechanism.Protocol_state.Value.t
   and type protocol_state_proof := Inputs.Protocol_state_proof.t
   and type consensus_local_state := Inputs.Consensus_mechanism.Local_state.t
   and type completed_work_statement :=
              Inputs.Transaction_snark_work.Statement.t
   and type completed_work_checked := Inputs.Transaction_snark_work.Checked.t
   and type time_controller := Inputs.Time.Controller.t
   and type keypair := Inputs.Keypair.t
   and type transition_frontier := Inputs.Transition_frontier.t
   and type transaction_pool := Inputs.Transaction_pool.t
   and type time := Inputs.Time.t = struct
  open Inputs
  open Consensus_mechanism

  let time_to_ms = Fn.compose Time.Span.to_ms Time.to_span_since_epoch

  let time_of_ms = Fn.compose Time.of_span_since_epoch Time.Span.of_ms

  let lift_sync f =
    Interruptible.uninterruptible
      (Deferred.create (fun ivar -> Ivar.fill ivar (f ())))

  module Singleton_scheduler : sig
    type t

    val create : Time.Controller.t -> t

    val schedule : t -> Time.t -> f:(unit -> unit) -> unit
  end = struct
    type t =
      { mutable timeout: unit Time.Timeout.t option
      ; time_controller: Time.Controller.t }

    let create time_controller = {time_controller; timeout= None}

    let cancel t =
      match t.timeout with
      | Some timeout ->
          Time.Timeout.cancel t.time_controller timeout () ;
          t.timeout <- None
      | None ->
          ()

    let schedule t time ~f =
      cancel t ;
      let span_till_time = Time.diff time (Time.now t.time_controller) in
      let timeout =
        Time.Timeout.create t.time_controller span_till_time ~f:(fun _ ->
            t.timeout <- None ;
            f () )
      in
      t.timeout <- Some timeout
  end

  let generate_next_state ~previous_protocol_state ~time_controller
      ~staged_ledger ~transactions ~get_completed_work ~logger
      ~(keypair : Keypair.t) ~proposal_data =
    let open Interruptible.Let_syntax in
    let self = Public_key.compress keypair.public_key in
    let%bind ( diff
             , next_staged_ledger_hash
             , ledger_proof_opt
             , is_new_stack
             , coinbase_amount ) =
      Interruptible.uninterruptible
        (let open Deferred.Let_syntax in
        let diff =
          measure "create_diff" (fun () ->
              Staged_ledger.create_diff staged_ledger ~self ~logger
                ~transactions_by_fee:transactions ~get_completed_work )
        in
        let%map ( `Hash_after_applying next_staged_ledger_hash
                , `Ledger_proof ledger_proof_opt
                , `Staged_ledger _transitioned_staged_ledger
                , `Pending_coinbase_data (is_new_stack, coinbase_amount) ) =
          let%map or_error =
            Staged_ledger.apply_diff_unchecked staged_ledger diff
          in
          Or_error.ok_exn or_error
        in
        (*staged_ledger remains unchanged and transitioned_staged_ledger is discarded because the external transtion created out of this diff will be applied in Transition_frontier*)
        ( diff
        , next_staged_ledger_hash
        , ledger_proof_opt
        , is_new_stack
        , coinbase_amount ))
    in
    let%bind protocol_state, consensus_transition_data =
      lift_sync (fun () ->
          let previous_ledger_hash =
            previous_protocol_state |> Protocol_state.blockchain_state
            |> Blockchain_state.snarked_ledger_hash
          in
          let next_ledger_hash =
            Option.value_map ledger_proof_opt
              ~f:(fun (proof, _) ->
                Ledger_proof.statement proof |> Ledger_proof.statement_target
                )
              ~default:previous_ledger_hash
          in
          let supply_increase =
            Option.value_map ledger_proof_opt
              ~f:(fun (proof, _) ->
                (Ledger_proof.statement proof).supply_increase )
              ~default:Currency.Amount.zero
          in
          let blockchain_state =
            Blockchain_state.create_value ~timestamp:(Time.now time_controller)
              ~snarked_ledger_hash:next_ledger_hash
              ~staged_ledger_hash:next_staged_ledger_hash
          in
          let time =
            Time.now time_controller |> Time.to_span_since_epoch
            |> Time.Span.to_ms
          in
          measure "consensus generate_transition" (fun () ->
              Consensus_mechanism.generate_transition ~previous_protocol_state
                ~blockchain_state ~time ~proposal_data
                ~transactions:
                  ( Staged_ledger_diff.With_valid_signatures_and_proofs
                    .user_commands diff
                    :> User_command.t list )
                ~snarked_ledger_hash:previous_ledger_hash ~supply_increase
                ~logger ) )
    in
    lift_sync (fun () ->
        measure "making Snark and Internal transitions" (fun () ->
            let snark_transition =
              Snark_transition.create_value
                ?sok_digest:
                  (Option.map ledger_proof_opt ~f:(fun (proof, _) ->
                       Ledger_proof.sok_digest proof ))
                ?ledger_proof:
                  (Option.map ledger_proof_opt ~f:(fun (proof, _) ->
                       Ledger_proof.underlying_proof proof ))
                ~supply_increase:
                  (Option.value_map ~default:Currency.Amount.zero
                     ~f:(fun (proof, _) ->
                       (Ledger_proof.statement proof).supply_increase )
                     ledger_proof_opt)
                ~blockchain_state:
                  (Protocol_state.blockchain_state protocol_state)
                ~consensus_data:consensus_transition_data ~proposer:self
                ~coinbase:coinbase_amount ()
            in
            let internal_transition =
              Internal_transition.create ~snark_transition
                ~prover_state:(Proposal_data.prover_state proposal_data)
                ~staged_ledger_diff:(Staged_ledger_diff.forget diff)
            in
            let witness =
              { Pending_coinbase_witness.pending_coinbases=
                  Staged_ledger.pending_coinbase_collection staged_ledger
              ; is_new_stack }
            in
            Some (protocol_state, internal_transition, witness) ) )

  let run ~logger ~trust_system ~get_completed_work ~transaction_pool
      ~time_controller ~keypair ~consensus_local_state ~frontier_reader
      ~transition_writer ~random_peers ~query_peer =
    trace_task "proposer" (fun () ->
        let log_bootstrap_mode () =
          Logger.info logger ~module_:__MODULE__ ~location:__LOC__
            "Bootstrapping right now. Cannot generate new blockchains or \
             schedule event"
        in
        let module Breadcrumb = Transition_frontier.Breadcrumb in
        let propose ivar proposal_data =
          let open Interruptible.Let_syntax in
          match Broadcast_pipe.Reader.peek frontier_reader with
          | None ->
              log_bootstrap_mode () ; Interruptible.return ()
          | Some frontier -> (
              let crumb = Transition_frontier.best_tip frontier in
              Logger.trace logger ~module_:__MODULE__ ~location:__LOC__
                ~metadata:[("breadcrumb", Breadcrumb.to_yojson crumb)]
                !"Begining to propose off of crumb $breadcrumb%!" ;
              let previous_protocol_state, previous_protocol_state_proof =
                let transition : External_transition.Verified.t =
                  (Breadcrumb.transition_with_hash crumb).data
                in
                ( External_transition.Verified.protocol_state transition
                , External_transition.Verified.protocol_state_proof transition
                )
              in
              trace_event "waiting for ivar..." ;
              let%bind () =
                Interruptible.lift (Deferred.return ()) (Ivar.read ivar)
              in
              let%bind next_state_opt =
                generate_next_state ~proposal_data ~previous_protocol_state
                  ~time_controller
                  ~staged_ledger:(Breadcrumb.staged_ledger crumb)
                  ~transactions:
                    (Transaction_pool.transactions transaction_pool)
                  ~get_completed_work ~logger ~keypair
              in
              trace_event "next state generated" ;
              match next_state_opt with
              | None ->
                  Interruptible.return ()
              | Some
                  ( protocol_state
                  , internal_transition
                  , pending_coinbase_witness ) ->
                  Debug_assert.debug_assert (fun () ->
                      [%test_result: [`Take | `Keep]]
                        (Consensus_mechanism.select
                           ~existing:
                             (Protocol_state.consensus_state
                                previous_protocol_state)
                           ~candidate:
                             (Protocol_state.consensus_state protocol_state)
                           ~logger)
                        ~expect:`Take
                        ~message:
                          "newly generated consensus states should be \
                           selected over their parent" ;
                      let root_consensus_state =
                        Transition_frontier.root frontier
                        |> (fun x -> (Breadcrumb.transition_with_hash x).data)
                        |> External_transition.Verified.protocol_state
                        |> Protocol_state.consensus_state
                      in
                      [%test_result: [`Take | `Keep]]
                        (Consensus_mechanism.select
                           ~existing:root_consensus_state
                           ~candidate:
                             (Protocol_state.consensus_state protocol_state)
                           ~logger)
                        ~expect:`Take
                        ~message:
                          "newly generated consensus states should be \
                           selected over the tf root" ) ;
                  Interruptible.uninterruptible
                    (let open Deferred.Let_syntax in
                    let t0 = Time.now time_controller in
                    match%bind
                      measure "proving state transition valid" (fun () ->
                          Prover.prove ~prev_state:previous_protocol_state
                            ~prev_state_proof:previous_protocol_state_proof
                            ~next_state:protocol_state internal_transition
                            pending_coinbase_witness )
                    with
                    | Error err ->
                        Logger.error logger ~module_:__MODULE__
                          ~location:__LOC__
                          "failed to prove generated protocol state: %s"
                          (Error.to_string_hum err) ;
                        return ()
                    | Ok protocol_state_proof -> (
                        let span = Time.diff (Time.now time_controller) t0 in
                        Logger.info logger ~module_:__MODULE__
                          ~location:__LOC__
                          ~metadata:
                            [ ( "proving_time"
                              , `Int (Time.Span.to_ms span |> Int64.to_int_exn)
                              ) ]
                          !"Protocol_state_proof proving time took: \
                            $proving_time%!" ;
                        (* since we generated this transition, we do not need to verify it *)
                        let (`I_swear_this_is_safe_see_my_comment
                              external_transition) =
                          External_transition.to_verified
                            (External_transition.create ~protocol_state
                               ~protocol_state_proof
                               ~staged_ledger_diff:
                                 (Internal_transition.staged_ledger_diff
                                    internal_transition))
                        in
                        let external_transition_with_hash =
                          { With_hash.hash= Protocol_state.hash protocol_state
                          ; data= external_transition }
                        in
                        let metadata =
                          [ ( "state_hash"
                            , Protocol_state_hash.to_yojson
                                external_transition_with_hash.hash ) ]
                        in
                        Logger.info logger ~module_:__MODULE__
                          ~location:__LOC__
                          !"Submitting transition to the transition frontier \
                            controller"
                          ~metadata ;
                        let%bind () =
                          Strict_pipe.Writer.write transition_writer
                            external_transition_with_hash
                        in
                        Logger.info logger ~module_:__MODULE__
                          ~location:__LOC__
                          "Waiting for transition to be inserted into frontier" ;
                        Deferred.choose
                          [ Deferred.choice
                              (Transition_frontier.wait_for_transition frontier
                                 (With_hash.hash external_transition_with_hash))
                              (Fn.const `Transition_accepted)
                          ; Deferred.choice
                              ( Time.Timeout.create time_controller
                                  (* We allow up to 15 seconds for the transition to make its way from the transition_writer to the frontier.
                                     This value is chosen to be reasonably generous. In theory, this should not take terribly long. But long
                                     cycles do happen in our system, and with medium curves those long cycles can be substantial. *)
                                  (Time.Span.of_ms 15000L)
                                  ~f:(Fn.const ())
                              |> Time.Timeout.to_deferred )
                              (Fn.const `Timed_out) ]
                        >>| function
                        | `Transition_accepted ->
                            Logger.info logger ~module_:__MODULE__
                              ~location:__LOC__ ~metadata
                              "Generated transition was accepted into \
                               transition frontier"
                        | `Timed_out ->
                            let str =
                              "Generated transition was never accepted into \
                               transition frontier"
                            in
                            Logger.fatal logger ~module_:__MODULE__
                              ~location:__LOC__ ~metadata "%s" str ;
                            Error.raise (Error.of_string str) )) )
        in
        let proposal_supervisor = Singleton_supervisor.create ~task:propose in
        let scheduler = Singleton_scheduler.create time_controller in
        let rec check_for_proposal () =
          trace_recurring_task "check for proposal" (fun () ->
              match Broadcast_pipe.Reader.peek frontier_reader with
              | None ->
                  log_bootstrap_mode () ;
                  don't_wait_for
                    (let%map () =
                       Broadcast_pipe.Reader.iter_until frontier_reader
                         ~f:(Fn.compose Deferred.return Option.is_some)
                     in
                     check_for_proposal ())
              | Some transition_frontier -> (
                  let breadcrumb =
                    Transition_frontier.best_tip transition_frontier
                  in
                  let transition =
                    (Breadcrumb.transition_with_hash breadcrumb).data
                  in
                  let protocol_state =
                    External_transition.Verified.protocol_state transition
                  in
                  let consensus_state =
                    Protocol_state.consensus_state protocol_state
                  in
                  match
                    Consensus_mechanism.required_local_state_sync
                      ~consensus_state ~local_state:consensus_local_state
                  with
                  | Some sync_jobs ->
                      Logger.info logger ~module_:__MODULE__ ~location:__LOC__
                        "Synchronizing consensus local state" ;
                      don't_wait_for
                        (let%map res =
                           Consensus_mechanism.sync_local_state
                             ~local_state:consensus_local_state ~logger
                             ~trust_system ~random_peers ~query_peer sync_jobs
                         in
                         ( match res with
                         | Ok () ->
                             ()
                         | Error e ->
                             Logger.error logger ~module_:__MODULE__
                               ~location:__LOC__
                               "error syncing local state: %s"
                               (Error.to_string_hum e) ) ;
                         check_for_proposal ())
                  | None -> (
                    match
                      measure "asking conensus what to do" (fun () ->
                          Consensus_mechanism.next_proposal
                            (time_to_ms (Time.now time_controller))
                            consensus_state ~local_state:consensus_local_state
                            ~keypair ~logger )
                    with
                    | `Check_again time ->
                        Singleton_scheduler.schedule scheduler
                          (time_of_ms time) ~f:check_for_proposal
                    | `Propose_now data ->
                        Interruptible.finally
                          (Singleton_supervisor.dispatch proposal_supervisor
                             data)
                          ~f:check_for_proposal
                        |> ignore
                    | `Propose (time, data) ->
                        Singleton_scheduler.schedule scheduler
                          (time_of_ms time) ~f:(fun () ->
                            ignore
                              (Interruptible.finally
                                 (Singleton_supervisor.dispatch
                                    proposal_supervisor data)
                                 ~f:check_for_proposal) ) ) ) )
        in
        check_for_proposal () )
end
