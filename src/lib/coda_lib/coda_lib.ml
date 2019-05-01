open Core_kernel
open Async_kernel
open Async_rpc_kernel
open Protocols
open Pipe_lib
open Strict_pipe
open O1trace

module type Network_intf = sig
  type t

  type consensus_state

  type state_with_witness

  type staged_ledger

  type protocol_state

  type ledger_hash

  type pending_coinbases

  type staged_ledger_hash

  type parallel_scan_state

  type sync_ledger_query

  type sync_ledger_answer

  type snark_pool_diff

  type transaction_pool_diff

  type time

  type state_hash

  type state_body_hash

  val states :
    t -> (state_with_witness Envelope.Incoming.t * time) Strict_pipe.Reader.t

  val peers : t -> Network_peer.Peer.t list

  val online_status : t -> [`Online | `Offline] Broadcast_pipe.Reader.t

  val random_peers : t -> int -> Network_peer.Peer.t list

  val catchup_transition :
       t
    -> Network_peer.Peer.t
    -> state_hash
    -> state_with_witness Non_empty_list.t option Or_error.t Deferred.t

  val snark_pool_diffs :
    t -> snark_pool_diff Envelope.Incoming.t Linear_pipe.Reader.t

  val transaction_pool_diffs :
    t -> transaction_pool_diff Envelope.Incoming.t Linear_pipe.Reader.t

  val broadcast_state : t -> state_with_witness -> unit

  val broadcast_snark_pool_diff : t -> snark_pool_diff -> unit

  val broadcast_transaction_pool_diff : t -> transaction_pool_diff -> unit

  val glue_sync_ledger :
       t
    -> (ledger_hash * sync_ledger_query) Linear_pipe.Reader.t
    -> ( ledger_hash
       * sync_ledger_query
       * sync_ledger_answer Envelope.Incoming.t )
       Linear_pipe.Writer.t
    -> unit

  val query_peer :
       t
    -> Network_peer.Peer.t
    -> (Versioned_rpc.Connection_with_menu.t -> 'q -> 'r Deferred.Or_error.t)
    -> 'q
    -> 'r Deferred.Or_error.t

  module Config : sig
    type t
  end

  val create :
       Config.t
    -> get_staged_ledger_aux_and_pending_coinbases_at_hash:(   state_hash
                                                               Envelope
                                                               .Incoming
                                                               .t
                                                            -> ( parallel_scan_state
                                                               * ledger_hash
                                                               * pending_coinbases
                                                               )
                                                               Deferred.Option
                                                               .t)
    -> answer_sync_ledger_query:(   (ledger_hash * sync_ledger_query)
                                    Envelope.Incoming.t
                                 -> sync_ledger_answer Deferred.Or_error.t)
    -> transition_catchup:(   state_hash Envelope.Incoming.t
                           -> state_with_witness Non_empty_list.t
                              Deferred.Option.t)
    -> get_ancestry:(   consensus_state Envelope.Incoming.t
                     -> ( state_with_witness
                        , state_body_hash list * state_with_witness )
                        Proof_carrying_data.t
                        Deferred.Option.t)
    -> t Deferred.t
end

module type Transaction_pool_read_intf = sig
  type t

  type transaction_with_valid_signature

  val transactions : t -> transaction_with_valid_signature Sequence.t
end

module type Transaction_pool_intf = sig
  include Transaction_pool_read_intf

  type pool_diff

  type transaction

  type transition_frontier

  val broadcasts : t -> pool_diff Linear_pipe.Reader.t

  val load :
       logger:Logger.t
    -> disk_location:string
    -> incoming_diffs:pool_diff Envelope.Incoming.t Linear_pipe.Reader.t
    -> frontier_broadcast_pipe:transition_frontier Option.t
                               Broadcast_pipe.Reader.t
    -> t Deferred.t

  val add : t -> transaction -> unit Deferred.t
end

module type Snark_pool_intf = sig
  type t

  type completed_work_statement

  type completed_work_checked

  type pool_diff

  type transition_frontier

  val broadcasts : t -> pool_diff Linear_pipe.Reader.t

  val load :
       logger:Logger.t
    -> disk_location:string
    -> incoming_diffs:pool_diff Envelope.Incoming.t Linear_pipe.Reader.t
    -> frontier_broadcast_pipe:transition_frontier Option.t
                               Broadcast_pipe.Reader.t
    -> t Deferred.t

  val get_completed_work :
    t -> completed_work_statement -> completed_work_checked option
end

module type Proposer_intf = sig
  type state_hash

  type ledger_hash

  type staged_ledger

  type transaction

  type external_transition

  type external_transition_verified

  type completed_work_statement

  type completed_work_checked

  type protocol_state

  type protocol_state_proof

  type consensus_local_state

  type time_controller

  type keypair

  type transition_frontier

  type transaction_pool

  type time

  val run :
       logger:Logger.t
    -> trust_system:Trust_system.t
    -> get_completed_work:(   completed_work_statement
                           -> completed_work_checked option)
    -> transaction_pool:transaction_pool
    -> time_controller:time_controller
    -> keypair:keypair
    -> consensus_local_state:consensus_local_state
    -> frontier_reader:transition_frontier option Broadcast_pipe.Reader.t
    -> transition_writer:( ( external_transition_verified
                           , state_hash )
                           With_hash.t
                         , synchronous
                         , unit Deferred.t )
                         Strict_pipe.Writer.t
    -> random_peers:(int -> Network_peer.Peer.t list)
    -> query_peer:Network_peer.query_peer
    -> unit
end

module type Witness_change_intf = sig
  type t_with_witness

  type witness

  type t

  val forget_witness : t_with_witness -> t

  val add_witness_exn : t -> witness -> t_with_witness

  val add_witness : t -> witness -> t_with_witness Or_error.t
end

module type State_with_witness_intf = sig
  type state

  type ledger_hash

  type staged_ledger_transition

  type staged_ledger_transition_with_valid_signatures_and_proofs

  type t =
    { staged_ledger_transition:
        staged_ledger_transition_with_valid_signatures_and_proofs
    ; state: state }
  [@@deriving sexp]

  module Stripped : sig
    type t = {staged_ledger_transition: staged_ledger_transition; state: state}
  end

  val strip : t -> Stripped.t

  val forget_witness : t -> state
end

module type Inputs_intf = sig
  include Coda_pow.Inputs_intf

  module Masked_ledger : sig
    type t
  end

  module Ledger_db : Coda_pow.Ledger_creatable_intf

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
     and type masked_ledger := Masked_ledger.t
     and type staged_ledger := Staged_ledger.t
     and type staged_ledger_diff := Staged_ledger_diff.t
     and type transaction_snark_scan_state := Staged_ledger.Scan_state.t
     and type consensus_local_state := Consensus_mechanism.Local_state.t
     and type user_command := User_command.t
     and type diff_mutant :=
                ( External_transition.Stable.Latest.t
                , Coda_base.State_hash.Stable.Latest.t )
                With_hash.t
                Diff_mutant.E.t

  module Transition_frontier_persistence :
    Transition_frontier_persistence.Intf.S
    with type frontier := Transition_frontier.t
     and type 'output diff := 'output Diff_mutant.E.t
     and type diff_hash := Diff_hash.t
     and type root_snarked_ledger := Ledger_db.t
     and type consensus_local_state := Consensus_mechanism.Local_state.t

  module Transaction_pool :
    Transaction_pool_intf
    with type transaction_with_valid_signature :=
                User_command.With_valid_signature.t
     and type transaction := User_command.t
     and type transition_frontier := Transition_frontier.t

  module Snark_pool :
    Snark_pool_intf
    with type completed_work_statement := Transaction_snark_work.Statement.t
     and type completed_work_checked := Transaction_snark_work.Checked.t
     and type transition_frontier := Transition_frontier.t

  module Work_selector :
    Coda_pow.Work_selector_intf
    with type staged_ledger := Staged_ledger.t
     and type work :=
                ( Ledger_proof_statement.t
                , Transaction.t
                , Transaction_witness.t
                , Ledger_proof.t )
                Snark_work_lib.Work.Single.Spec.t
     and type snark_pool := Snark_pool.t
     and type fee := Currency.Fee.t

  module State_body_hash : sig
    type t
  end

  module Net :
    Network_intf
    with type state_with_witness := External_transition.t
     and type staged_ledger := Staged_ledger.t
     and type staged_ledger_hash := Staged_ledger_hash.t
     and type protocol_state := Consensus_mechanism.Protocol_state.Value.t
     and type snark_pool_diff := Snark_pool.pool_diff
     and type transaction_pool_diff := Transaction_pool.pool_diff
     and type parallel_scan_state := Staged_ledger.Scan_state.t
     and type ledger_hash := Ledger_hash.t
     and type sync_ledger_query := Coda_base.Sync_ledger.Query.t
     and type sync_ledger_answer := Coda_base.Sync_ledger.Answer.t
     and type time := Time.t
     and type state_hash := Coda_base.State_hash.t
     and type state_body_hash := State_body_hash.t
     and type consensus_state := Consensus_mechanism.Consensus_state.Value.t
     and type pending_coinbases := Pending_coinbase.t

  module Transition_router :
    Protocols.Coda_transition_frontier.Transition_router_intf
    with type time_controller := Time.Controller.t
     and type external_transition := External_transition.t
     and type external_transition_verified := External_transition.Verified.t
     and type transition_frontier := Transition_frontier.t
     and type state_hash := Protocol_state_hash.t
     and type time := Time.t
     and type network := Net.t
     and type ledger_db := Ledger_db.t

  module Root_prover :
    Protocols.Coda_transition_frontier.Root_prover_intf
    with type state_body_hash := State_body_hash.t
     and type transition_frontier := Transition_frontier.t
     and type external_transition := External_transition.t
     and type proof_verified_external_transition :=
                External_transition.Proof_verified.t
     and type consensus_state := Consensus_mechanism.Consensus_state.Value.t
     and type state_hash := Coda_base.State_hash.t

  module Proposer :
    Proposer_intf
    with type state_hash := Protocol_state_hash.t
     and type ledger_hash := Ledger_hash.t
     and type staged_ledger := Staged_ledger.t
     and type transaction := User_command.With_valid_signature.t
     and type protocol_state := Consensus_mechanism.Protocol_state.Value.t
     and type protocol_state_proof := Protocol_state_proof.t
     and type consensus_local_state := Consensus_mechanism.Local_state.t
     and type completed_work_statement := Transaction_snark_work.Statement.t
     and type completed_work_checked := Transaction_snark_work.Checked.t
     and type external_transition := External_transition.t
     and type external_transition_verified := External_transition.Verified.t
     and type time_controller := Time.Controller.t
     and type keypair := Keypair.t
     and type transition_frontier := Transition_frontier.t
     and type transaction_pool := Transaction_pool.t
     and type time := Time.t

  module Ledger_transfer :
    Coda_pow.Ledger_transfer_intf
    with type src := Ledger.t
     and type dest := Ledger_db.t

  module Genesis : sig
    val state :
      ( Consensus_mechanism.Protocol_state.Value.t
      , Protocol_state_hash.t )
      With_hash.t

    val ledger : Ledger.maskable_ledger

    val proof : Protocol_state_proof.t
  end

  module Sync_handler :
    Protocols.Coda_transition_frontier.Sync_handler_intf
    with type ledger_hash := Ledger_hash.t
     and type state_hash := Coda_base.State_hash.t
     and type external_transition := External_transition.t
     and type transition_frontier := Transition_frontier.t
     and type syncable_ledger_query := Coda_base.Sync_ledger.Query.t
     and type syncable_ledger_answer := Coda_base.Sync_ledger.Answer.t
     and type pending_coinbases := Pending_coinbase.t
     and type parallel_scan_state := Staged_ledger.Scan_state.t
end

module Make (Inputs : Inputs_intf) = struct
  open Inputs

  type t =
    { propose_keypair: Keypair.t option
    ; snark_worker_key: Public_key.Compressed.Stable.V1.t option
    ; net: Net.t
    ; wallets: Secrets.Wallets.t
    ; transaction_pool: Transaction_pool.t
    ; snark_pool: Snark_pool.t
    ; transition_frontier: Transition_frontier.t option Broadcast_pipe.Reader.t
    ; verified_transitions:
        (External_transition.Verified.t, Protocol_state_hash.t) With_hash.t
        Strict_pipe.Reader.t
    ; proposer_transition_writer:
        ( (External_transition.Verified.t, Protocol_state_hash.t) With_hash.t
        , synchronous
        , unit Deferred.t )
        Writer.t
    ; logger: Logger.t
    ; trust_system: Trust_system.t
    ; mutable seen_jobs: Work_selector.State.t
    ; receipt_chain_database: Coda_base.Receipt_chain_database.t
    ; staged_ledger_transition_backup_capacity: int
    ; external_transitions_writer:
        (External_transition.t Envelope.Incoming.t * Inputs.Time.t)
        Pipe.Writer.t
    ; time_controller: Time.Controller.t
    ; snark_work_fee: Currency.Fee.t
    ; consensus_local_state: Consensus_mechanism.Local_state.t
    ; transaction_database: Transaction_database.t }

  let peek_frontier frontier_broadcast_pipe =
    Broadcast_pipe.Reader.peek frontier_broadcast_pipe
    |> Result.of_option
         ~error:
           (Error.of_string
              "Cannot retrieve transition frontier now. Bootstrapping right \
               now.")

  let wallets t = t.wallets

  let snark_worker_key t = t.snark_worker_key

  let propose_keypair t = t.propose_keypair

  let best_tip_opt t =
    let open Option.Let_syntax in
    let%map frontier = Broadcast_pipe.Reader.peek t.transition_frontier in
    Transition_frontier.best_tip frontier

  let transition_frontier t = t.transition_frontier

  let root_length_opt t =
    let open Option.Let_syntax in
    let%map frontier = Broadcast_pipe.Reader.peek t.transition_frontier in
    Transition_frontier.root_length frontier

  let best_staged_ledger_opt t =
    let open Option.Let_syntax in
    let%map tip = best_tip_opt t in
    Transition_frontier.Breadcrumb.staged_ledger tip

  let best_protocol_state_opt t =
    let open Option.Let_syntax in
    let%map tip = best_tip_opt t in
    Transition_frontier.Breadcrumb.transition_with_hash tip
    |> With_hash.data |> External_transition.Verified.protocol_state

  let best_ledger_opt t =
    let open Option.Let_syntax in
    let%map staged_ledger = best_staged_ledger_opt t in
    Staged_ledger.ledger staged_ledger

  let compose_of_option f =
    Fn.compose
      (Option.value_map ~default:`Bootstrapping ~f:(fun x -> `Active x))
      f

  let best_tip = compose_of_option best_tip_opt

  let root_length = compose_of_option root_length_opt

  module Incr = struct
    open Coda_incremental.Status

    let online_status t = of_broadcast_pipe @@ Net.online_status t.net

    let transition_frontier t = of_broadcast_pipe @@ t.transition_frontier
  end

  let sync_status t =
    let open Coda_incremental.Status in
    let transition_frontier_incr = Var.watch @@ Incr.transition_frontier t in
    let incremental_status =
      map2
        (Var.watch @@ Incr.online_status t)
        transition_frontier_incr
        ~f:(fun online_status active_status ->
          match online_status with
          | `Offline ->
              `Offline
          | `Online ->
              Option.value_map active_status ~default:`Bootstrap
                ~f:(Fn.const `Synced) )
    in
    let observer = observe incremental_status in
    stabilize () ; observer

  let visualize_frontier ~filename =
    compose_of_option
    @@ fun t ->
    let open Option.Let_syntax in
    let%map frontier = Broadcast_pipe.Reader.peek t.transition_frontier in
    Transition_frontier.visualize ~filename frontier

  let best_staged_ledger = compose_of_option best_staged_ledger_opt

  let best_protocol_state = compose_of_option best_protocol_state_opt

  let best_ledger = compose_of_option best_ledger_opt

  let get_ledger t staged_ledger_hash =
    let open Deferred.Or_error.Let_syntax in
    let%bind frontier =
      Deferred.return (t.transition_frontier |> peek_frontier)
    in
    match
      List.find_map (Transition_frontier.all_breadcrumbs frontier) ~f:(fun b ->
          let staged_ledger = Transition_frontier.Breadcrumb.staged_ledger b in
          if
            Staged_ledger_hash.equal
              (Staged_ledger.hash staged_ledger)
              staged_ledger_hash
          then Some (Ledger.to_list (Staged_ledger.ledger staged_ledger))
          else None )
    with
    | Some x ->
        Deferred.return (Ok x)
    | None ->
        Deferred.Or_error.error_string
          "staged ledger hash not found in transition frontier"

  let seen_jobs t = t.seen_jobs

  let set_seen_jobs t seen_jobs = t.seen_jobs <- seen_jobs

  let transaction_pool t = t.transaction_pool

  let transaction_database t = t.transaction_database

  let snark_pool t = t.snark_pool

  let peers t = Net.peers t.net

  let snark_work_fee t = t.snark_work_fee

  let receipt_chain_database t = t.receipt_chain_database

  let staged_ledger_ledger_proof t =
    let open Option.Let_syntax in
    let%bind sl = best_staged_ledger_opt t in
    Staged_ledger.current_ledger_proof sl

  let verified_transitions t = t.verified_transitions

  let root_diff t =
    let root_diff_reader, root_diff_writer =
      Strict_pipe.create ~name:"root diff"
        (Buffered (`Capacity 30, `Overflow Crash))
    in
    don't_wait_for
      (Broadcast_pipe.Reader.iter t.transition_frontier ~f:(function
        | None ->
            Deferred.unit
        | Some frontier ->
            Broadcast_pipe.Reader.iter
              (Transition_frontier.root_diff_pipe frontier)
              ~f:(fun root_diff ->
                Strict_pipe.Writer.write root_diff_writer root_diff
                |> Deferred.return ) )) ;
    root_diff_reader

  let dump_tf t =
    peek_frontier t.transition_frontier
    |> Or_error.map ~f:Transition_frontier.visualize_to_string

  (** The [best_path coda] is the list of state hashes from the root to the best_tip in the transition frontier. It includes the root hash and the hash *)
  let best_path t =
    let open Option.Let_syntax in
    let%map tf = Broadcast_pipe.Reader.peek t.transition_frontier in
    let bt = Transition_frontier.best_tip tf in
    List.cons
      Transition_frontier.(root tf |> Breadcrumb.state_hash)
      (Transition_frontier.hash_path tf bt)

  module Config = struct
    (** If ledger_db_location is None, will auto-generate a db based on a UUID *)
    type t =
      { logger: Logger.t
      ; trust_system: Trust_system.t
      ; propose_keypair: Keypair.t option
      ; snark_worker_key: Public_key.Compressed.Stable.V1.t option
      ; net_config: Net.Config.t
      ; transaction_pool_disk_location: string
      ; snark_pool_disk_location: string
      ; wallets_disk_location: string
      ; ledger_db_location: string option
      ; transition_frontier_location: string option
      ; staged_ledger_transition_backup_capacity: int [@default 10]
      ; time_controller: Time.Controller.t
      ; receipt_chain_database: Coda_base.Receipt_chain_database.t
      ; snark_work_fee: Currency.Fee.t
      ; monitor: Monitor.t option
      ; consensus_local_state: Consensus_mechanism.Local_state.t
            (* TODO: Pass banlist to modules discussed in Ban Reasons issue: https://github.com/CodaProtocol/coda/issues/852 *)
      }
    [@@deriving make]
  end

  let start t =
    Option.iter t.propose_keypair ~f:(fun keypair ->
        Proposer.run ~logger:t.logger ~trust_system:t.trust_system
          ~transaction_pool:t.transaction_pool
          ~get_completed_work:(Snark_pool.get_completed_work t.snark_pool)
          ~time_controller:t.time_controller ~keypair
          ~consensus_local_state:t.consensus_local_state
          ~frontier_reader:t.transition_frontier
          ~transition_writer:t.proposer_transition_writer
          ~random_peers:(Net.random_peers t.net)
          ~query_peer:
            {Network_peer.query= (fun a b c -> Net.query_peer t.net a b c)} )

  let create_genesis_frontier (config : Config.t) =
    let consensus_local_state = config.consensus_local_state in
    let pending_coinbases = Pending_coinbase.create () |> Or_error.ok_exn in
    let empty_diff =
      { Staged_ledger_diff.diff=
          ( { completed_works= []
            ; user_commands= []
            ; coinbase= Staged_ledger_diff.At_most_two.Zero }
          , None )
      ; prev_hash=
          Staged_ledger_hash.of_aux_ledger_and_coinbase_hash
            (Staged_ledger_aux_hash.of_bytes "")
            (Ledger.merkle_root Genesis_ledger.t)
            pending_coinbases
      ; creator= Account.public_key (snd (List.hd_exn Genesis_ledger.accounts))
      }
    in
    let genesis_protocol_state =
      With_hash.data Consensus_mechanism.genesis_protocol_state
    in
    (* the genesis transition is assumed to be valid *)
    let (`I_swear_this_is_safe_see_my_comment first_transition) =
      External_transition.to_verified
        (External_transition.create ~protocol_state:genesis_protocol_state
           ~protocol_state_proof:Genesis.proof ~staged_ledger_diff:empty_diff)
    in
    let ledger_db =
      Ledger_db.create ?directory_name:config.ledger_db_location ()
    in
    let root_snarked_ledger =
      Ledger_transfer.transfer_accounts ~src:Genesis.ledger ~dest:ledger_db
    in
    let snarked_ledger_hash =
      Frozen_ledger_hash.of_ledger_hash @@ Ledger.merkle_root Genesis.ledger
    in
    let%bind root_staged_ledger =
      match%map
        Staged_ledger.of_scan_state_and_ledger ~snarked_ledger_hash
          ~ledger:Genesis.ledger
          ~scan_state:(Staged_ledger.Scan_state.empty ())
          ~pending_coinbase_collection:pending_coinbases
      with
      | Ok staged_ledger ->
          staged_ledger
      | Error err ->
          Error.raise err
    in
    let%map frontier =
      Transition_frontier.create ~logger:config.logger
        ~root_transition:
          (With_hash.of_data first_transition
             ~hash_data:
               (Fn.compose Consensus_mechanism.Protocol_state.hash
                  External_transition.Verified.protocol_state))
        ~root_staged_ledger ~root_snarked_ledger ~consensus_local_state
    in
    (root_snarked_ledger, frontier)

  let create (config : Config.t) =
    let monitor = Option.value ~default:(Monitor.create ()) config.monitor in
    Async.Scheduler.within' ~monitor (fun () ->
        trace_task "coda" (fun () ->
            let external_transitions_reader, external_transitions_writer =
              Strict_pipe.create Synchronous
            in
            let proposer_transition_reader, proposer_transition_writer =
              Strict_pipe.create Synchronous
            in
            let net_ivar = Ivar.create () in
            let%bind persistence, ledger_db, transition_frontier =
              match config.transition_frontier_location with
              | None ->
                  let%map ledger_db, frontier =
                    create_genesis_frontier config
                  in
                  ( None
                  , Ledger_transfer.transfer_accounts ~src:Genesis.ledger
                      ~dest:ledger_db
                  , frontier )
              | Some transition_frontier_location -> (
                  match%bind
                    Async.Sys.file_exists transition_frontier_location
                  with
                  | `No | `Unknown ->
                      Logger.info config.logger ~module_:__MODULE__
                        ~location:__LOC__
                        !"Persistence database does not exist yet. Creating \
                          it at %s"
                        transition_frontier_location ;
                      let%bind () =
                        Async.Unix.mkdir transition_frontier_location
                      in
                      let persistence =
                        Transition_frontier_persistence.create
                          ~directory_name:transition_frontier_location
                          ~logger:config.logger ()
                      in
                      let%map root_snarked_ledger, frontier =
                        create_genesis_frontier config
                      in
                      (Some persistence, root_snarked_ledger, frontier)
                  | `Yes ->
                      let directory_name = transition_frontier_location in
                      let root_snarked_ledger =
                        Ledger_db.create
                          ?directory_name:config.ledger_db_location ()
                      in
                      let%map frontier =
                        Transition_frontier_persistence.deserialize
                          ~directory_name ~logger:config.logger
                          ~root_snarked_ledger
                          ~consensus_local_state:config.consensus_local_state
                      in
                      let persistence =
                        Transition_frontier_persistence.create ~directory_name
                          ~logger:config.logger ()
                      in
                      (Some persistence, root_snarked_ledger, frontier) )
            in
            (* TODO: the name of transaction_database should be supplied by Config #2333 *)
            let transaction_database = Transaction_database.create () in
            let frontier_broadcast_pipe_r, frontier_broadcast_pipe_w =
              Broadcast_pipe.create (Some transition_frontier)
            in
            Option.iter persistence ~f:(fun persistence ->
                Transition_frontier_persistence
                .listen_to_frontier_broadcast_pipe ~logger:config.logger
                  frontier_broadcast_pipe_r persistence
                |> don't_wait_for ) ;
            let%bind net =
              Net.create config.net_config
                ~get_staged_ledger_aux_and_pending_coinbases_at_hash:
                  (fun enveloped_hash ->
                  let hash = Envelope.Incoming.data enveloped_hash in
                  Deferred.return
                  @@
                  let open Option.Let_syntax in
                  let%bind frontier =
                    Broadcast_pipe.Reader.peek frontier_broadcast_pipe_r
                  in
                  Sync_handler
                  .get_staged_ledger_aux_and_pending_coinbases_at_hash
                    ~frontier hash )
                ~answer_sync_ledger_query:(fun query_env ->
                  let open Deferred.Or_error.Let_syntax in
                  let ledger_hash, _ = Envelope.Incoming.data query_env in
                  let%bind frontier =
                    Deferred.return @@ peek_frontier frontier_broadcast_pipe_r
                  in
                  Sync_handler.answer_query ~frontier ledger_hash
                    (Envelope.Incoming.map ~f:Tuple2.get2 query_env)
                    ~logger:config.logger ~trust_system:config.trust_system
                  |> Deferred.map
                       ~f:
                         (Result.of_option
                            ~error:
                              (Error.createf
                                 !"Refused to answer query for ledger_hash: \
                                   %{sexp:Ledger_hash.t}"
                                 ledger_hash)) )
                ~transition_catchup:(fun enveloped_hash ->
                  let open Deferred.Option.Let_syntax in
                  let hash = Envelope.Incoming.data enveloped_hash in
                  let%bind frontier =
                    Deferred.return
                    @@ Broadcast_pipe.Reader.peek frontier_broadcast_pipe_r
                  in
                  Deferred.return
                  @@ Sync_handler.transition_catchup ~frontier hash )
                ~get_ancestry:(fun query_env ->
                  let consensus_state = Envelope.Incoming.data query_env in
                  Deferred.return
                  @@
                  let open Option.Let_syntax in
                  let%bind frontier =
                    Broadcast_pipe.Reader.peek frontier_broadcast_pipe_r
                  in
                  Root_prover.prove ~logger:config.logger ~frontier
                    consensus_state )
            in
            let valid_transitions =
              Transition_router.run ~logger:config.logger
                ~trust_system:config.trust_system ~network:net
                ~time_controller:config.time_controller
                ~frontier_broadcast_pipe:
                  (frontier_broadcast_pipe_r, frontier_broadcast_pipe_w)
                ~ledger_db
                ~network_transition_reader:
                  (Strict_pipe.Reader.map external_transitions_reader
                     ~f:(fun (tn, tm) -> (`Transition tn, `Time_received tm)))
                ~proposer_transition_reader
            in
            let valid_transitions_for_network, valid_transitions_for_api =
              Strict_pipe.Reader.Fork.two valid_transitions
            in
            let%bind transaction_pool =
              Transaction_pool.load ~logger:config.logger
                ~disk_location:config.transaction_pool_disk_location
                ~incoming_diffs:(Net.transaction_pool_diffs net)
                ~frontier_broadcast_pipe:frontier_broadcast_pipe_r
            in
            don't_wait_for
              (Linear_pipe.iter (Transaction_pool.broadcasts transaction_pool)
                 ~f:(fun x ->
                   Net.broadcast_transaction_pool_diff net x ;
                   Deferred.unit )) ;
            Ivar.fill net_ivar net ;
            don't_wait_for
              (Strict_pipe.Reader.iter_without_pushback
                 valid_transitions_for_network ~f:(fun transition_with_hash ->
                   (* remove verified status for network broadcast *)
                   Net.broadcast_state net
                     (External_transition.of_verified
                        (With_hash.data transition_with_hash)) )) ;
            don't_wait_for
              (Strict_pipe.transfer (Net.states net)
                 external_transitions_writer ~f:ident) ;
            let%bind snark_pool =
              Snark_pool.load ~logger:config.logger
                ~disk_location:config.snark_pool_disk_location
                ~incoming_diffs:(Net.snark_pool_diffs net)
                ~frontier_broadcast_pipe:frontier_broadcast_pipe_r
            in
            let%bind wallets =
              Secrets.Wallets.load ~logger:config.logger
                ~disk_location:config.wallets_disk_location
            in
            don't_wait_for
              (Linear_pipe.iter (Snark_pool.broadcasts snark_pool) ~f:(fun x ->
                   Net.broadcast_snark_pool_diff net x ;
                   Deferred.unit )) ;
            return
              { propose_keypair= config.propose_keypair
              ; snark_worker_key= config.snark_worker_key
              ; net
              ; wallets
              ; transaction_pool
              ; snark_pool
              ; transition_frontier= frontier_broadcast_pipe_r
              ; time_controller= config.time_controller
              ; external_transitions_writer=
                  Strict_pipe.Writer.to_linear_pipe external_transitions_writer
              ; verified_transitions= valid_transitions_for_api
              ; logger= config.logger
              ; trust_system= config.trust_system
              ; seen_jobs= Work_selector.State.init
              ; staged_ledger_transition_backup_capacity=
                  config.staged_ledger_transition_backup_capacity
              ; receipt_chain_database= config.receipt_chain_database
              ; snark_work_fee= config.snark_work_fee
              ; proposer_transition_writer
              ; consensus_local_state= config.consensus_local_state
              ; transaction_database } ) )
end
