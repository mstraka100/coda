open Core_kernel
open Async
open Tuple_lib
open Fold_lib
open Coda_numbers

module type Prover_state_intf = sig
  type t [@@deriving sexp]

  module Stable :
    sig
      module V1 : sig
        type t [@@deriving bin_io, sexp, version]
      end

      module Latest : module type of V1
    end
    with type V1.t = t

  type pending_coinbase_witness

  val precomputed_handler : Snark_params.Tick.Handler.t

  val handler :
       t
    -> pending_coinbase:pending_coinbase_witness
    -> Snark_params.Tick.Handler.t
end

(** Constants are defined with a single letter (latin or greek) based on
 * their usage in the Ouroboros suite of papers *)
module type Shared_constants = sig
  (** k is the number of blocks required to reach finality *)
  val k : int

  (** The amount of money minted and given to the proposer whenever a block
   * is created *)
  val coinbase : Currency.Amount.t

  (** The window of time available to create a block *)
  val block_window_duration_ms : Int64.t
end

module type S = sig
  val name : string

  module Local_state : sig
    type t [@@deriving sexp, to_yojson]

    val create : Signature_lib.Public_key.Compressed.t option -> t
  end

  module Consensus_transition_data : sig
    module Value : sig
      type t [@@deriving sexp]

      module Stable :
        sig
          module V1 : sig
            type t [@@deriving sexp, bin_io, version]
          end
        end
        with type V1.t = t
    end

    include Snark_params.Tick.Snarkable.S with type value := Value.t

    val genesis : Value.t
  end

  module Consensus_state : sig
    module Value : sig
      (* bin_io omitted *)
      type t [@@deriving hash, eq, compare, sexp, to_yojson]

      module Stable :
        sig
          module V1 : sig
            type t
            [@@deriving hash, eq, compare, bin_io, sexp, to_yojson, version]
          end
        end
        with type V1.t = t
    end

    type display [@@deriving yojson]

    include Snark_params.Tick.Snarkable.S with type value := Value.t

    val genesis : Value.t

    val length_in_triples : int

    val var_to_triples :
         var
      -> ( Snark_params.Tick.Boolean.var Triple.t list
         , _ )
         Snark_params.Tick.Checked.t

    val fold : Value.t -> bool Triple.t Fold.t

    val length : Value.t -> Length.t

    val time_hum : Value.t -> string

    val to_lite : (Value.t -> Lite_base.Consensus_state.t) option

    val display : Value.t -> display
  end

  module Rpcs : sig
    val implementations :
         logger:Logger.t
      -> local_state:Local_state.t
      -> Host_and_port.t Rpc.Implementation.t list
  end

  module Blockchain_state : Coda_base.Blockchain_state.S

  module Prover_state :
    Prover_state_intf
    with type pending_coinbase_witness := Coda_base.Pending_coinbase_witness.t

  module Protocol_state :
    Coda_base.Protocol_state.S
    with module Blockchain_state = Blockchain_state
     and module Consensus_state = Consensus_state

  module Snark_transition :
    Coda_base.Snark_transition.S
    with module Blockchain_state = Blockchain_state
     and module Consensus_data = Consensus_transition_data

  module Proposal_data : sig
    type t

    val prover_state : t -> Prover_state.t
  end

  module For_tests : sig
    val gen_consensus_state :
         gen_slot_advancement:int Quickcheck.Generator.t
      -> (   previous_protocol_state:( Protocol_state.Value.t
                                     , Coda_base.State_hash.t )
                                     With_hash.t
          -> snarked_ledger_hash:Coda_base.Frozen_ledger_hash.t
          -> Consensus_state.Value.t)
         Quickcheck.Generator.t

    val create_genesis_protocol_state :
         Coda_base.Ledger.t
      -> (Protocol_state.Value.t, Coda_base.State_hash.t) With_hash.t
  end

  module Configuration : sig
    type t [@@deriving yojson, bin_io]

    val t : t
  end

  val genesis_protocol_state :
    (Protocol_state.Value.t, Coda_base.State_hash.t) With_hash.t

  (**
   * Generate a new protocol state and consensus specific transition data
   * for a new transition. Called from the proposer in order to generate
   * a new transition to propose to the network. Returns `None` if a new
   * transition cannot be generated.
   *)
  val generate_transition :
       previous_protocol_state:Protocol_state.Value.t
    -> blockchain_state:Blockchain_state.Value.t
    -> time:Unix_timestamp.t
    -> proposal_data:Proposal_data.t
    -> transactions:Coda_base.User_command.t list
    -> snarked_ledger_hash:Coda_base.Frozen_ledger_hash.t
    -> supply_increase:Currency.Amount.t
    -> logger:Logger.t
    -> Protocol_state.Value.t * Consensus_transition_data.Value.t

  (**
   * Check that a consensus state was received at a valid time.
  *)
  val received_at_valid_time :
       Consensus_state.Value.t
    -> time_received:Unix_timestamp.t
    -> (unit, (string * Yojson.Safe.json) list) result

  (**
   * Create a constrained, checked var for the next consensus state of
   * a given consensus state and snark transition.
  *)
  val next_state_checked :
       prev_state:Protocol_state.var
    -> prev_state_hash:Coda_base.State_hash.var
    -> Snark_transition.var
    -> Currency.Amount.var
    -> ( [`Success of Snark_params.Tick.Boolean.var] * Consensus_state.var
       , _ )
       Snark_params.Tick.Checked.t

  (**
   * Select between two ledger builder controller tips given the consensus
   * states for the two tips. Returns `\`Keep` if the first tip should be
   * kept, or `\`Take` if the second tip should be taken instead.
  *)
  val select :
       existing:Consensus_state.Value.t
    -> candidate:Consensus_state.Value.t
    -> logger:Logger.t
    -> [`Keep | `Take]

  (**
   * Determine if and when to perform the next transition proposal. Either
   * informs the callee to check again at some time in the future, or to
   * schedule a proposal at some time in the future, or to propose now
   * and check again some time in the future.
  *)
  val next_proposal :
       Unix_timestamp.t
    -> Consensus_state.Value.t
    -> local_state:Local_state.t
    -> keypair:Signature_lib.Keypair.t
    -> logger:Logger.t
    -> [ `Check_again of Unix_timestamp.t
       | `Propose_now of Proposal_data.t
       | `Propose of Unix_timestamp.t * Proposal_data.t ]

  (**
   * A hook for managing local state when the locked tip is updated.
  *)
  val lock_transition :
       Consensus_state.Value.t
    -> Consensus_state.Value.t
    -> local_state:Local_state.t
    -> snarked_ledger:Coda_base.Ledger.Any_ledger.witness
    -> unit

  (**
     * Indicator of when we should bootstrap
    *)
  val should_bootstrap :
       existing:Consensus_state.Value.t
    -> candidate:Consensus_state.Value.t
    -> bool

  (** Data needed to synchronize the local state. *)
  type local_state_sync [@@deriving to_yojson]

  (**
    * Predicate indicating whether or not the local state requires synchronization.
    *)
  val required_local_state_sync :
       consensus_state:Consensus_state.Value.t
    -> local_state:Local_state.t
    -> local_state_sync Non_empty_list.t option

  (**
    * Synchronize local state over the network.
    *)
  val sync_local_state :
       logger:Logger.t
    -> trust_system:Trust_system.t
    -> local_state:Local_state.t
    -> random_peers:(int -> Network_peer.Peer.t list)
    -> query_peer:Network_peer.query_peer
    -> local_state_sync Non_empty_list.t
    -> unit Deferred.Or_error.t

  (** Return a string that tells a human what the consensus view of an instant in time is.
    *
    * This is mostly useful for PoStake and other consensus mechanisms that have their own
    * notions of time.
    *)
  val time_hum : Time.t -> string
end
