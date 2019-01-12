open Core
open Async
open Banlist_lib.Banlist
open Unsigned
open Network_peer

module Punishment_record = struct
  type time = Time.t

  include Punishment.Record.Make (struct
    let duration = Time.Span.of_day 1.0
  end)
end

module Punished_db =
  Punished_db.Make (Discovery_peer) (Time) (Punishment_record)
    (Key_value_database.Make_mock (Discovery_peer) (Punishment_record))

let ban_threshold = 100

module Score_mechanism = struct
  open Offense

  let score offense =
    Score.of_int
      ( match offense with
      | Failed_to_connect -> ban_threshold + 1
      | Send_bad_hash -> ban_threshold / 2
      | Send_bad_aux -> ban_threshold / 4 )
end

module Suspicious_db = Key_value_database.Make_mock (Discovery_peer) (Score)

module Banlist = struct
  include Make (Discovery_peer) (Punishment_record) (Suspicious_db)
            (Punished_db)
            (Score_mechanism)

  let create = create ~ban_threshold
end

include Banlist
