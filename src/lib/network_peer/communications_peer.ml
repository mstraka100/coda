open Core

module T = struct
  type t =
    { (* the host is an IPv4 or IPv6 address *)
      host: Unix.Inet_addr.Blocking_sexp.t (* TCP *)
    ; port: int }
  [@@deriving bin_io, sexp, compare, hash]
end

include T
include Hashable.Make (T)
include Comparable.Make_binable (T)

let create host port = {host; port}

let of_host_and_port host_and_port =
  let host =
    Host_and_port.host host_and_port |> Unix.Inet_addr.of_string_or_getbyname
  in
  let port = Host_and_port.port host_and_port in
  {host; port}

let to_host_and_port t =
  Host_and_port.create ~host:(Unix.Inet_addr.to_string t.host) ~port:t.port

let of_peer peer = create peer.Peer.host peer.communication_port

let to_string t = sexp_of_t t |> Sexp.to_string

let of_string s = Sexp.of_string s |> t_of_sexp
