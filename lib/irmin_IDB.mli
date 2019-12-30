(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** An Irmin backend that stores data in an IndexedDB. *)

(* [config dn_name] store all values in the given IndexedDB database. *)
val config : string -> Irmin.config

module AO: Irmin.AO_MAKER
module RW (K: Irmin.Hum.S) (V: Irmin.Hash.S): sig
  include Irmin.RRW with type key = K.t and type value = V.t
  val create: Irmin.config -> t Lwt.t
end
module Make: Irmin.S_MAKER
