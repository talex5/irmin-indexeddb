(* Copyright (C) 2019, Thomas Leonard
 * See the README file for details. *)

(** An Irmin backend that stores data in an IndexedDB. *)

(* [config db_name] store all values in the given IndexedDB database. *)
val config : string -> Irmin.config

exception Format_too_old of [`Irmin_0_10]
(** Raised on [Repo.create] if the existing data format cannot be read by this version of irmin-indexeddb.
    To migrate Irmin 0.10 format data, upgrade to irmin-indexeddb version 0.6 first. *)

module Make (C: Irmin.Contents.S) (R: Irmin.Ref.S) (H: Irmin.Hash.S) : sig
  include Irmin.S
    with type key = C.Path.t
     and module Key = C.Path
     and type value = C.t
     and type branch_id = R.t
     and type commit_id = H.t
end
