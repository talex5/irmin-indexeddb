(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** An Irmin backend that stores data in an IndexedDB. *)

(* [config db_name] store all values in the given IndexedDB database. *)
val config : string -> Irmin.config

module Make (C: Irmin.Contents.S) (R: Irmin.Ref.S) (H: Irmin.Hash.S) : sig
  include Irmin.S
    with type key = C.Path.t
     and module Key = C.Path
     and type value = C.t
     and type branch_id = R.t
     and type commit_id = H.t

  val create_full: log:(string -> unit) -> Irmin.config -> Repo.t Lwt.t
  (** [create_full ~log config] is like [Repo.create] but if a database
      migration is required, it writes progress messages to [log] rather than
      to the console. *)
end
