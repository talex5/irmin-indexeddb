(* Copyright (C) 2020, Thomas Leonard
   See the README file for details. *)

(** An Irmin backend that stores data in an IndexedDB.
    Use [Irmin_git.Generic] to create a Git-format store, or [Irmin.Make] for an Irmin-format one. *)

val config : string -> Irmin.config
(** [config db_name] is a configuration that stores all values in the given IndexedDB database. *)

module Content_store : Irmin.CONTENT_ADDRESSABLE_STORE_MAKER
(** The content-addressable store stores blobs, trees and commits. *)

module Branch_store : Irmin.ATOMIC_WRITE_STORE_MAKER
(** The branch store records the head commit hash for each branch. *)

exception Format_too_old of [`Irmin_0_10]
(** Raised on creation if the existing data format cannot be read by this version of irmin-indexeddb.
    To migrate Irmin 0.10 format data, upgrade to irmin-indexeddb version 0.6 first.
    Note: to be able to read old databases you must use a Git-format store. *)
