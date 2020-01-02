(* Copyright (C) 2019, Thomas Leonard
 * See the README file for details. *)

(** An Irmin backend that stores data in an IndexedDB. *)

(* [config db_name] store all values in the given IndexedDB database. *)
val config : string -> Irmin.config

module AO (K : Irmin.Hash.S) : Irmin_git.VALUE_STORE
(** An append-only store that stores [Git.Value.t] items using hash [K]. *)

exception Format_too_old of [`Irmin_0_10]
(** Raised on [Repo.create] if the existing data format cannot be read by this version of irmin-indexeddb.
    To migrate Irmin 0.10 format data, upgrade to irmin-indexeddb version 0.6 first. *)

module Make (C: Irmin.Contents.S) (P: Irmin.Path.S) (B: Irmin.Branch.S) :
  Irmin.S with type key = P.t
           and type step = P.step
           and type metadata = Irmin_git.Metadata.t
           and type contents = C.t
           and type branch = B.t
           and type Commit.Hash.t = Irmin.Hash.SHA1.t
           and type Tree.Hash.t = Irmin.Hash.SHA1.t
           and type Contents.Hash.t = Irmin.Hash.SHA1.t
