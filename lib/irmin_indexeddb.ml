(* Copyright (C) 2020, Thomas Leonard.
   See the README file for details. *)

module Content_store_non_git = Content_store.Make
module Branch_store = Branch_store.Make
module Raw = Raw

module Content_store_git (K : Irmin.Hash.S) (V : Irmin.Type.S) =
  Content_store_non_git (K) (struct type t = V.t let t = Irmin.Type.boxed V.t end)

let config = Config.v

exception Format_too_old = Config.Format_too_old
