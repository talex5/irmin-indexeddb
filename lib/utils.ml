(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

let tail s i =
  String.sub s i (String.length s - i)

let option_map f = function
  | None -> None
  | Some x -> Some (f x)
