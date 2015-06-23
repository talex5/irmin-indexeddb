(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

let tail s i =
  String.sub s i (String.length s - i)

let (>|?=) x f =
  match x with
  | None -> None
  | Some x -> Some (f x)

module UTF8_codec : sig
  (* If the data is valid UTF-8 then store it directly (prefixed with '"').
   * Otherwise, encode with Base64 (and prefix with "%"). *)
  val encode : string -> string
  val decode : string -> string
end = struct
  (* From https://github.com/mirage/ezjsonm.
   * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org> *)
  let is_valid_utf8 str =
    try
      Uutf.String.fold_utf_8 (fun () _ -> function
        | `Malformed _ -> raise Exit
        | _ -> ()
      ) () str;
      true
    with Exit -> false

  let encode s =
    if is_valid_utf8 s then "\"" ^ s
    else "%" ^ B64.encode s

  let decode s =
    match s.[0] with
    | '%' -> B64.decode (tail s 1)
    | '"' -> tail s 1
    | _ -> B64.decode s            (* Old format, base64 *)
end
