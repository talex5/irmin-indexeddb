(* If the data is valid UTF-8 then store it directly (prefixed with '"').
   Otherwise, encode with Base64 (and prefix with "%"). *)
val encode : string -> string
val decode : string -> string
