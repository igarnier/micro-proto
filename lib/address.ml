type t = string

let of_string x = x
let to_string x = x
let from_nonce (nonce : int) =
  Printf.sprintf "internal_%d" nonce
let (=) = (=)

module Map = String_map
