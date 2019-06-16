type t

val of_string : string -> t
val to_string : t -> string
val from_nonce : int -> t
val (=) : t -> t -> bool

module Map : Map.S with type key = t
