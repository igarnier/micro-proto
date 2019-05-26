type t

val of_string : string -> t
val to_string : t -> string
val (=) : t -> t -> bool

module Map : Map.S with type key = t
