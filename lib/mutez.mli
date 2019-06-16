open Error_monad

type t

val of_int : int -> t result
val of_int_exn : int -> t
val to_string : t -> string
val zero : t
val (+) : t -> t -> t
val (-) : t -> t -> t result
val (=) : t -> t -> bool
val (<) : t -> t -> bool
val (<=) : t -> t -> bool
