type tag = int

type t =
  | Unit
  | Int of int
  | String of string
  | Tuple of t list
  | Cons of tag * t
  | Mutez of Mutez.t
  | Address of Address.t
