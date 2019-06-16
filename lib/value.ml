type tag = int

type t =
  | Unit
  | Int of int
  | Bool of bool
  | String of string
  | Tuple of t list
  | Cons of tag * t
  | Mutez of Mutez.t
  | Address of Address.t
  | Op of operation

and operation =
  | Transaction of { sender : Address.t ;
                     target : Address.t ;
                     entrypoint : string ;
                     arg : t ;
                     amount : Mutez.t }
