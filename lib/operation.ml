type t =
  | Transaction of { sender : Address.t ;
                     target : Address.t ;
                     entrypoint : string ;
                     arg : Value.t ;
                     amount : Mutez.t }
