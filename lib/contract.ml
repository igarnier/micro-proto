open Error_monad

type 's state =
  { storage : 's ;
    balance : Mutez.t }

module type S =
sig
  type storage

  val state : storage state
  (** Current internal state of the contract. *)

  val dispatch : self:Address.t -> entrypoint:string -> sender:Address.t -> storage -> Value.t -> Mutez.t -> (Operation.t list * storage) result
  (** Entrypoint of the contract, with built-in dispatch. *)

  val show : storage -> string
  (** Printing internal storage. This is strictly for convenience. *)
end

type t = (module S)
