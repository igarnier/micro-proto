module Error_monad = Error_monad
module Chain = Chain
module Value = Value

module Address = Address
module Mutez = Mutez

module Contract = State.Contract

module State :
sig
  type state = State.State.state

  type 'a t = 'a State.State.t

  val user_error : string -> 'a t

  val run : unit t -> state
  val return : 'a -> 'a t

  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  val (let+) : 'a t -> ('a -> 'b) -> 'b t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t

  (** Address & contract handling *)
  val fresh_address : Address.t t
end = State.State
