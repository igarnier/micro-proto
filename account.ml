open Error_monad

(* Un contrat de compte en banque standard *)
module Make(_ : sig end) : Contract.S =
struct
  type storage = unit

  let state = ()

  let dispatch
      ~(env : Contract.runtime_environment)
      ~(entrypoint : string)
      ~(sender : Address.t)
      ~(state : storage)
      ~(arg : Value.t)
      ~(amount : Mutez.t) =
    ignore env ;
    ignore sender ;
    ignore state ;
    ignore arg ;
    ignore amount ;
    match entrypoint with
    | "default" ->
      State.return ([], state)
    | _ ->
      State.user_error "Account.dispatch: entrypoint does not exist"

  let show _ = "()"
end

(* Interface via module de 1ère classe *)
let make () =
  (module Make(struct end) : Contract.S)
