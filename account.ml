open Error_monad

(* Un contrat de compte en banque standard *)
module Make(P : sig val initial : Mutez.t end) : Contract.S =
struct
  type storage = unit

  let state : storage Contract.state =
    { storage = () ; balance = P.initial }

  let dispatch ~self ~entrypoint ~sender (storage : storage) (arg : Value.t) (amount : Mutez.t) =
    ignore self ;
    ignore sender ;
    ignore storage ;
    ignore arg ;
    ignore amount ;
    match entrypoint with
    | "default" ->
      return ([], storage)
    | _ ->
      error "Account.dispatch: entrypoint does not exist"

  let show _ = "()"
end

(* Interface via module de 1ère classe *)
let make (initial : int) =
  run begin
    Mutez.of_int initial >>= fun initial ->
    return (module Make(struct let initial = initial end) : Contract.S)
  end
