open Error_monad

(* Un contrat "ERC20"-like *)
module Make(_ : sig end) : Contract.S =
struct
  type storage = Mutez.t Address.Map.t
  (* The contract stores a mapping from addresses to mutez. *)

  let state : storage Contract.state =
    { storage = Address.Map.empty ;
      balance = Mutez.zero }

  let credit (storage : storage) (account : Address.t) (amount : Mutez.t) =
    if Mutez.(amount = zero) then
      error "Custodial.credit: cannot credit null amount"
    else
      match Address.Map.find account storage with
      | value ->
        return @@ Address.Map.add account Mutez.(value + amount) storage
      | exception Not_found ->
        return @@ Address.Map.add account amount storage

  let debit (storage : storage) (account : Address.t) (amount : Mutez.t) =
    if Mutez.(amount = zero) then
      error "Custodial.debit: cannot debit null amount"
    else
      match Address.Map.find account storage with
      | value ->
        Mutez.(value - amount) >>= fun new_value ->
        return @@ Address.Map.add account new_value storage
      | exception Not_found ->
        error @@
        "Custodial.debit: account "^(Address.to_string account)^" not found"

  let create_account sender storage amount =
    if Address.Map.mem sender storage then
      error "Custodial.create_account: account already exists"
    else
      credit storage sender amount

  let internal_transfer sender storage arg =
    let open Value in
    begin match arg with
      | Tuple [ Mutez internal_amount ; Address dest ] ->
        if Address.Map.mem dest storage then
          debit storage sender internal_amount >>= fun storage ->
          credit storage dest internal_amount
        else
          error "Custodial.internal_transfer: destination does not exist"
      | _ ->
        error "Custodial.internal_transfer: type error"
    end

  let internal_debit self sender storage arg =
    let open Operation in
    let open Value in
    match arg with
    | Mutez amount ->
      let value = Address.Map.find sender storage in
      Mutez.(value - amount) >>= fun remaining ->
      let storage = Address.Map.add sender remaining storage in
      let op =  Transaction
          { sender = self ;
            target = sender ;
            entrypoint = "default" ;
            arg = Value.Unit ;
            amount } in
      return ([ op ], storage)
    | _ ->
      error "Custodial.internal_debit: type error"

  let dispatch ~self ~entrypoint ~sender (storage : storage) (arg : Value.t) (amount : Mutez.t) =
    match entrypoint with
    | "create_account" ->
      create_account sender storage amount >>= fun storage ->
      return ([], storage)
    | "credit" ->
      if Address.Map.mem sender storage then
        credit storage sender amount >>= fun storage ->
        return ([], storage)
      else
        error "Custodial.credit: unregistered sender"
    | "debit" ->
      if Address.Map.mem sender storage then
        if Mutez.(amount = zero) then
          internal_debit self sender storage arg
        else
          error "Custodial.transfer: nonzero amount"
      else
        error "Custodial.debit: unregistered sender"
    | "transfer" ->
      if Address.Map.mem sender storage then
        if Mutez.(amount = zero) then
          internal_transfer sender storage arg >>= fun storage ->
          return ([], storage)
        else
          error "Custodial.transfer: nonzero amount"
      else
        error "Custodial.transfer: unregistered sender"
    | _ ->
      error "Custodial.dispatch: entrypoint does not exist"

  let show (storage : storage) =
    Address.Map.fold (fun addr mutez string ->
        Printf.sprintf "%s%s -> %s\n"
          string
          (Address.to_string addr)
          (Mutez.to_string mutez)
      ) storage ""
end

(* Interface via module de 1ère classe *)
let make () =
  (module Make(struct end) : Contract.S)
