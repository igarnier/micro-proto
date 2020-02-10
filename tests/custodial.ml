open State

(* Un contrat "ERC20"-like *)
module Make(_ : sig end) : Contract.S =
struct
  type storage = Mutez.t Address.Map.t
  (* The contract stores a mapping from addresses to mutez. *)

  let state = Address.Map.empty

  let credit (state : storage) (account : Address.t) (amount : Mutez.t) =
    if Mutez.(amount = zero) then
      user_error "Custodial.credit: cannot credit null amount"
    else
      match Address.Map.find account state with
      | value ->
        return @@ Address.Map.add account Mutez.(value + amount) state
      | exception Not_found ->
        return @@ Address.Map.add account amount state

  let debit (state : storage) (account : Address.t) (amount : Mutez.t) =
    if Mutez.(amount = zero) then
      user_error "Custodial.debit: cannot debit null amount"
    else
      match Address.Map.find account state with
      | value ->
        (match Mutez.(value - amount) with
         | Ok new_value ->
           return @@ Address.Map.add account new_value state
         | Error err ->
           user_error err)
      | exception Not_found ->
        user_error @@
        "Custodial.debit: account "^(Address.to_string account)^" not found"

  let create_account sender state amount =
    if Address.Map.mem sender state then
      user_error "Custodial.create_account: account already exists"
    else
      credit state sender amount

  let internal_transfer sender state arg =
    let open Value in
    begin match arg with
      | Tuple [ Mutez internal_amount ; Address dest ] ->
        if Address.Map.mem dest state then
          let* state = debit state sender internal_amount in
          credit state dest internal_amount
        else
          user_error "Custodial.internal_transfer: destination does not exist"
      | _ ->
        user_error "Custodial.internal_transfer: type error"
    end

  let internal_withdraw self sender state arg =
    let open Value in
    match arg with
    | Mutez amount ->
      let value = Address.Map.find sender state in
      let* remaining =
        match Mutez.(value - amount) with
        | Ok remaining -> return remaining
        | Error err -> user_error err in
      let state = Address.Map.add sender remaining state in
      let op = Transaction
          { sender = self ;
            target = sender ;
            entrypoint = "default" ;
            arg = Value.Unit ;
            amount } in
      return ([ op ], state)
    | _ ->
      user_error "Custodial.internal_debit: type error"

  let dispatch ~env ~entrypoint ~sender ~state ~arg ~amount =
    match entrypoint with
    | "create_account" ->
      let* storage = create_account sender state amount in
      return ([], storage)
    | "credit" ->
      if Address.Map.mem sender state then
        let* storage = credit state sender amount in
        return ([], storage)
      else
        user_error "Custodial.credit: unregistered sender"
    | "withdraw" ->
      if Address.Map.mem sender state then
        if Mutez.(amount = zero) then
          internal_withdraw env.Contract.self sender state arg
        else
          user_error "Custodial.transfer: nonzero amount"
      else
        user_error "Custodial.debit: unregistered sender"
    | "transfer" ->
      if Address.Map.mem sender state then
        if Mutez.(amount = zero) then
          let* storage = internal_transfer sender state arg in
          return ([], storage)
        else
          user_error "Custodial.transfer: nonzero amount"
      else
        user_error "Custodial.transfer: unregistered sender"
    | _ ->
      user_error "Custodial.dispatch: entrypoint does not exist"

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
