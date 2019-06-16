module type Param =
sig
  val owner : Address.t
end

module Make(P : Param) : Contract.S =
struct

  type storage = { owner : Address.t ; sent : bool  }

  let state = { owner = P.owner ; sent = false }

  let dispatch ~env ~entrypoint ~sender ~state ~arg ~amount =
    let open Value in
    let open State in
    match entrypoint with
    | "default" ->
      if state.sent then
        user_error "Bouncer.dispatch: bouncer already used"
      else
        (match arg with
         | Op tx ->
           let is_valid =
             match tx with
             | Transaction { sender ; amount = req_amount ; _ } ->
               Address.(sender = state.owner) &&
               Mutez.(req_amount <= amount)
             | _ -> false in
           if is_valid then
             return ([ tx ], { state with sent = true })
           else
             user_error "Bouncer.dispatch: not enough funds"
         | _ ->
           user_error "Bouncer.dispatch: invalid argument")
    | "oracle" ->
      let answer =
        Transaction { sender = env.Contract.self ;
                      target = sender ;
                      entrypoint = "co_oracle" ;
                      arg = Bool state.sent ;
                      amount = Mutez.zero } in
      return ([ answer ], state)
    | _ ->
      user_error "Bouncer.dispatch: entrypoint does not exist"

  let show { owner ; sent } =
    Printf.sprintf "Bouncer (%s, %s)"
      (Address.to_string owner)
      (string_of_bool sent)
end

let make addr =
  let module R = Make(struct let owner = addr end) in
  (module R : Contract.S)
