open Error_monad

(* The storage ("context" in Tezos parlance) is a map from addresses to contracts. *)
type storage = Contract.t Address.Map.t

(* The current state of the "chain" is the current storage and the current internal
   operation queue. *)
type chain =
  { state : storage ;
    operation_queue : Operation.t list }

(* Empty chain. *)
let genesis =
  { state = Address.Map.empty ;
    operation_queue = [] }

(* In a real chain, the contract address would be computed from some shared
   pseudorandom seed using a public mechanism. For convenience, we let you
   decide contract addresses. It is the user's responsibility to ensure that
   names are globally unique. *)
(* [originate addr ct chain ] originates a contract [ct] at address [addr] on [chain]
   and returns the updated chain state. *)
let originate : Address.t -> Contract.t -> chain -> chain result =
  fun addr contract chain ->
    if Address.Map.mem addr chain.state then
      error @@
      "originate: contract with address "^(Address.to_string addr)^" already originated"
    else
      return { chain with
               state = Address.Map.add addr contract chain.state }

(* [apply_operation op chain] applies operation [op] on the current state of the [chain]
   and returns the updated chain state. *)
let apply_operation : Operation.t -> chain -> chain result =
  fun op chain ->
    match op with
    | Operation.Transaction { sender ; target ; entrypoint ; arg ; amount } ->
      (match Address.Map.find target chain.state with
       | target_ct ->
         (match Address.Map.find sender chain.state with
          | sender_ct ->
            (* T = target *)
            let module T = (val target_ct) in
            (* S = sender *)
            let module S = (val sender_ct) in
            T.dispatch ~self:target ~entrypoint ~sender T.state.storage arg amount >>= fun (ops, ct_storage) ->
            let t_balance_after = Mutez.(T.state.balance + amount) in
            Mutez.(S.state.balance - amount) >>= fun s_balance_after ->
            let module T_after =
            struct
              include T
              let state = { Contract.storage = ct_storage ;
                            balance = t_balance_after }
            end in
            let module S_after =
            struct
              include S
              let state = { S.state with
                            balance = s_balance_after }
            end in
            let chain_state = Address.Map.add sender (module S_after : Contract.S) chain.state in
            let chain_state = Address.Map.add target (module T_after : Contract.S) chain_state in
            return { state = chain_state ;
                     operation_queue = chain.operation_queue @ ops }
          | exception Not_found ->
            error @@
            "apply_operation: contract "^(Address.to_string sender)^" not found")
       | exception Not_found ->
         error @@
         "apply_operation: contract "^(Address.to_string target)^" not found")

(* Apply all the operations in the queue until it becomes empty. *)
let rec apply_queue : chain -> chain result =
  fun chain ->
    match chain.operation_queue with
    | [] -> return chain
    | op :: tail ->
      let chain = { chain with operation_queue = tail } in
      apply_operation op chain >>= fun chain ->
      apply_queue chain

(* Convenience *)

let dump_contract_state (addr : Address.t) (chain : chain) : chain result =
  match Address.Map.find addr chain.state with
  | contract ->
    let module C = (val contract : Contract.S) in
    let internal_state = C.show C.state.storage in
    let amount = Mutez.to_string C.state.balance in
    Printf.printf
      "contract %s\nbalance = %s\n%s\n"
      (Address.to_string addr)
      amount
      internal_state ;
    return chain
  | exception Not_found ->
    Printf.printf
      "dump_contract_state:\ncontract %s not found\n"
      (Address.to_string addr) ;
    return chain

(* Pipelining operator. *)

let (>@>) : chain result -> (chain -> chain result) -> chain result =
  fun chain next ->
    chain >>= fun c ->
    next c
