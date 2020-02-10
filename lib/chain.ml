open State

(* [originate addr ct chain] originates a contract [ct] with 0 balance
   at address [addr] on [chain] and returns the updated chain state. *)
let originate : Address.t -> Contract.t * Mutez.t -> unit State.t =
  let open State in
  fun addr (contract, bal) ->
    let* present = contract_mem addr in
    if present then
      proto_error @@
      "originate: contract with address "^(Address.to_string addr)^" already originated"
    else
      contract_add addr (contract, bal)

(* [apply_operation op chain] applies operation [op] on the current state of the [chain]
   and returns the updated chain state. *)
let apply_operation : Value.operation -> unit State.t =
  let open State in
  fun op ->
    let s = Value.show_operation op in
    Printf.printf "%s\n" s ;
    match op with
    | Transaction { sender ; target ; entrypoint ; arg ; amount } ->
      let* (target_ct, target_balance) = contract_find target in
      let* (sender_ct, sender_balance) = contract_find sender in
      (* T = target *)
      let module T = (val target_ct) in
      (* S = sender *)
      let module S = (val sender_ct) in
      let* now = State.current_time in
      let env = { Contract.self = target ;
                  balance = target_balance ;
                  now } in
      let* (ops, ct_storage) =
        T.dispatch
          ~env
          ~entrypoint
          ~sender
          ~state:T.state
          ~arg
          ~amount in
      let  t_balance_after = Mutez.(target_balance + amount) in
      let* s_balance_after =
        match Mutez.(sender_balance - amount) with
        | Error s -> proto_error s
        | Ok x    -> return x in
      let module T_after = struct include T let state = ct_storage end in
      let* () = contract_set_code target (module T_after) in
      let* () = contract_set_balance target t_balance_after in
      let* () = contract_set_balance sender s_balance_after in
      append_operations ops
    | Origination { originator ; address ; code ; amount } ->
      let* () = contract_set_code address code in
      let* originator_balance = contract_get_balance originator in
      let* originator_balance_after =
        match Mutez.(originator_balance - amount) with
        | Error s -> proto_error s
        | Ok x    -> return x in
      let* () = contract_set_balance originator originator_balance_after in
      contract_set_balance address amount


(* Apply all the operations in the queue until it becomes empty. *)
let rec apply_queue : unit -> unit State.t =
  let open State in
  fun () ->
    let* next_op = take_operation in
    match next_op with
    | None -> return ()
    | Some op ->
      let* () = apply_operation op in
      apply_queue ()

let apply_step : unit State.t =
  let open State in
  let* next_op = take_operation in
  match next_op with
  | None -> return ()
  | Some op ->
    apply_operation op

(* Convenience *)

let dump_contract_state : Address.t -> unit State.t =
  let open State in
  fun addr ->
    let* contract, bal = contract_find addr in
    let module C = (val contract : Contract.S) in
    let internal_state = C.show C.state in
    let balance = Mutez.to_string bal in
    Printf.printf
      "contract %s\nbalance = %s\n%s\n"
      (Address.to_string addr)
      balance
      internal_state ;
    return ()
