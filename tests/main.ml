open State

(* A small scenario involving three contracts, among which two accounts and
   a custodial contract. The "dump_contract_state" are no-ops.  *)

let chain =
  let open Chain in
  let account0, account0_addr =
    Account.make (), Address.of_string "account0" in
  let account1, account1_addr =
    Account.make (), Address.of_string "account1" in
  let custodial, custodial_addr =
    Custodial.make (), Address.of_string "custodial" in
  let bouncer, bouncer_addr =
    Bouncer.make account0_addr, Address.of_string "bouncer" in
  run begin
    let* () = originate account0_addr (account0, Mutez.of_int_exn 100) in
    let* () = originate account1_addr (account1, Mutez.of_int_exn 105) in
    let* () = originate custodial_addr (custodial, Mutez.of_int_exn 0) in
    let* () = originate bouncer_addr (bouncer, Mutez.of_int_exn 0) in

    let* () = dump_contract_state account0_addr in
    let* () = dump_contract_state account1_addr in
    let* () = dump_contract_state custodial_addr in
    let* () = apply_operation
        Value.(Transaction { sender = account0_addr ;
                             target = custodial_addr ;
                             entrypoint = "create_account" ;
                             arg = Value.Unit;
                             amount = Mutez.of_int_exn 10 }) in
    let* () = apply_operation
        Value.(Transaction { sender = account1_addr ;
                             target = custodial_addr ;
                             entrypoint = "create_account" ;
                             arg = Value.Unit;
                             amount = Mutez.of_int_exn 1 }) in

    let* () = dump_contract_state account0_addr in
    let* () = dump_contract_state account1_addr in
    let* () = dump_contract_state custodial_addr in
    let* () = apply_operation
        Value.(Transaction { sender = account0_addr ;
                             target = custodial_addr ;
                             entrypoint = "transfer" ;
                             arg = Value.(Tuple [ Mutez (Mutez.of_int_exn 5) ; Address account1_addr ; ]);
                             amount = Mutez.of_int_exn 0 }) in
    let* () = dump_contract_state custodial_addr in
    let* () = apply_operation
        Value.(Transaction { sender = account1_addr ;
                             target = custodial_addr ;
                             entrypoint = "withdraw" ;
                             arg = Value.(Mutez (Mutez.of_int_exn 6));
                             amount = Mutez.of_int_exn 0 }) in
    let* () = apply_queue () in

    let* () = apply_operation
        Value.(Transaction { sender = account0_addr ;
                             target = bouncer_addr ;
                             entrypoint = "default" ;
                             arg = Value.(Op (Transaction
                                                { sender = account0_addr ;
                                                  target = custodial_addr ;
                                                  entrypoint = "credit" ;
                                                  arg = Value.Unit ;
                                                  amount = Mutez.of_int_exn 33 }
                                             )) ;
                             amount = Mutez.of_int_exn 33 }) in
    let* () = apply_queue () in
    let* () = dump_contract_state account0_addr in
    let* () = dump_contract_state bouncer_addr in
    dump_contract_state custodial_addr
    (* >@> apply_operation
     *   Value.(Transaction { sender = account0_addr ;
     *                        target = bouncer_addr ;
     *                        entrypoint = default }) *)
  end
