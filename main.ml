open Error_monad

(* A small scenario involving three contracts, among which two accounts and
   a custodial contract. The "dump_contract_state" are no-ops.  *)

let chain =
  let open Chain in
  let account0, account0_addr =
    Account.make 100, Address.of_string "account0" in
  let account1, account1_addr =
    Account.make 105, Address.of_string "account1" in
  let custodial, custodial_addr =
    Custodial.make (), Address.of_string "custodial" in
  run begin
    return genesis
    >@> originate account0_addr account0
    >@> originate account1_addr account1
    >@> originate custodial_addr custodial
    >@> dump_contract_state account0_addr
    >@> dump_contract_state account1_addr
    >@> dump_contract_state custodial_addr
    >@> apply_operation
      Operation.(Transaction { sender = account0_addr ;
                               target = custodial_addr ;
                               entrypoint = "create_account" ;
                               arg = Value.Unit;
                               amount = Mutez.of_int_exn 10 })
    >@> apply_operation
      Operation.(Transaction { sender = account1_addr ;
                               target = custodial_addr ;
                               entrypoint = "create_account" ;
                               arg = Value.Unit;
                               amount = Mutez.of_int_exn 1 })

    >@> dump_contract_state account0_addr
    >@> dump_contract_state account1_addr
    >@> dump_contract_state custodial_addr
    >@> apply_operation
      Operation.(Transaction { sender = account0_addr ;
                               target = custodial_addr ;
                               entrypoint = "transfer" ;
                               arg = Value.(Tuple [ Mutez (Mutez.of_int_exn 5) ; Address account1_addr ; ]);
                               amount = Mutez.of_int_exn 0 })
    >@> dump_contract_state custodial_addr
    >@> apply_operation
      Operation.(Transaction { sender = account1_addr ;
                               target = custodial_addr ;
                               entrypoint = "debit" ;
                               arg = Value.(Mutez (Mutez.of_int_exn 6));
                               amount = Mutez.of_int_exn 0 })
    >@> apply_queue (* apply operation emitted by Custodial contract *)
    >@> dump_contract_state custodial_addr
  end
