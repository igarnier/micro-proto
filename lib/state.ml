(* Ugly code duplication in signature because of recursive modules.
   No way around that. *)
module rec Contract :
sig
  (** Environment provided by the protocol at dispatch time *)
  type runtime_environment = {
    self  : Address.t ;
    (** Address of contract itself *)
    balance : Mutez.t ;
    (** Balance of contract at dispatch time *)
  }

  module type S =
  sig
    type storage

    val state : storage
    (** Current internal state of the contract. *)

    val dispatch :
      env:runtime_environment ->
      entrypoint:string ->
      sender:Address.t ->
      state:storage ->
      arg:Value.t ->
      amount:Mutez.t -> (Value.operation list * storage) State.t
    (** Entrypoint of the contract, with built-in dispatch. *)

    val show : storage -> string
    (** Printing internal storage. This is strictly for convenience. *)
  end

  type t = (module S)
end =
struct
  type runtime_environment = {
    self  : Address.t ;
    balance : Mutez.t
  }

  module type S =
  sig
    type storage
    val state : storage
    val dispatch :
      env:runtime_environment ->
      entrypoint:string ->
      sender:Address.t ->
      state:storage ->
      arg:Value.t ->
      amount:Mutez.t -> (Value.operation list * storage) State.t

    val show : storage -> string
  end

  type t = (module S)
end

and Value :
sig
  type tag = int

  type t =
    | Unit
    | Int of int
    | Bool of bool
    | String of string
    | Tuple of t list
    | Cons of tag * t
    | Mutez of Mutez.t
    | Address of Address.t
    | Op of operation

  and operation =
    | Transaction of { sender : Address.t ;
                       target : Address.t ;
                       entrypoint : string ;
                       arg : t ;
                       amount : Mutez.t }
    | Origination of { originator : Address.t ;
                       address : Address.t ;
                       code  : Contract.t ;
                       amount : Mutez.t }

  val show : t -> string
  val show_operation : operation -> string
end =
struct
  type tag = int

  type t =
    | Unit
    | Int of int
    | Bool of bool
    | String of string
    | Tuple of t list
    | Cons of tag * t
    | Mutez of Mutez.t
    | Address of Address.t
    | Op of operation

  and operation =
    | Transaction of { sender : Address.t ;
                       target : Address.t ;
                       entrypoint : string ;
                       arg : t ;
                       amount : Mutez.t }
    | Origination of { originator : Address.t ;
                       address : Address.t ;
                       code  : Contract.t ;
                       amount : Mutez.t }

  let rec show (x : t) =
    match x with
    | Unit -> "Unit"
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | String s -> Printf.sprintf "\"%s\"" s
    | Tuple l ->
      let l = List.map show l in
      let r = String.concat ";" l in
      "[ " ^ r ^ " ]"
    | Cons (tag, v) ->
      Printf.sprintf "[%d]{%s}" tag (show v)
    | Mutez m -> Mutez.to_string m
    | Address addr -> Address.to_string addr
    | Op op ->
      show_operation op

  and show_operation (op : operation) =
    match op with
    | Transaction { sender ; target ; entrypoint ; arg ; amount } ->
      Printf.sprintf "transaction from %s to %s.%s with arg (%s), %s mutez"
        (Address.to_string sender)
        (Address.to_string target)
        entrypoint
        (show arg)
        (Mutez.to_string amount)
    | Origination { originator ; address ; code ; amount } ->
      let sto =
        let module C = (val code) in
        C.show C.state in
      Printf.sprintf "origination by %s to address %s, initial storage %s, amount = %s"
        (Address.to_string originator)
        (Address.to_string address)
        sto
        (Mutez.to_string amount)
end

and State :
sig
  type state

  type 'a t

  type error

  val empty : state

  val internal_error : string -> 'a t
  val proto_error : string -> 'a t
  val user_error : string -> 'a t

  val run : unit t -> state
  val return : 'a -> 'a t

  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  val (let+) : 'a t -> ('a -> 'b) -> 'b t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t

  (** Address & contract handling *)
  val fresh_address : Address.t t
  val contract_mem : Address.t -> bool t
  val contract_find : Address.t -> (Contract.t * Mutez.t) t
  val contract_add : Address.t -> (Contract.t * Mutez.t) -> unit t
  val contract_get_balance : Address.t -> Mutez.t t
  val contract_set_balance : Address.t -> Mutez.t -> unit t
  val contract_set_code : Address.t -> Contract.t -> unit t
  val append_operations : Value.operation list -> unit t
  val take_operation : Value.operation option t
end =
struct

  (* The protocol state *)

  type storage = (Contract.t * Mutez.t) Address.Map.t

  type state =
    { storage  : storage ;
      nonce    : int ;
      op_queue : Value.operation list }

  (* We use a custom state+error monad for the protocol. *)
  type error =
    | Protocol_error of string
    | Internal_error of string
    | User_error of string

  type 'a t = state -> ('a * state, error) result

  let empty =
    { storage  = Address.Map.empty ;
      nonce    = 0 ;
      op_queue = [ ] }

  let error_to_string err =
    match err with
    | Protocol_error s ->
      Printf.sprintf "Protocol error: %s" s
    | Internal_error s ->
      Printf.sprintf "Internal error: %s" s
    | User_error s ->
      Printf.sprintf "User error: %s" s

  let return (x : 'a) : 'a t =
    fun state -> Ok (x, state)

  let proto_error (s : string) : 'a t =
    fun _ -> Error (Protocol_error s)

  let internal_error (s : string) : 'a t =
    fun _ -> Error (Internal_error s)

  let user_error (s : string) : 'a t =
    fun _ -> Error (User_error s)

  let run (x : unit t) =
    match x empty with
    | Ok (_res, state) -> state
    | Error err ->
      failwith (error_to_string err)

  let bind (m : 'a t) (f : 'a -> 'b t) =
    fun state ->
    match m state with
    | Ok (res, state) -> f res state
    | Error err -> Error err

  let apply (m : 'a t) (f : 'a -> 'b) =
    fun state ->
    match m state with
    | Ok (res, state) -> Ok (f res, state)
    | Error err -> Error err

  let (>>=) = bind
  let (>>|) = apply


  let (let*) = bind
  let (let+) = apply

  let fresh_address : Address.t t =
    fun state ->
    let addr  = Printf.sprintf "internal_contract_%d" state.nonce in
    let nonce = state.nonce + 1 in
    Ok (Address.of_string addr, { state with nonce })

  let contract_mem : Address.t -> bool t =
    fun addr state ->
    let res = Address.Map.mem addr state.storage in
    Ok (res, state)

  let contract_add : Address.t -> (Contract.t * Mutez.t) -> unit t =
    fun addr (code, bal) state ->
    let storage = Address.Map.add addr (code, bal) state.storage in
    Ok ((), { state with storage })

  let contract_find : Address.t -> (Contract.t * Mutez.t) t =
    fun addr state ->
    match Address.Map.find addr state.storage with
    | res -> Ok (res, state)
    | exception Not_found ->
      Error (Internal_error "contract_find: invalid address")

  let contract_get_balance : Address.t -> Mutez.t t =
    fun addr state ->
    match Address.Map.find addr state.storage with
    | (_, bal) -> Ok (bal, state)
    | exception Not_found ->
      Error (Internal_error "contract_get_balance: invalid address")

  let contract_set_balance : Address.t -> Mutez.t -> unit t =
    fun addr new_bal state ->
    match Address.Map.find addr state.storage with
    | (code, _bal) ->
      let storage = Address.Map.add addr (code, new_bal) state.storage in
      Ok ((), { state with storage })
    | exception Not_found ->
      Error (Internal_error "contract_set_balance: invalid address")

  let contract_set_code : Address.t -> Contract.t -> unit t =
    fun addr new_code state ->
    match Address.Map.find addr state.storage with
    | (_code, bal) ->
      let storage = Address.Map.add addr (new_code, bal) state.storage in
      Ok ((), { state with storage })
    | exception Not_found ->
      Error (Internal_error "contract_set_balance: invalid address")

  let append_operations : Value.operation list -> unit t =
    fun ops state ->
    Ok ((), { state with op_queue = state.op_queue @ ops })

  let take_operation : Value.operation option t =
    fun state ->
    match state.op_queue with
    | [] -> Ok (None, state)
    | op :: tl ->
      let state = { state with op_queue = tl } in
      Ok (Some op, state)

end
