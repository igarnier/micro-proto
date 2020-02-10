(* 
Idea:
- load code in top level
- the state of the game is persistent data
- which one rewrites by injecting moves
*)

(**
Todo:
- build "tenary" with resampling by game
- finish cl.pay dans ffuse
- add ask field in position
- introduce the ask/take mechanism to resell self-extensions 
(NB: upfront payment become easy if we resell)
- abstraire le code sur les clauses
- provision! lock funds, it is the whole point
- implement multiple tradelines as an array (or a map?)
- implement trapdoor move when > max_delay (already done, anyone can B-move) 
- payments are a functor from the trace = move sequence to tuples of debts
- add a reverse map: Positions -> Players for payments?
 **)

(** 
Optimisations:
- simpler to have owner inside the position?
- to not List.rev all the time keep the list "folded": l <- rev(l1),l2
**)

(**
 . utilities

**)

let print_int_list =
  List.iter (fun ele -> print_int ele; print_string ";")
;;

(** 
I. ownership map: players -> position list 
**)
  
module Own = 
  Map.Make (struct
	      type t = string
	      let compare = String.compare
	    end)
;;

(* pretty print an ownership map *)
let print_map m =
  print_string "{|";
  Own.iter
    (fun x l -> print_string " ";
                print_string x;
                print_string " -> ";
                print_int_list l;
                print_string " |") m;
  print_string "}\n"
;;


(* examples of ownership map *)
let zero = Own.empty;;
let one = Own.add "Alice" [1;6] zero;;
let two = Own.add "Bob" [3;5;7] one;;
let three = Own.add "Charlie" [12;13] two;;
print_map three;;

let mapex = Own.add "Charlie" [11;12] (Own.add "Donna" [1;5]
(Own.add "Bob" [5;7] (Own.add "Alice" [3;6] Own.empty)));;
print_map mapex;;

(* not used *)
let split_map_by_pos_nb ~map:m ~nb:n =
  let p1, p2 =
    Own.partition
    (fun k el -> ((List.length el) > n)) m
    in 
    print_map p1;
    print_map p2
;;

(* costly method to find owner of given position *)
(* returns the smallest key if multiple owners *)
let find_owner pos map =
  let ll = Own.bindings map
  in
  let rec aux ll =
    match
      ll
    with
    [] -> failwith "find_owner: ownerless position"
  | (s1,l1)::ll2 ->
     if (List.mem pos l1)
     then s1
     else aux ll2
  in
  aux ll
;;

(* check ownership *)
(* a function dual to find_owner *)
let check_ownership key pos m =
  match
    Own.find_opt key m
  with
    None -> false
  | (Some l) -> List.mem pos l
;;

(* to use when extending *)
let add_position_map key pos =
  let aux pos1 r =
    match r
      with
        (Some l) -> (Some (pos1::l))
      | None -> (Some [pos1])
  in
  Own.update key (aux pos)
;;

(* to use when contracting *)
let rem_position_map key pos =
  let aux pos1 r =
    match r
      with
        (Some l) -> (Some (List.filter (fun x -> x != pos) l))
      | None -> None
  in
  Own.update key (aux pos)
;;

let swap_position key1 key2 pos =
  fun m ->
  add_position_map key2 pos
    (rem_position_map key1 pos m)
;;

find_owner 3 mapex;;
check_ownership "Alice" 3 mapex;;
check_ownership "Bob" 3 mapex;;
print_map (swap_position "Charlie" "Donna" 12 mapex);;

let four =
  (add_position_map "Alice" 22
     (add_position_map "Donna" 34
        (rem_position_map "Bob" 7 mapex)));;

print_map four;;

(** 
II. (global) trade line parameters 
on peut imaginer que le max_delay est local a une tl
**)

let time = ref 0     (* global time *)
and max_delay = 1000 (* time to live - to unlock dusty trade-lines, cf. bfuse*)
(* and ctr = ref 0    (* numero d'entrée dans la tl *) *)
(* and max_size = 4   (* max length of tl *)*)
;;

(* advancing time *)
let tick () = incr time
;;

(** 

II.A lines

position <- coordinate:int, fwd price:float, bwd delay:int

we think of the coordinate x as a position-token 
which is issued by the trade line at the time of extension 
- issuance can be done somewhere else

NB: x = entry index in line != index in line 

**)

(* an example line *)
let lex =
  [
    (1,10.0,(130,0.03));
    (3,1.,(60,0.01));
    (5,1.5,(3,0.02));
    (6,0.3,(32,0.03));
    (7,0.,(0,0.15))
  ]
;;

(* pretty printing clauses and lines *)
let print_clauses =
  fun (pos,fwd_price,(delay,penalty)) ->
  let si = string_of_int pos
  and sf = string_of_float fwd_price
  and sj = string_of_int delay
  and sp = string_of_float penalty
  and op = ""
  and cp = ")-> "
  in
  let str = String.concat "" [op;si;" -(";sf;",(";sj;",";sp;")";cp]
  in
  print_string str
;;

let print_line l =
  print_string "{| ";
  List.iter print_clauses l;
  print_string "* |}"
;;

print_line lex
;;

(* 
the trade line is a persistent object with 
- a max length
- a mutable clicker = name  generator for new positions 
- a line = chain of bilateral std contracts
- an ownership map
*)

(** II.B trade lines **)
(*
in general, 
we need a local context for a trade line, namely the payments in flight,
but here we "atomise" active payments with a pay-as-you-push model;
and we set penalties to 0, so no passive payments needed either 
*)

type trade_line =
  {
    mutable max_length: int;
    mutable clicker:int; (* name*)
    mutable line: (int*float*(int*float)) list;
    mutable map: int list Own.t;
    mutable pay: (string*float*string) list
  }
;;

(*
NB: invariant on clicker: 
tl.clicker(s) = max_{u <= s} positions in tlt
not necessarily equal because of bwd fusion:
1 -> 2 -> 3
-- B2 --
1 -> 2
-- E2 --
1 -> 2 -> 4
*)

let print_pay =
  List.iter
    (fun (s1,f,s2) ->
      print_string
        (String.concat "" [s1;" -";string_of_float f;"-> ";s2;", "])
    )
  ;;

let print_trade_line clex =
  print_string "time: ";
  print_int !time;
  print_string "\n";
  print_string "maxl: ";
  print_int clex.max_length;
  print_string "\n";
  print_string "clic: ";
  print_int clex.clicker;
  print_string "\n";
  print_string "line: ";
  print_line clex.line;
  print_string "\n";
  print_string "omap: ";
  print_map clex.map;
  print_string "payl: ";
  print_pay clex.pay;
  print_string "\n"
;;

(* examples of trade lines *)
(* assert: clicker >= rank of List.last *)
(* delta clicker/delta time = frequency of line *)

let make_trade_line
      ~max_length:n
      ~clicker:c
      ~line:l
      ~map:m
      ~pay:p
  =
  {
    max_length = n;
    clicker = c; 
    line = l;
    map = m;
    pay=p
  }  
;;

let clex_zero = 
    make_trade_line
    ~max_length:10
    ~clicker:1 
    ~line:[(1,0.,(0,0.))]
    ~map:(add_position_map "Alice" 1 Own.empty)
    ~pay:[]
;;

let clex = 
    make_trade_line
    ~max_length:10
    ~clicker:9
    ~line:lex
    ~map:mapex
    ~pay:[]
;;

print_trade_line clex_zero;;
print_trade_line clex;;

(* to reset an example trade line and time *)
let re_ex ~time_point:t ~trade_line:tl ~clicker:c ~map:m ~line:l=
  time := t;
  tl.max_length <- 10;
  tl.clicker <- c;
  tl.line <- l;
  tl.map <- m;
  tl.pay <- []
;;

re_ex ~time_point:67 ~clicker:11 ~trade_line:clex ~map:mapex ~line:lex;;
  
clex;;
!time;;

(* 
NB: in this representation of a line there is a "ghost edge" hanging from the last node.
The actual values is to be set when extending the line;
a natural thing would be to set it to neutral elements in the semi-ring fusions
a=\lambda t.0, p=0, \Delta = +\infty
but we use std fusions to begin with ... 
*)

(** II.C moves **)
(* backward contraction *)
let bfuse (player:string) (x:int) (cl:trade_line) =
  (* check ownership *)
  if (check_ownership player x cl.map) 
  then
    (* definition des fusions bwd *)
    let bbmerge (bad,bap) (bad',bap') = (bad,bap)
    and bfmerge phi phi' = phi +. phi'
    in 
    let rec aux l1 l2 =
      match
        l2
      with
        [] -> failwith "B-move: x not found"
      | (rank,phi,(bad,bap))::l21 ->
         if (rank = x)
         then 
           match
             l21
           with
             [] -> failwith "B-move: x is last in the line"
           (* could just return the line intact here ??*)
           | (rank1,phi',ba')::l22 ->
              if (!time < bad) && (!time < max_delay) (* long delays are pre-empted*)
              then failwith "B-move: too early"
              else 
                (List.rev l1)@((x,bfmerge phi phi',bbmerge (bad,bap) ba')::l22),
                rank1,
                bap
         else
           aux ((rank,phi,(bad,bap))::l1) l21
    in
    let newline, oldpos, bap = aux [] cl.line
    in
    cl.line <- newline;
    cl.pay <- (find_owner oldpos cl.map,bap,player)::cl.pay;
    (* pay penalty before being kicked from map !*)
    cl.map <- (Own.map (List.filter (fun x -> x!= oldpos)) cl.map)
  else
    failwith "player does not own the position (which may not exist)"
;;

(* forward contraction *)
let ffuse  (player:string) x cl =
  if (not (check_ownership player x cl.map))
  then failwith "player does not own the position (which may not exist)";
     (* if not we reject silently not revealing
          if the position exists *)
  let fbmerge ba ba' = ba'
  and ffmerge phi phi' = phi +. phi'
  in 
  let rec aux l1 l2 =
    match
      l2
    with
      [] -> failwith "x not found"
    | (rank,phi',ba')::l21 ->
       if (rank = x)
       then 
         match
           l1
         with
           [] -> failwith "x is first in the line"
         | (rank1,phi,ba)::l11 -> (List.rev l11)@
                                    ((x,ffmerge phi phi',fbmerge ba ba')::l21), rank1,phi
       else
         aux ((rank,phi',ba')::l1) l21
  in
  let newline, oldpos,fwd_price = aux [] cl.line
  in
  cl.line <- newline;
  cl.pay <- (player,fwd_price,find_owner oldpos cl.map)::cl.pay;
  cl.map <- (Own.map (List.filter (fun x -> x!= oldpos)) cl.map)
;;

(* extension 
- one can extend only at the end of the line
- and only if length line <= max_length
- altvly we could control the nb fo extensions/time click
 *)

(* 
- it is a self-extension model with new pos later to be sold irreversibly
- simpler than multisig or t-lock
 *)

(* 
one has to declare the position of extension =
think of it as a mock proof of authentication
*)

let xtend (player:string) (x:int) (cl:trade_line) (phi':float) (beta':(int*float)) =
  (* if not we do nothing - silent rejection *)
  if (not (check_ownership player x cl.map))
  then failwith "player does not own the position (which may not exist)";
  if (List.length cl.line >=  cl.max_length)
  then failwith "the line is already at max size";
  let newpos = cl.clicker + 1
  in
  (* print_string "*"; *)
  let rec aux l1 l2 =
    match
      l2 
    with
      [] -> failwith "x is not in the trade line"
    | (rank,phi,beta)::l21 ->
       if (rank=x)
       then
           match
             l21
           with
             [] -> List.rev ((newpos,0.,(0,0.))::(x,phi',beta')::l1)
           | _  -> failwith "x is not at the end of the line"
       else
         aux ((rank,phi,beta)::l1) l21
  in
  cl.line <- aux [] cl.line;
  cl.clicker <- newpos (* global ranking counter of the line *);
  cl.map <- (add_position_map player newpos cl.map)
;;

(* test *)
let test_fuse bool =
  let fuse = if (bool) then bfuse else ffuse
  in
  for i=1 to 10
  do
    try
      fuse "Alice" i clex;
      print_line clex.line;
      print_string "\n"
    with
      Failure s -> print_string s; print_string "\n"
  done
;;

(* should buy pos for the explicit ask price ?*)
(* irreversible sell of position *)
let isell (seller:string) (buyer:string) (pos:int) (price:float) (tl:trade_line) =
  tl.map <- swap_position seller buyer pos tl.map;
  tl.pay <- (buyer,price,seller)::tl.pay
;;

(* let set_ask player pos price = *)

(** III. games **)
(** III.A. intermediation game* *)
time := 0;; 
let tl0 =  make_trade_line
             ~max_length:3 ~clicker:0 ~line:[0,0.,(0,0.)]
             ~map:(add_position_map "Sue" 0 Own.empty) ~pay:[]
;;
let p0 () = print_trade_line tl0;;
p0();;
xtend "Sue" 0 tl0 103. (32,10.);;  (* Sue the Seller extends *)
p0();;
isell "Sue" "Bob" 1 0.001 tl0;;   (* Bob the Broker takes an option *)
p0();;
xtend "Bob" 1 tl0 0.01 (4,10.);;
p0();;
isell "Bob" "Chang" 2 0.002 tl0;; (* Chang takes an 2-option *)
p0();;
(* here we have a ternary game Seller-Broker-Buyer:
time: 0
maxl: 3
clic: 2
line: {| 0 -(0.1,(32,10.))-> 1 -(0.01,(4,10.))-> 2 -(0.,(0,0.))-> * |}
omap: {| Bob -> 1; | Chang -> 2; | Sue -> 0; |}
payl: Chang -0.002-> Bob, Bob -0.001-> Sue, 
*)
bfuse "Bob" 1 tl0;; (* "B-move: too early" *)
p0();; (* nothing has changed *)
time := 4;; (* time passes - Chang does not buy*)
(* resampling: the Bv branch *)
bfuse "Bob" 1 tl0;; (* Bob kicks Chang, Chang pays penalty*)
p0();;
(* the Fw';Fw branch*)
xtend "Bob" 1 tl0 0.01 (8,10.);; (* Bob resets the offer with new term + 4*)
p0();;
isell "Bob" "Demi" 3 0.0001 tl0;;
p0();;
ffuse "Demi" 3 tl0;; (* Demi buys 2-option*)
p0();;
ffuse "Demi" 3 tl0;; (* Demi buys 1-option, and resolves the tradeline *)
p0();;
(** 
payoff stack:
Demi -103.-> Sue
Demi -0.01-> Bob
Demi -0.0001-> Bob
Chang -10.-> Bob
Chang -0.002-> Bob
Bob -0.001-> Sue
**)

(** III.B simulation of a game - including bad moves **)
let tl1 = make_trade_line ~max_length:9 ~clicker:11
            ~line:lex ~map:mapex ~pay:[]
;;
let p1 () = print_trade_line tl1;;
p1();;
xtend "Alice" 1 tl1 0.01 (5,0.2);;
xtend "Alice" 2 tl1 0.01 (5,0.2);;
xtend "Alice" 3 tl1 0.01 (5,0.2);;
xtend "Alice" 4 tl1 0.01 (5,0.2);;
xtend "Alice" 5 tl1 0.01 (5,0.2);;
xtend "Alice" 6 tl1 0.01 (5,0.2);;
xtend "Bob" 7 tl1 0.01 (5,0.2);;
p1();;
xtend "Alice" 8 tl1 0.01 (5,0.2);;
xtend "Alice" 9 tl1 0.01 (5,0.2);;
xtend "Alice" 10 tl1 0.01 (5,0.2);;
xtend "Alice" 11 tl1 0.01 (5,0.2);;
xtend "Bob" 12 tl1 0.01 (5,0.2);;
p1();;
bfuse "Alice" 3 tl1;;
time := 60;;
bfuse "Alice" 3 tl1;;
p1();;
bfuse "Bob" 13 tl1;;
bfuse "Bob" 12 tl1;;
p1();;
bfuse "Alice" 3 tl1;;
p1();;


(** IV. notes  **)
(* 
offer/take 
irreversible sale via ownership structure 
*)


(* simpler to have owner inside the position?
 * type position = {
 * mutable owner:string; 
 * x:int; 
 * mutable price:float; 
 * mutable delay:int;
 * mutable penalty:float;
 * mutable ask:float
 * }
 * 
 * 
 * let p1 = {owner="Alice";x=0;price=1000.;delay=100;penalty=2.;ask = 1000.}
 * and p2 = {owner="Bob";x=1;price=10.;delay=10;penalty=3.;ask=10.} 
 * and p3 = {owner="Charlie";x=2;price=0.;delay=0;penalty=0.; ask=10.}
 * ;;
 * 
 * let tl1 = [p1;p2;p3]
 * ;; 
*)


(*

let tl_start owner coin_id max_length max_time =

let open_trade_line () =
  ...

let close_trade_line ... =
  ...

let offer position =

if (exists position)
  then 
  let amount = msg.pay 
  and potential_owner = msg.sender in
  if (amount > ask(position))
  then 
  transfer amount (!owner(position));
  owner(position) := potential_owner
  

let take position payment = 

*)
