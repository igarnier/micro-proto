type 'a result
(* Ok of 'a | Error of string *)

val return : 'a -> 'a result
val error : string -> 'a result
val run : 'a result -> 'a
(* [run] raises Failure in case of error *)

val (>>=) : 'a result -> ('a -> 'b result) -> 'b result
val (>>|) : 'a result -> ('a -> 'b) -> 'b result
