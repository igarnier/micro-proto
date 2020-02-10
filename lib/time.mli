(** Block height, nanoseconds, whatever that monotonically increases. *)
type t = private int

(** A time _span_. *)
type span

val zero : t

val compare : t -> t -> int

val span : int -> span

val add : t -> span -> t
