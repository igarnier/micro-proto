type t = int
type span = int

let zero = 0

let compare (x : int) (y : int) =
  if x < y then -1
  else if x = y then 0
  else 1

let span x = x

let add = (+)
