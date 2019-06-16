open Error_monad

type t = int

let of_int x =
  if x < 0 then
    error "Mutez.of_int: negative integer"
  else
    return x

let of_int_exn x =
  run (of_int x)

let to_string = string_of_int

let zero = 0

let (+) x y = x + y

let (-) x y =
  if y > x then
    error "Mutez.(-): invalid argument"
  else
    return (x - y)

let (=) x y = x = y

let (<) x y = x < y

let (<=) x y = x <= y
