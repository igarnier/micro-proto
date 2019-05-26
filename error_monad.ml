type nonrec 'a result = ('a, string) result

let return (x : 'a) : 'a result = Ok x
let error (s : string) : 'a result = Error s
let run (x : 'a result) =
  match x with
  | Ok x -> x
  | Error err -> failwith err

let bind (m : 'a result) (f : 'a -> 'b result) =
  match m with
  | Ok x -> f x
  | Error err -> Error err

let apply (m : 'a result) (f : 'a -> 'b) =
  match m with
  | Ok x -> Ok (f x)
  | Error err -> Error err

let (>>=) = bind
let (>>|) = apply
