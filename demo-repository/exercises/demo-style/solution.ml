let rec member x l =
  match l with
  | [] -> false
  | y :: ys -> x = y || member x ys

let rec length l =
  match l with
  | [] -> 0
  | _ :: xs -> 1 + length xs

let rec filter p l =
  match l with
  | [] -> []
  | x :: xs ->
    if p x then x :: filter p xs
    else filter p xs

let num_multiples x l =
  length (filter (divides x) l)
