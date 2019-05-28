let rec member x l =
  match l with
  | [] -> false
  | y :: ys -> if x = y then true else member x ys

let rec length l =
  if l = [] then 0
  else 1 + length (List.tl l)

let rec filter p l =
  match l with
  | [] -> []
  | x :: xs ->
    if p x then [x] @ filter p xs
    else filter p xs

let num_multiples x l =
  length (filter (fun y -> divides x y) l)
