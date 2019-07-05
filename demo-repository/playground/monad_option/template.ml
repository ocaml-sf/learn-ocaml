let return x = Some x;;

let map f = function
  | None -> None
  | Some x -> Some (f x);;

let bind x f =
  match x with
  | None -> None
  | Some x -> f x;;
