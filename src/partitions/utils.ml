let maybe e f = function
  | None -> e
  | Some x -> f x

let fmapOption f = maybe None (fun x -> Some (f x))

let bindOption f = maybe None f
