let maybe e f = function
  | None -> e
  | Some x -> f x

module Err : sig
  type 'a t

  val fail : 'a t
  val ret : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val run : 'a t -> 'a option
  val to_err : 'a option -> 'a t
end = struct
  type 'a t = 'a option
  let fail = None
  let ret x = Some x

  let ( >>= ) x f = maybe None f x

  let run x = x
  let to_err x = x
end
