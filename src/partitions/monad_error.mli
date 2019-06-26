val maybe : 'b -> ('a -> 'b) -> 'a option -> 'b

module Err : sig
  type 'a t

  val fail : 'a t
  val ret : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val run : 'a t -> 'a option
  val to_err : 'a option -> 'a t
end
