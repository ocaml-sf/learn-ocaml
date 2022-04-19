(* (hidden) interface of the module that will be pre-loaded in the toplevel *)
module type S = sig
  val register_printer: string -> ('a -> 'b) -> unit
end
