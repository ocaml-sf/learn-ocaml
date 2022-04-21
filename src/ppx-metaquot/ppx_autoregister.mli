module type ARG = sig
  val val_prefix: string
  val inject_def: string -> string -> string Ppxlib.loc -> Ppxlib.expression
end

module Make (_: ARG): sig
  val expand: Ppxlib.structure -> Ppxlib.structure
end
