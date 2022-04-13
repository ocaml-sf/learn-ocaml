open Ppxlib

module type ARG = sig
  val val_prefix: string
  val inject_def: string -> string loc -> expression
end

module Make (Arg: ARG) = struct

let pattern_defs =
  object
    inherit [(string * string loc) list] Ast_traverse.fold as super

    method! pattern p acc =
      let acc = super#pattern p acc in
      match p.ppat_desc with
      | Ppat_var var | Ppat_alias (_, var) -> (
          match String.index_opt var.txt '_' with
          | Some i when String.sub var.txt 0 i = Arg.val_prefix ->
              let suffix =
                String.sub var.txt (i + 1) (String.length var.txt - i - 1)
              in
              (suffix, var) :: acc
          | _ -> acc)
      | _ -> acc
  end

let rec get_defs bindings acc =
  match bindings with
  | [] -> List.rev @@ List.flatten acc
  | binding :: rest ->
      get_defs rest @@ (pattern_defs#pattern binding.pvb_pat [] :: acc)

module Ast_builder = Ast_builder.Make (struct
  let loc = Location.none
end)

let val_recorder s =
  let open Ast_builder in
  let create_val_registration defs =
    let gen_expr (name, e) = Arg.inject_def name e in
    let val_registration = List.map gen_expr defs |> esequence in
    let register_toplevel =
      [ value_binding ~pat:punit ~expr:val_registration ]
    in
    pstr_value Nonrecursive register_toplevel
  in
  List.fold_right
    (fun si acc ->
      match si.pstr_desc with
      | Pstr_value (_, bindings) -> (
          match get_defs bindings [] with
          | [] -> si :: acc
          | defs -> si :: create_val_registration defs :: acc)
      | _ -> si :: acc)
    s []

let expand = val_recorder

end
