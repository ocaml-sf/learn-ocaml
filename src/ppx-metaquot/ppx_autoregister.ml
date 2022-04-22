(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2022 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Ppxlib

module type ARG = sig
  val val_prefix: string
  val inject_def: string -> string -> string loc -> expression
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

let gen_expr (name, e) =
  let id =
    (* Create a fresh id that will be exported in the interface, so that looking
       up the register function type in the cmi can't be tricked by later
       redefinitions with a different type *)
    Printf.sprintf "learnocaml_autoregister_%s_%06X"
      name (Random.int 0xFFFFFF)
  in
  ({txt=id; loc=e.loc}, e), Arg.inject_def id name e

let val_recorder s =
  let open Ast_builder in
  let create_val_registration defs =
    let ids, exprs = List.split (List.map gen_expr defs) in
    let val_registration = esequence exprs in
    let register_toplevel =
      List.map (fun (id, e) ->
          value_binding
            ~pat:(Ast_builder.ppat_var id)
            ~expr:(Ast_builder.pexp_ident
                     {txt=Longident.Lident e.txt; loc=e.loc}))
        ids
      @ [ value_binding ~pat:punit ~expr:val_registration ]
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

let modname var =
  (* This is fragile. Do we have a better way to recover the current
     compilation unit name in a ppx ? *)
  String.capitalize_ascii @@
  Filename.basename @@
  Filename.remove_extension @@
  var.Location.loc.Location.loc_start.Lexing.pos_fname
