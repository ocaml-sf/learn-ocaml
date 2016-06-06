(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

let ppx_rewriters = ref []

let () =
  Ast_mapper.register_function :=
    (fun _ f -> ppx_rewriters := f :: !ppx_rewriters)

let preprocess_structure str =
  let open Parsetree in
  let open Ast_mapper in
  List.fold_right
    (fun ppx_rewriter str ->
       let mapper = ppx_rewriter [] in
       mapper.structure mapper str)
    !ppx_rewriters
    str

let preprocess_signature str =
  let open Parsetree in
  let open Ast_mapper in
  List.fold_right
    (fun ppx_rewriter str ->
       let mapper = ppx_rewriter [] in
       mapper.signature mapper str)
    !ppx_rewriters
    str

let preprocess_phrase phrase =
  let open Parsetree in
  match phrase with
  | Ptop_def str -> Ptop_def (preprocess_structure str)
  | Ptop_dir _ as x -> x

