(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2022-2023 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

let () =
  let mapper = Ppx_metaquot.expander [] in
  let impl = mapper.Ast_mapper.structure mapper in
  let intf = mapper.Ast_mapper.signature mapper in
  Ppxlib.Driver.register_transformation_using_ocaml_current_ast
    "ppx_metaquot" ~impl ~intf

let () =
  Ppxlib.Driver.register_transformation "print_recorder" ~impl:Printer_recorder.expand;
  Ppxlib.Driver.register_transformation "sample_recorder" ~impl:Sampler_recorder.expand
