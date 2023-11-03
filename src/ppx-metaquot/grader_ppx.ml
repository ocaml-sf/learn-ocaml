(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2022-2023 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

let () =
  Migrate_parsetree.Driver.register ~name:"ppx_metaquot" (module Migrate_parsetree.OCaml_412)
    (fun _config _cookies -> Ppx_metaquot.Main.expander []);
  Ppxlib.Driver.register_transformation "print_recorder" ~impl:Printer_recorder.expand;
  Ppxlib.Driver.register_transformation "sample_recorder" ~impl:Sampler_recorder.expand
