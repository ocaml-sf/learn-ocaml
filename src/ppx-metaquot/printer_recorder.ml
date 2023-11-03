(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2022-2023 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

include Ppx_autoregister.Make(struct
    let val_prefix = "print"
    let inject_def id name var =
      let open Ppxlib in
      let open Ast_builder.Default in
      let loc = var.Location.loc in
      pexp_apply ~loc
        (evar ~loc "Learnocaml_internal.install_printer")
        [ Nolabel, estring ~loc (Ppx_autoregister.modname var);
          Nolabel, estring ~loc id;
          Nolabel, estring ~loc name;
          Nolabel, evar ~loc var.txt ]
  end)
