(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2022-2023 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

let () =
  Migrate_parsetree.Driver.run_main ~exit_on_error:true ()
