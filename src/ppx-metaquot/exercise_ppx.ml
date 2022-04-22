(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2022 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

let () =
  Ppxlib.Driver.register_transformation "print_recorder" ~impl:Printer_recorder.expand
