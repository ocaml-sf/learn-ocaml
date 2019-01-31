(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type t = {
  sync_dir: string;
  cert: string option;
  port: int;
}

val term: string Cmdliner.Term.t -> t Cmdliner.Term.t
