(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

module type Section_name = sig 
  val section : string
end

module type S = sig
  type t = {
    sync_dir: string;
    base_url: string;
    port: int;
    cert: string option;
    replace: bool;
    child_pid: int option;
  }

  val term: string Cmdliner.Term.t -> string Cmdliner.Term.t -> int option Cmdliner.Term.t -> t Cmdliner.Term.t
end

module Args : functor (_ : Section_name) -> S
