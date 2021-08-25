(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data

val parse_md_tutorial :
     tutorial_name:string
  -> file_name:string
  -> (Tutorial.Index.entry * Tutorial.t) Lwt.t

val parse_html_tutorial :
     tutorial_name:string
  -> file_name:string
  -> (Tutorial.Index.entry * Tutorial.t) Lwt.t

val print_html_tutorial : tutorial_name:string -> Tutorial.t -> string

val print_md_tutorial : Tutorial.t -> string
