(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type lesson =
  { lesson_title : string (* may contains HTML formatting *) ;
    lesson_steps : step list }
and step =
  { step_title : string (* may contains HTML formatting *) ;
    step_phrases : phrase list }
and phrase =
  | Text of string (* may contains HTML formatting *)
  | Code of string

val lesson_enc : lesson Json_encoding.encoding
