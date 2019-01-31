(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type tutorial =
  { tutorial_title : Learnocaml_index.text ;
    tutorial_steps : step list }
and step =
  { step_title : Learnocaml_index.text ;
    step_contents : phrase list }
and phrase =
  | Paragraph of Learnocaml_index.text
  | Enum of phrase list list
  | Code_block of Learnocaml_index.code

open Json_encoding

