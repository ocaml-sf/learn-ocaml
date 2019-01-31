(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

val teacher_tab:
  Learnocaml_data.Token.t -> (unit -> 'a Lwt.t) -> 'b  -> unit ->
  [> Html_types.div ] Tyxml_js.Html5.elt Lwt.t
