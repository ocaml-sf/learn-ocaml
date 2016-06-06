(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

type output

val setup :
  ?limit: int ->
  ?on_resize:(unit -> unit) ->
  container: [ `Div ] Tyxml_js.Html5.elt ->
  unit -> output

val clear : output -> unit

val scroll : output -> unit

val oldify : output -> unit

val output_stdout : output -> string -> unit

val output_stderr : output -> string -> unit

val output_html : output -> string -> unit

type phrase

val phrase : unit -> phrase

val output_code : ?phrase: phrase -> output -> string -> unit

val output_answer : ?phrase: phrase -> output -> string -> unit

val output_error : ?phrase: phrase -> output -> Toploop_results.error -> unit

val output_warning : ?phrase: phrase -> output -> Toploop_results.warning -> unit

val format_ocaml_code : string -> [> `Span | `PCDATA ] Tyxml_js.Html5.elt list
