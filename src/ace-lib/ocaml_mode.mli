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

type editor

type loc = Ace.loc = {
  loc_start: int * int;
  loc_end: int * int;
}

type error = {
  locs: loc list;
  msg: string;
}

type warning = {
  loc: loc;
  msg: string;
}

val create_ocaml_editor: Dom_html.divElement Js.t -> editor
val get_editor: editor -> editor Ace.editor

val report_error: editor -> ?set_class: bool -> error option -> warning list -> unit Lwt.t
val report_current_error: editor -> ?set_class: bool -> unit -> unit Lwt.t

val get_current_error: editor -> error option
val get_current_warnings: editor -> warning list

val token_type : Approx_tokens.token -> string
