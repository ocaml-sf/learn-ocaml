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

open Learnocaml_data

val find_div_or_append_to_body : string -> [> Html_types.div ] Tyxml_js.Html.elt

val find_component : string -> 'a Tyxml_js.Html.elt

val gettimeofday : unit -> float

val fake_download : name: string -> contents: Js.js_string Js.t -> unit

val fake_upload : unit -> (string * Js.js_string Js.t ) Lwt.t

val fatal : string -> unit

val alert : ?title: string -> string -> unit

val catch_with_alert : ?printer: (exn -> string) -> (unit -> unit Lwt.t) -> unit Lwt.t

val hide_loading : ?id: string -> unit -> unit

val show_loading : ?id: string -> [< Html_types.div_content_fun ] Tyxml_js.Html.elt list -> unit

val set_assoc : string -> 'a -> (string * 'a) list -> (string * 'a) list

val delete_assoc : string -> (string * 'a) list -> (string * 'a) list

val arg : string -> string

val set_arg : string -> string -> unit

val delete_arg : string -> unit

type button_group

val button_group : unit -> button_group

type button_state

val button_state : unit -> button_state

val disable_button_group : button_group -> unit

val enable_button_group : button_group -> unit

val button_group_disabled : button_group -> bool

val disable_button : button_state -> unit

val enable_button : button_state -> unit

val disabling_button_group : button_group -> (unit -> unit Lwt.t) -> unit Lwt.t

val disable_with_button_group :
  < disabled : bool Js.t Js.prop ; .. > Js.t ->
  button_group -> unit

val button :
  container: 'a Tyxml_js.Html.elt ->
  theme: string ->
  ?group: button_group ->
  ?state: button_state ->
  icon:string ->
  string -> (unit -> unit Lwt.t) ->
  unit

val render_rich_text :
  ?on_runnable_clicked: (string -> unit) ->
  Learnocaml_data.Tutorial.text ->
  [< Html_types.phrasing > `Code `Em `PCDATA ] Tyxml_js.Html.elt list

val extract_text_from_rich_text : Learnocaml_data.Tutorial.text -> string

val set_state_from_save_file :
  ?token:Token.t -> Save.t -> unit

val get_state_as_save_file : unit -> Save.t

(** Sync the local save state with the server state, and returns the merged save
    file. The save will be created on the server if it doesn't exist. *)
val sync: Token.t -> Save.t Lwt.t
