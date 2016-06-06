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

(** An OCaml toplevel whose input and output will be in a given HTML [div]. *)

open Tyxml_js

(** An abstract type representing a toplevel instance. *)
type t

(** Create a toplevel instance whose input and output will be in a given [div].

   @param container_id the identifier of a [div] in the current DOM
          that will be used for the toplevel input and output.

   @param async should the toplevel should be runned in a Web Worker ?
         (default [true])

   @param timeout default value for the optional parameter of the
          {!val:execute}, {!val:load} and {!val:reset} functions, use
          {!make_timeout_popup} to build one conveniently.

   @param flood_limit the number of bytes after which the
          [after_flood] function is called. The default limit is to
          8000 bytes, 0 means unlimited.

   @param flood a function called when an execution is printing
           too much on a named channel. If the function returns
           [false], the flooding continues.  Otherwise, nothing more
           until the end of the current evaluation. You can use
           {!make_flood_popup} to build such a callback.

   @param after_init a function that will be called whenever the
          toplevel is initialized or reseted.

   @param oldify by default the toplevel container content is emptied
          when the toplevel is reseted. When [~oldify:true] is used,
          the previous content is kept and wrapped in [div] with the
          class [old]. When [~oldify:false] is used, the toplevel
          content is untouched.
*)
val create:
  ?worker_js_file:string ->
  ?timeout_delay: float ->
  ?timeout_prompt:(t -> unit Lwt.t) ->
  ?flood_limit: int ->
  ?flood_prompt: (t -> string -> (unit -> int) -> bool Lwt.t) ->
  ?after_init:(t -> unit Lwt.t) ->
  ?input_sizing: Tryocaml_input.sizing ->
  ?on_resize:(unit -> unit) ->
  ?disable_input_hook:(t -> unit) ->
  ?enable_input_hook:(t -> unit) ->
  ?history:Tryocaml_history.history ->
  ?oldify:bool ->
  ?display_welcome: bool ->
  container:[`Div] Html5.elt ->
  unit -> t Lwt.t

(** Creates a thread that displays a popup over the toplevel container
    with a countdown and a button to augment it manually, and
    terminates after the countdown. *)
val make_timeout_popup:
  ?countdown: int ->
  ?refill_step: int ->
  ?on_show: (unit -> unit) ->
  unit ->
  t -> unit Lwt.t

(** Create a thread that displays a popup over the toplevel
    container displaying the flood amount in real time, and
    asking if the display should be hidden or not. *)
val make_flood_popup:
  ?on_show: (unit -> unit) ->
  unit ->
  (t -> string -> (unit -> int) -> bool Lwt.t)

(** Execute the content of the input [textarea]. This is equivalent to
    pressing [Enter] when the toplevel is focused.

    @param timeout  a Lwt thread that will interrupt the computation
           whenever it terminates.

    @returns Returns [Success true] whenever the code were correctly
             typechecked and its evaluation did not raised an
             exception not timeouted and [false] otherwise.

 *)
val execute: t -> ?timeout:(t -> unit Lwt.t) -> unit -> bool Lwt.t
val execute_phrase: t -> ?timeout:(t -> unit Lwt.t) -> string -> bool Lwt.t


(** Compile and load a given source code.

    @param timeout  a Lwt thread that will interrupt the computation
           whenever it terminates.

    @returns Returns [Success true] whenever the code were correctly
             typechecked and its evaluation did not raised an exception
             and [false] otherwise.

 *)
val load:
  t ->
  ?print_outcome:bool ->
  ?timeout:(t -> unit Lwt.t) ->
  ?message: string ->
  string -> bool Lwt.t

(** Parse and typecheck a given source code.

    @returns Returns [Success ()] if the code typechecks.

 *)
val check:
  t -> string -> unit Toploop_results.toplevel_result Lwt.t

(** Freezes the environment for future calls to {!check}. *)
val set_checking_environment:
  t -> unit Lwt.t

(** Empty the toplevel container content. *)
val clear: t -> unit


(** Reset the toplevel environment.

    @param oldify override the [oldify] parameter passed to {!val:create}.

    @param timeout  a Lwt thread that will interrupt the computation
           whenever it terminates.

 *)
val reset:
  t ->
  ?oldify:bool option ->
  ?timeout:(t -> unit Lwt.t) ->
  unit -> unit Lwt.t


(** Print a message in the toplevel standard output. This is equivalent
    to calling [Pervasives.print_string] in the toplevel session. *)
val print_string: t -> string -> unit

(** Print a message in the toplevel standard error output. This is
    equivalent to calling [Pervasives.prerr_string] in the toplevel
    session. *)
val prerr_string: t -> string -> unit

(** Print a block of HTML in the toplevel output. *)
val print_html: t -> string -> unit

(** scroll the view to show the last phrase. *)
val scroll: t -> unit

val set_timeout_prompt: t -> (t -> unit Lwt.t) -> unit
val set_flood_prompt: t -> (t -> Html5_types.nmtoken -> (unit -> int) -> bool Lwt.t) -> unit

val set_enable_input_hook: t -> (t -> unit) -> unit
val set_disable_input_hook: t -> (t -> unit) -> unit
