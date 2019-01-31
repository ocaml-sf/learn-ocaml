(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Toplevel input box. *)

(** A toplevel input box handle. *)
type input

(** Size parameters for the input box. *)
type sizing =
  { line_height : int (** The height of ta line in pixels. *) ;
    min_lines : int (** The minimum assigned height in terms of of lines. *) ;
    max_lines : int (** The maximum assigned height in terms of of lines. *) }

(** Use a given div as an input box.
    Inserts a textarea in it and gives it the class [toplevel-input].

    @param sizing
      See {!sizing}.
    @param history
     The history storage to use. If none, a new volatile one is created.
    @param on_resize
      A callback, called every time the height of the box is updated.
    @param execute
      The callback called whenever the [Enter] key is pressed, or the
      {!execute} function is called. *)
val setup :
  ?sizing: sizing ->
  ?history:Learnocaml_toplevel_history.history ->
  ?on_resize:(unit -> unit) ->
  execute: (string -> unit) ->
  container: [ `Div ] Tyxml_js.Html5.elt ->
  unit -> input

(** Disable the input box. *)
val disable : input -> unit

(** Enable the input box. *)
val enable : input -> unit

(** Updates the contents of the field.
    Discards its current contents. *)
val set : input -> string -> unit

(** Gives the content of the input box. *)
val get : input -> string

(** Simulates a hit on the [Enter] key *)
val execute : input -> unit

(** Simulates a hit on the [Up] key *)
val go_backward : input -> unit

(** Simulates a hit on the [Down] key *)
val go_forward : input -> unit

(** Sets focus to the text input field *)
val focus : input -> unit
