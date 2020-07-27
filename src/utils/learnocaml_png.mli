(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Bigarray

(** [to_png_data data w h] generates a string containing the png image
    [data] as a string. The image has dimensions [w] * [h]. *)
val to_png_data:
  (int, int8_unsigned_elt, c_layout) Array1.t -> int -> int -> string

