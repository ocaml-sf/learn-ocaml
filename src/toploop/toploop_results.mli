(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type 'a toplevel_result =
  (* ('a * warning list, error * warning list) result = *)
  | Ok of 'a * warning list
  | Error of error * warning list

and error = {msg : string; locs : loc list; if_highlight : string}

and warning = error

and loc = {loc_start : int * int; loc_end : int * int}
