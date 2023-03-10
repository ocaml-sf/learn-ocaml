(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type loc = Location.t

type msg = Location.t * string

type warning = msg * msg list
type error = msg * msg list

type 'a toplevel_result =
  | Ok of 'a * warning list
  | Error of error * warning list

val of_report: Location.report -> warning

val to_error: error -> Location.report
val to_warning: warning -> Location.report

val to_report:
  'a toplevel_result ->
  ('a * Location.report list, Location.report * Location.report list) result
