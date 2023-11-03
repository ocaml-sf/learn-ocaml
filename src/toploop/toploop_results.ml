(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
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

let of_msg msg =
  msg.Location.loc,
  (msg.Location.txt Format.str_formatter; Format.flush_str_formatter ())
let to_msg (loc, txt) = { Location.loc; txt = Format.dprintf "%s" txt }

let of_report { Location.kind = _; main; sub } =
  of_msg main, List.map of_msg sub
let x_to_report kind (main, sub) =
  { Location.kind; main = to_msg main; sub = List.map to_msg sub }
let to_warning = x_to_report (Location.Report_warning "")
let to_error = x_to_report Location.Report_error

let to_report: 'a toplevel_result -> ('b, 'c) result  = function
  | Ok (x, warns) -> Stdlib.Ok (x, List.map to_warning warns)
  | Error (err, warns) -> Stdlib.Error (to_error err, List.map to_warning warns)
