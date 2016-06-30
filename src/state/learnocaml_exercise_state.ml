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

type exercise_state =
  { solution : string ;
    grade : int (* \in [0, 100] *) option ;
    report : Learnocaml_report.report option ;
    mtime : float }

open Json_encoding

let exercise_state_enc =
  let grade_enc =
    conv
      (fun s -> s)
      (fun s ->
         if s < 0 || s > 100 then
           raise (Cannot_destruct ([], Failure "grade overflow"))
         else s)
      int in
  conv
    (fun { grade ; solution ; report ; mtime } ->
       (grade, solution, report, mtime))
    (fun (grade, solution, report, mtime) ->
       { grade ; solution ; report ; mtime })
    (obj4
       (opt "grade" grade_enc)
       (req "solution" string)
       (opt "report" Learnocaml_report.report_enc)
       (dft "mtime" float 0.))
