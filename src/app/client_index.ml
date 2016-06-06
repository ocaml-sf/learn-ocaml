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

module StringMap = Map.Make (String)

type exercise_state =
  { solution : string ;
    grade : int (* \in [0, 100] *) option ;
    report : Report.report option }

type client_index =
  exercise_state StringMap.t

open Json_encoding

let map_enc enc =
  conv
    StringMap.bindings
    (List.fold_left (fun s (k,v) -> StringMap.add k v s) StringMap.empty)
    (assoc enc)

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
    (fun { grade ; solution ; report } -> (grade, solution, report))
    (fun (grade, solution, report) -> { grade ; solution ; report })
    (obj3
       (opt "grade" grade_enc)
       (req "solution" string)
       (opt "report" Report.report_enc))

let client_index_enc =
  map_enc exercise_state_enc
