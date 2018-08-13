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

type to_worker =
  { exercise : Learnocaml_exercise.t ;
    solution : string }
type from_worker =
  | Callback of string
  | Answer of Learnocaml_report.t * string * string * string

open Json_encoding

let to_worker_enc =
  conv
    (fun { solution ; exercise } -> (solution, exercise))
    (fun (solution, exercise) -> { solution ; exercise })
    (obj2
       (req "solution" string)
       (req "exercise" Learnocaml_exercise.encoding))

let from_worker_enc =
  union
    [ case
        (obj4
           (req "report" Learnocaml_report.enc)
           (dft "stdout" string "")
           (dft "stderr" string "")
           (dft "outcomes" string ""))
        (function
          | Answer (rep, out, err, msgs) -> Some (rep, out, err, msgs)
          | Callback _ -> None)
        (fun (rep, out, err, msgs) -> Answer (rep, out, err, msgs)) ;
      case string
        (function
          | Answer _ -> None
          | Callback msg -> Some msg)
        (fun msg -> Callback msg) ]
