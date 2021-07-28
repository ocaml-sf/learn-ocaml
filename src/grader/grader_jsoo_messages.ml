(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type to_worker = {exercise : Learnocaml_exercise.t; solution : string}

type from_worker =
  | Callback of string
  | Answer of Learnocaml_report.t * string * string * string

open Json_encoding

let to_worker_enc =
  conv
    (fun {solution; exercise} -> (solution, exercise))
    (fun (solution, exercise) -> {solution; exercise})
    (obj2 (req "solution" string) (req "exercise" Learnocaml_exercise.encoding))

let from_worker_enc =
  union
    [ case
        (obj4
           (req "report" Learnocaml_report.enc)
           (dft "stdout" string "") (dft "stderr" string "")
           (dft "outcomes" string ""))
        (function
          | Answer (rep, out, err, msgs) -> Some (rep, out, err, msgs)
          | Callback _ -> None)
        (fun (rep, out, err, msgs) -> Answer (rep, out, err, msgs))
    ; case string
        (function Answer _ -> None | Callback msg -> Some msg)
        (fun msg -> Callback msg) ]
