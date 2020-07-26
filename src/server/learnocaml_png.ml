(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Bigarray

let to_png_data data w h =
  let dim = Array1.dim data in
  let header = [
    "\n" ; "255" ; "\n" ;
    string_of_int h ; " " ; string_of_int w ; "\n" ;
    "P3" ]
  in
  let rec loop k acc =
    if k < dim then
      let r = data.{k} |> string_of_int in
      let g = data.{k+1} |> string_of_int in
      let b = data.{k+2} |> string_of_int in
      let px = r ^ " " ^ g ^ " " ^ b ^ " " in
      let acc = px :: acc in
      loop (k+3) acc
    else List.rev acc |> String.concat ""
  in loop 0 header
