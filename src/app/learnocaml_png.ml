(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Bigarray

let fill big_data data =
  let dim = Array1.dim big_data in
  let rec loop big_count data_count =
    if big_count < dim then
      begin
        Dom_html.pixel_set data data_count big_data.{big_count};
        Dom_html.pixel_set data (data_count+1) big_data.{big_count+1};
        Dom_html.pixel_set data (data_count+2) big_data.{big_count+2};
        Dom_html.pixel_set data (data_count+3) 255;
        loop (big_count+3) (data_count+4)
      end
    else ()
  in loop 0 0

let to_png_data big_data w h =
  let canvas = Dom_html.createCanvas Dom_html.document in
  canvas ##. width := w;
  canvas ##. height := h;
  let context = canvas ## getContext Dom_html._2d_ in
  let image_data = context ## createImageData w h in
  let data = image_data ##.data in
  fill big_data data;
  context ## putImageData image_data 0. 0.;
  canvas##toDataURL_type (Js.string "image/png")
  |> Js.to_string
