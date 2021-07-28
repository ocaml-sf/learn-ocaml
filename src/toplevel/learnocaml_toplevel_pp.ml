(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** [construct_image img] renders an image as a svg code string. This code is
    used to display the image in the toplevel, using the toplevel directive
    "#install_printer".

    The size isn't taken into account in this function because a pretty printer
    doesn't take one. *)
let construct_image i =
  let coeff = 1.0 in
  let b = Buffer.create 2048 in
  let size = Gg.Size2.v (coeff *. 100.) (coeff *. 100.) in
  let view = Gg.Box2.v Gg.P2.o (Gg.Size2.v coeff coeff) in
  let r = Vg.Vgr.create (Vgr_svg.target ()) (`Buffer b) in
  ignore (Vg.Vgr.render r (`Image (size, view, i)));
  ignore (Vg.Vgr.render r `End);
  Buffer.contents b

(* Prelude for pretty printers *)
let prelude_pp = "let pp_svg _ i = construct_image i |> print_svg;;"

(* List of pretty printers to deploy in toplevel *)
let pp_list = ["pp_svg"]
