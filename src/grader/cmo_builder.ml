(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2021-2022  OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Lwt

let re_given = "^given_[0-9A-Za-z_]+$"

let check_given str =
  Str.string_match (Str.regexp re_given) str 0

let re_shortid_ascii = "^[0-9A-Za-z_-]+$"

let check_shortid_ascii id =
  Str.string_match (Str.regexp re_shortid_ascii) id 0

let given_of_shortid id =
  if not (check_shortid_ascii id) then
    Format.printf "Warning: Short id '%s' does not match '%s'."
      id re_shortid_ascii;
  Str.global_replace (Str.regexp "_") "_0" id
  |> Str.global_replace (Str.regexp "-") "_1"
  |> fun s -> "given_" ^ s

let shortid_of_given str =
  if not (check_given str) then
    failwith
      (Format.sprintf "Incorrect given prefix: '%s' does not match '%s'."
         str re_given);
  Str.replace_first (Str.regexp "^given_") "" str
  |> Str.global_replace (Str.regexp "_1") "-"
  |> Str.global_replace (Str.regexp "_0") "_"

let compile_given exo json_path =
  (* let subdir = "gen.learn-ocaml" in *)
  let dir, base = Filename.dirname json_path, Filename.basename json_path in
  let given =
    Str.replace_first (Str.regexp "\\.json$") "" base
    |> given_of_shortid in
  let output_prefix = Filename.concat dir given in
  let output_ml =  Filename.concat dir (given ^ ".ml") in
  let prelude = Learnocaml_exercise.(decipher File.prelude exo) in
  let prepare = Learnocaml_exercise.(decipher File.prepare exo) in
  let both = prelude ^ " ;;\n" ^ prepare in
  let write filename str =
    Lwt_io.with_file ~mode:Lwt_io.Output filename
      (fun oc -> Lwt_io.write oc str) in
  let compile source_file output_prefix =
    let orig = !Clflags.binary_annotations in
    Clflags.binary_annotations := true;
    Compile.implementation ~start_from:Clflags.Compiler_pass.Parsing
      ~source_file ~output_prefix;
    Clflags.binary_annotations := orig in
  (* minor detail: display the basename "given_exo.ml" instead of subdir/exo *)
  Format.printf "%-24s (build .cmo)@." given;
  Lwt_utils.mkdir_p dir >>= fun () ->
  write output_ml both >>= fun () ->
  Lwt.return (compile output_ml output_prefix) >>= fun () ->
  (* then overwrite to remove prepare.ml *)
  write output_ml prelude
