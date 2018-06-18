(* Copyright (c) 2018 OCamlPro
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software. *)

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let read_translations f =
  let ic = open_in f in
  let t = Hashtbl.create 427 in
  let id = ref None in
  let in_id = ref false in
  try while true do
      let l = String.trim (input_line ic) in
      if String.length l = 0 then id := None
      else if l.[0] = '#' then ()
      else
        try Scanf.sscanf l "msgid %S" (fun s -> in_id := true; id := Some s)
        with Scanf.Scan_failure _ ->
        try Scanf.sscanf l "msgstr %S" (fun s ->
            match !id with
            | None ->
                Printf.ksprintf failwith
                  "Missing 'msgid' before 'msgstr' in %s" f
            | Some id ->
                if Hashtbl.mem t id then
                  Printf.ksprintf failwith
                    "Duplicate definition for msgid '%s' in %s" id f;
                in_id := false;
                Hashtbl.add t id s)
        with Scanf.Scan_failure _ ->
        try Scanf.sscanf l "%S" (fun s ->
            match !id with
            | None ->
                Printf.ksprintf failwith
                  "Missing 'msgid' before string in %s" f
            | Some sid ->
                if !in_id then id := Some (sid ^ s)
                else
                  let s0 = Hashtbl.find t sid in
                  Hashtbl.replace t sid (s0 ^ s))
        with Scanf.Scan_failure _ ->
          Printf.ksprintf failwith "Error in translation file %s at %S" f l
    done; assert false
  with End_of_file ->
    close_in ic;
    t

let translations_dir = "./translations"
let transl_file_suffix = ".po"
let dump_pot_file = Sys.getenv_opt "DUMP_POT" <> None

let all_ids = Hashtbl.create 23

let htbl_update ht key f create =
  try Hashtbl.replace ht key (f (Hashtbl.find ht key))
  with Not_found -> Hashtbl.add ht key (f (create ()))

let find_translation lang ht ~loc s =
  if dump_pot_file then
    htbl_update all_ids lang
      (fun t -> Hashtbl.add t s loc; t)
      (fun () -> Hashtbl.create 143);
  try Hashtbl.find ht s
  with Not_found ->
    Location.print_warning loc !Location.formatter_for_warnings @@
    Warnings.Preprocessor
      (Printf.sprintf "%s translation not found for %S" lang s);
    s

let dump_pot () =
  Hashtbl.iter (fun lang strs ->
      let file = Filename.concat translations_dir (lang ^ ".pot") in
      let oc = open_out_gen [Open_append; Open_creat] 0o644 file in
      let misses =
        Hashtbl.fold (fun str loc acc -> match acc with
            | (locs, s)::acc when s = str -> (loc::locs, str)::acc
            | acc -> ([loc], str)::acc)
          strs []
      in
      let misses =
        List.sort compare
          (List.map (fun (locs, s) -> List.sort compare locs, s) misses)
      in
      let fmt = Format.formatter_of_out_channel oc in
      List.iter (fun (locs, s) ->
          Format.pp_print_string fmt "#:";
          List.iter (fun l ->
              Format.pp_print_char fmt ' ';
              Location.print_compact fmt l)
            locs;
          Format.fprintf fmt "\nmsgid %S\n" s;
          Format.fprintf fmt "msgstr \"\"\n\n";
        ) misses;
      close_out oc
    )
    all_ids

let translations =
  (* default language should be first *)
  ("en", (fun ~loc:_ s -> s)) ::
  let langs = Hashtbl.create 7 in
  Array.iter (fun f ->
      if Filename.check_suffix f transl_file_suffix then
        Hashtbl.add langs
          (Filename.chop_suffix f transl_file_suffix)
          (read_translations (Filename.concat translations_dir f))
    )
    (Sys.readdir translations_dir);
  List.sort compare @@
  Hashtbl.fold (fun lang h acc ->
      (lang, find_translation lang h) :: acc) langs []

let get_lang_expr ~loc transl_expr =
  Exp.apply ~loc
    (Exp.ident { txt = Ldot (Lident "Ocplib_i18n", "s_"); loc })
    [Nolabel, transl_expr]

let transl_mapper _argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc =
            Pexp_extension ({ txt = "lang_ids_array"; loc }, _)} ->
          Exp.array ~loc
            (List.map (fun (lang, _) ->
                 Exp.constant ~loc (Pconst_string (lang,None)))
                translations)
      | { pexp_desc =
            Pexp_extension ({ txt = ("i"|"if" as tag); loc }, pstr)} ->
          (match pstr with
           | PStr [{
               pstr_desc =
                 Pstr_eval ({
                     pexp_loc  = loc;
                     pexp_desc = Pexp_constant (Pconst_string (s, _))
                   }, _)
             }] ->
               let is_format = tag = "if" in
               let translations =
                 List.map (fun (_lang, f) -> f ~loc s) translations
               in
               let translations_expr =
                 Exp.array ~loc
                   (List.map (fun s ->
                        let e = Exp.constant ~loc (Pconst_string (s,None)) in
                        if is_format then
                          Exp.apply ~loc
                            (Exp.ident { txt = Lident "format_of_string"; loc })
                            [Nolabel, e]
                        else e)
                       translations)
               in
               get_lang_expr ~loc translations_expr
           | _ ->
               raise (Location.Error (
                   Location.error ~loc "[%i] requires a constant string, e.g. [%i \"text\"]")))
      | x -> default_mapper.expr mapper x;
  }

let () = register "i18n" transl_mapper
let () =
  if Sys.getenv_opt "DUMP_POT" <> None then at_exit dump_pot
