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

type 'a toplevel_result = 'a Toploop_results.toplevel_result =
  (* ('a * warning list, error * warning list) result = *)
  | Ok of 'a * warning list
  | Error of error * warning list

and error = Toploop_results.error =
  { msg: string;
    locs: loc list;
    if_highlight: string; }

and warning = error

and loc = Toploop_results.loc  = {
  loc_start: int * int;
  loc_end: int * int;
}

module Ppx = struct

  let ppx_rewriters = ref []

  let () =
    Ast_mapper.register_function :=
      (fun _ f -> ppx_rewriters := f :: !ppx_rewriters)

  let preprocess_structure str =
    let open Ast_mapper in
    List.fold_right
      (fun ppx_rewriter str ->
         let mapper = ppx_rewriter [] in
         mapper.structure mapper str)
      !ppx_rewriters
      str

  let preprocess_signature str =
    let open Ast_mapper in
    List.fold_right
      (fun ppx_rewriter str ->
         let mapper = ppx_rewriter [] in
         mapper.signature mapper str)
      !ppx_rewriters
      str

  let preprocess_phrase phrase =
    let open Parsetree in
    match phrase with
    | Ptop_def str -> Ptop_def (preprocess_structure str)
    | Ptop_dir _ as x -> x

end

let warnings = ref []

let convert_loc loc =
  let _file1,line1,col1 = Location.get_pos_info (loc.Location.loc_start) in
  let _file2,line2,col2 = Location.get_pos_info (loc.Location.loc_end) in
  { loc_start = (line1, col1) ; loc_end = (line2, col2) }

let () =
  Location.warning_printer :=
    (fun loc _fmt w ->
       if Warnings.is_active w then begin
         let buf = Buffer.create 503 in
         let ppf = Format.formatter_of_buffer buf in
         Location.print ppf loc;
         Format.fprintf ppf "Warning %a@." Warnings.print w;
         let msg = Buffer.contents buf in
         Buffer.reset buf;
         Format.fprintf ppf "Warning %a@." Warnings.print w;
         let if_highlight = Buffer.contents buf in
         let loc = convert_loc loc in
         warnings := { msg; locs = [loc]; if_highlight } :: !warnings
       end)

let return_success (e: 'a) : 'a toplevel_result = Ok (e, !warnings)
let return_error e : 'a toplevel_result  = Error (e, !warnings)
(* let return_unit_success = return_success () *)

(** Error handling *)
let dummy_ppf = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

let rec report_error_rec hg_ppf ppf {Location.loc; msg; sub; if_highlight} =
  Location.print ppf loc;
  Format.pp_print_string ppf msg;
  let hg_ppf =
    if if_highlight <> "" then
      (Format.pp_print_string hg_ppf if_highlight; dummy_ppf)
    else
      (Format.pp_print_string hg_ppf msg; hg_ppf) in
  let locs =
    List.concat @@
    List.map
      (fun err ->
         Format.pp_force_newline ppf ();
         Format.pp_open_box ppf 2;
         let locs = report_error_rec hg_ppf ppf err in
         Format.pp_close_box ppf ();
         locs)
      sub in
  convert_loc loc :: locs

let report_error err =
  let buf = Buffer.create 503 in
  let ppf = Format.formatter_of_buffer buf in
  let hg_buf = Buffer.create 503 in
  let hg_ppf = Format.formatter_of_buffer hg_buf in
  let locs = report_error_rec hg_ppf ppf err in
  Format.pp_print_flush ppf ();
  Format.pp_print_flush hg_ppf ();
  let msg = Buffer.contents buf in
  let if_highlight = Buffer.contents hg_buf in
  { msg; locs; if_highlight; }

let error_of_exn exn =
  match Location.error_of_exn exn with
  | None ->
      let msg = Printexc.to_string exn in
      { msg; locs = []; if_highlight = msg }
  | Some error -> report_error error

let return_exn exn = return_error (error_of_exn exn)

(** Execution helpers *)

let trim_end s =
  let ws c = c = ' ' || c = '\t' || c = '\n' in
  let len = String.length s in
  let stop = ref (len - 1) in
  while !stop > 0 && (ws s.[!stop])
  do decr stop done;
  String.sub s 0 (!stop + 1)

let normalize code =
  let content = trim_end code in
  let len = String.length content in
  if content = "" then
    content
  else if (len > 2
           && content.[len - 2] = ';'
           && content.[len - 1] = ';') then
    content ^ "\n"
  else
    content ^ " ;;\n"

let refill_lexbuf src ppf =
  let i = ref 0 in
  let max_i = String.length src in
  fun buf len ->
    if max_i <= !i then
      0
    else
      let (len, nl) =
        try min len (String.index_from src !i '\n' - !i + 1), false
        with Not_found | Invalid_argument _ ->
          min len (max_i - !i), true in
      String.blit src !i buf 0 len ;
      Format.pp_print_string ppf (Bytes.sub_string buf 0 len) ;
      if nl then Format.pp_print_newline ppf () ;
      Format.pp_print_flush ppf () ;
      i := !i + len ;
      len

let init_loc lb filename =
  Location.input_name := filename;
  Location.input_lexbuf := Some lb;
  Location.init lb filename

(** *)

let execute ?ppf_code ?(print_outcome  = true) ~ppf_answer code =
  let code = normalize code in
  let lb =
    match ppf_code with
    | Some ppf_code -> Lexing.from_function (refill_lexbuf code ppf_code)
    | None -> Lexing.from_string code in
  init_loc lb "//toplevel//";
  warnings := [];
  let rec loop () =
    let phr = !Toploop.parse_toplevel_phrase lb in
    let phr = Ppx.preprocess_phrase phr in
    let success = Toploop.execute_phrase print_outcome ppf_answer phr in
    Format.pp_print_flush ppf_answer ();
    if success then loop () else return_success false in
  try let res = loop () in flush_all () ; res
  with
  | End_of_file ->
      flush_all ();
      return_success true
  | exn ->
      flush_all ();
      return_error (error_of_exn exn)

let use_string
    ?(filename = "//toplevel//") ?(print_outcome  = true) ~ppf_answer code =
  let lb = Lexing.from_string code in
  init_loc lb filename;
  warnings := [];
  try
    List.iter
      (fun phr ->
         if not (Toploop.execute_phrase print_outcome ppf_answer phr) then
           raise Exit
         else
           Format.pp_print_flush ppf_answer ())
      (List.map Ppx.preprocess_phrase (!Toploop.parse_use_file lb)) ;
    flush_all ();
    return_success true
  with
  | Exit ->
      flush_all ();
      Format.pp_print_flush ppf_answer ();
      return_success false
  | exn ->
      flush_all ();
      return_error (error_of_exn exn)

let parse_mod_string modname sig_code impl_code =
  let open Parsetree in
  let open Ast_helper in
  let str =
    let impl_lb = Lexing.from_string impl_code in
    init_loc impl_lb (String.uncapitalize_ascii modname ^ ".ml");
    Parse.implementation impl_lb in
  let m =
    match sig_code with
    | None -> (Mod.structure str)
    | Some sig_code ->
        let sig_lb = Lexing.from_string sig_code in
        init_loc sig_lb (String.uncapitalize_ascii modname ^ ".mli");
        let s = Parse.interface sig_lb in
        Mod.constraint_ (Mod.structure str) (Mty.signature s) in
  Ptop_def [ Str.module_ (Mb.mk (Location.mknoloc modname) m) ]

let use_mod_string
    ?(print_outcome  = true) ~ppf_answer ~modname ?sig_code impl_code =
  if String.capitalize_ascii modname <> modname then
    invalid_arg
      "Learnocaml_toplevel_toploop.use_mod_string: \
       the module name must start with a capital letter.";
  warnings := [];
  try
    let phr =
      Ppx.preprocess_phrase @@
      parse_mod_string modname sig_code impl_code in
    let res = Toploop.execute_phrase print_outcome ppf_answer phr in
    Format.pp_print_flush ppf_answer ();
    flush_all ();
    return_success res
  with exn ->
    flush_all ();
    return_error (error_of_exn exn)

(* Extracted from the "execute" function in "ocaml/toplevel/toploop.ml" *)
let check_phrase env = function
  | Parsetree.Ptop_def sstr ->
      Typecore.reset_delayed_checks ();
      let (str, sg, newenv) = Typemod.type_toplevel_phrase env sstr in
      let sg' = Typemod.simplify_signature sg in
      ignore (Includemod.signatures env sg sg');
      Typecore.force_delayed_checks ();
      let _lam = Translmod.transl_toplevel_definition str in
      Warnings.check_fatal ();
      newenv
  | Parsetree.Ptop_dir _ -> env

let check ?(setenv = false) code =
  let lb = Lexing.from_string code in
  init_loc lb "//toplevel//";
  warnings := [];
  try
    let env =
      List.fold_left
        check_phrase
        !Toploop.toplevel_env
        (List.map
           Ppx.preprocess_phrase
           (!Toploop.parse_use_file lb)) in
    if setenv then Toploop.toplevel_env := env;
    return_success ()
  with
  | End_of_file -> return_success ()
  | exn -> return_exn exn

