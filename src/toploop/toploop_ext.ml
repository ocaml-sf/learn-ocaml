(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

include Toploop_results

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

let () =
  Location.warning_reporter :=
    (fun loc w ->
       match Warnings.report w with
       | `Inactive -> None
       | `Active { Warnings.id = _; message; is_error = _; sub_locs } ->
           let r = (loc, message), sub_locs in
           warnings := r :: !warnings;
           None)

let return_success (e: 'a) : 'a toplevel_result = Ok (e, !warnings)
let return_error e : 'a toplevel_result  = Error (e, !warnings)

(** Error handling *)

let error_of_exn exn =
  match Location.error_of_exn exn with
  | None | Some `Already_displayed ->
      let msg = match exn with
        | Failure msg -> msg
        | exn -> Printexc.to_string exn
      in
      let main = { Location.txt = (fun fmt -> Format.pp_print_text fmt msg);
                   loc = Location.none } in
      { Location.main; sub = []; kind = Location.Report_error }
  | Some (`Ok report) -> report

let return_exn exn = return_error (of_report (error_of_exn exn))

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
      return_exn exn

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
      return_exn exn

let parse_mod_string ?filename modname sig_code impl_code =
  let open Parsetree in
  let open Ast_helper in
  let str =
    let impl_lb = Lexing.from_string impl_code in
    init_loc impl_lb
      (match filename with
       | None -> String.uncapitalize_ascii modname ^ ".ml"
       | Some f -> f);
    Parse.implementation impl_lb in
  let m =
    match sig_code with
    | None -> (Mod.structure str)
    | Some sig_code ->
        let sig_lb = Lexing.from_string sig_code in
        init_loc sig_lb (String.uncapitalize_ascii modname ^ ".mli");
        let s = Parse.interface sig_lb in
        Mod.constraint_ (Mod.structure str) (Mty.signature s) in
  Ptop_def [ Str.module_ (Mb.mk (Location.mknoloc (Some modname)) m) ]

let use_mod_string
    ?filename
    ?(print_outcome  = true) ~ppf_answer ~modname ?sig_code
    impl_code =
  if String.capitalize_ascii modname <> modname then
    invalid_arg
      "Learnocaml_toplevel_toploop.use_mod_string: \
       the module name must start with a capital letter.";
  warnings := [];
  try
    let phr =
      Ppx.preprocess_phrase @@
      parse_mod_string ?filename modname sig_code impl_code in
    let res = Toploop.execute_phrase print_outcome ppf_answer phr in
    Format.pp_print_flush ppf_answer ();
    flush_all ();
    return_success res
  with exn ->
    flush_all ();
    return_exn exn

(* Extracted from the "execute" function in "ocaml/toplevel/toploop.ml" *)
let check_phrase env = function
  | Parsetree.Ptop_def sstr ->
      Typecore.reset_delayed_checks ();
      let (str, sg, sn, newenv) = Typemod.type_toplevel_phrase env sstr in
      let sg' = Typemod.Signature_names.simplify newenv sn sg in
      ignore (Includemod.signatures env ~mark:Includemod.Mark_positive sg sg');
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

