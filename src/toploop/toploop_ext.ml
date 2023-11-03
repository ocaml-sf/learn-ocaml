(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
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

let inject_sig name sign =
  Toploop.toplevel_env :=
    Env.add_module
      (Ident.create_persistent name)
      Types.Mp_present
      (Types.Mty_signature sign)
      !Toploop.toplevel_env

let load_cmi_from_string cmi_str =
  (* Cmi_format.input_cmi only supports reading from a channel *)
  let magic_len = String.length Config.cmi_magic_number in
  if String.length cmi_str < magic_len ||
     String.sub cmi_str 0 magic_len <> Config.cmi_magic_number then
    Printf.ksprintf failwith "Bad cmi file";
  let (name, sign) = Marshal.from_string cmi_str magic_len in
  (* we ignore crc and flags *)
  inject_sig name sign

let inject_global_hook: (Ident.t -> unit) ref = ref (fun _ -> ())

let set_inject_global_hook f = inject_global_hook := f

let inject_global name obj =
  let id = Ident.create_persistent name in
  let fake_buf = Misc.LongString.create 4 in
  let reloc = [Cmo_format.Reloc_setglobal id, 0] in
  Symtable.patch_object fake_buf reloc;
  (* we don't care about patching but this is the only entry point that allows us to register the global *)
  Symtable.check_global_initialized reloc;
  Symtable.update_global_table ();
  Symtable.assign_global_value id obj;
  !inject_global_hook id


(** Printing *)

(* Replacement for [Toploop.print_value] that doesn't segfault on yet
   unregistered extension constructors (needed for printing types defined in
   test.ml from within test.ml). *)
module Printer = Genprintval.Make(Obj)(struct
    type valu = Obj.t
    exception Error
    let eval_address = function
      | Env.Aident id ->
          if Ident.persistent id || Ident.global id then
            Symtable.get_global_value id
          else begin
            let name = Translmod.toplevel_name id in
            try Toploop.getvalue name
            with _ -> raise Error
          end
      | Env.Adot(_, _) ->
          (* in this case we bail out because this may refer to a
             yet-unregistered extension constructor within the current module.
             The printer has a reasonable fallback. *)
          raise Error
    let same_value v1 v2 = (v1 == v2)
  end)

let pending_installed_printers = ref []

(** Relies on the env (already loaded cmi) to get the correct type parameters
    for the [Printer] functions *)
let install_printer modname id tyname pr =
  let open Types in
  let inmodpath id =
    match String.split_on_char '.' modname with
    | md::r ->
        List.fold_left (fun acc id -> Path.Pdot (acc, id))
          (Path.Pident (Ident.create_persistent md)) (r @ [id])
    | [] ->
        Path.Pident (Ident.create_local id)
  in
  let printer_path = inmodpath id in
  let env = !Toploop.toplevel_env in
  let ( @-> ) a b = Ctype.newty (Tarrow (Asttypes.Nolabel, a, b, Cunknown)) in
  let gen_printer_type ty =
    let format_ty =
      let ( +. ) a b = Path.Pdot (a, b) in
      Path.Pident (Ident.create_persistent "Stdlib") +. "Format" +. "formatter"
    in
    (Ctype.newty (Tconstr (format_ty, [], ref Mnil))
     @-> ty
     @-> Predef.type_unit)
  in
  let ty_path1 = inmodpath tyname in
  match
    Env.find_value printer_path env,
    try ty_path1, Env.find_type ty_path1 env
    with Not_found -> Env.find_type_by_name (Longident.Lident tyname) env
  with
  | exception Not_found ->
      Format.printf
        "Warning: bad printer definition %s.print_%s. The type and printer \
         must be found in the cmi file (no mli file allowed).@."
        modname tyname
  | printer_desc, (ty_path, ty_decl) ->
      Ctype.begin_def();
      let ty_args = List.map (fun _ -> Ctype.newvar ()) ty_decl.type_params in
      let ty_target =
        Ctype.expand_head env
          (Ctype.newty (Tconstr (ty_path, ty_args, ref Mnil)))
      in
      let printer_ty_expected =
        List.fold_right (fun argty ty -> gen_printer_type argty @-> ty)
          ty_args
          (gen_printer_type ty_target)
      in
      (try
         Ctype.unify env
           printer_ty_expected
           (Ctype.instance printer_desc.val_type)
       with Ctype.Unify _ ->
         Format.printf
           "Warning: mismatching type for print function %s.print_%s.@;\
            The type must be@ @[<hov>%aformatter -> %a%s -> unit@]@."
           modname tyname
           (Format.pp_print_list
              (fun ppf -> Format.fprintf ppf "(formatter -> %a -> unit) ->@ "
                  (Printtyp.type_expr)))
           ty_args
           (fun ppf -> function
              | [] -> ()
              | [arg] -> Format.fprintf ppf "%a " Printtyp.type_expr arg
              | args ->
                  Format.fprintf ppf "(%a) "
                    (Format.pp_print_list
                       ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
                       Printtyp.type_expr)
                    args)
           ty_args
           tyname);
      Ctype.end_def ();
      Ctype.generalize printer_ty_expected;
      let register_as_path = inmodpath ("print_"^tyname) in
      let rec build_generic v = function
        | [] ->
            Genprintval.Zero
              (fun formatter repr -> Obj.obj v formatter (Obj.obj repr))
        | _ :: args ->
            Genprintval.Succ
              (fun fn -> build_generic ((Obj.obj v : _ -> Obj.t) fn) args)
      in
      (* Register for our custom 'Printer' as used by the graders *)
      let () =
        match ty_decl.type_params, ty_target.desc with
        | [], _ ->
            Printer.install_printer register_as_path ty_target
              (fun ppf repr -> Obj.magic pr ppf (Obj.obj repr))
        | _, (Tconstr (ty_path, args, _) | Tlink {desc = Tconstr (ty_path, args, _); _})
          when Ctype.all_distinct_vars env args ->
            Printer.install_generic_printer' register_as_path ty_path
              (build_generic (Obj.repr pr) ty_decl.type_params)
        | _, ty ->
            Format.printf
              "Warning: invalid printer for %a = %a: OCaml doesn't support \
               printers for types with partially instanciated variables. \
               Define a generic printer and a printer for the type of your \
               variable instead."
              Printtyp.path ty_path
              Printtyp.type_expr (Ctype.newty ty)
      in
      (* Register for the toplevel built-in printer (the API doesn't allow us to
         override it). Attempting to use the printer registered this way before
         the module is fully loaded would risk crashes (e.g. on extensible
         variants) *)
      let rec path_to_longident = function
        | Path.Pdot (p, s) -> Longident.Ldot (path_to_longident p, s)
        | Path.Pident i -> Longident.Lident (Ident.name i)
        | Path.Papply _ -> assert false
      in
      pending_installed_printers :=
        path_to_longident printer_path :: !pending_installed_printers

let register_pending_printers () =
  List.iter (Topdirs.dir_install_printer Format.std_formatter)
    (List.rev !pending_installed_printers);
  pending_installed_printers := []
