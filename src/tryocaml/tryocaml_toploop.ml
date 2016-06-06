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

type error = {
  msg: string;
  locs: Location.t list;
  if_highlight: string;
}

type warning = error

type 'a result =
  | Success of 'a * warning list
  | Error of error * warning list

let (>>=) = Lwt.bind

let warnings = ref []

#if ocaml_full_version >= (4,02,2)
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
         warnings := { msg; locs = [loc]; if_highlight } :: !warnings
       end)
#endif

(* Workaround Marshal bug triggered by includemod.ml:607 *)
let () = Clflags.error_size := 0

(* Disable inlining of JSOO which may blow the JS stack *)
let () = Compiler.Option.Optim.disable "inline"

let return_success e = Lwt.return (Success (e, !warnings))
let return_error e = Lwt.return (Error (e, !warnings))
let return_unit_success = return_success ()

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
  loc :: locs

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

let () =
  Lwt.async_exception_hook :=
    (fun exn ->
       Format.eprintf "Internal error: exception during Lwt.async:\n%s@."
         (error_of_exn exn).msg)

let return_exn exn = return_error (error_of_exn exn)

(** Execution helpers *)

let refill_lexbuf s p ppf buffer len =
  if !p = String.length s
  then 0
  else
    let len',nl =
      try String.index_from s !p '\n' - !p + 1,false
      with _ -> String.length s - !p,true in
    let len'' = min len len' in
    String.blit s !p buffer 0 len'';
    Format.fprintf ppf "%s" (Bytes.sub_string buffer 0 len'');
    if nl then Format.pp_print_newline ppf ();
    Format.pp_print_flush ppf ();
    p:=!p + len'';
    len''

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

let init_loc lb filename =
  Location.input_name := filename;
  Location.input_lexbuf := Some lb;
  Location.init lb filename

let execute ?ppf_code ~ppf_answer print_outcome code =
  let code = normalize code in
  let lb =
    match ppf_code with
    | Some ppf_code ->
      Lexing.from_function (refill_lexbuf code (ref 0) ppf_code)
    | None -> Lexing.from_string code in
  init_loc lb "//toplevel//";
  warnings := [];
  let rec loop () =
    let phr = !Toploop.parse_toplevel_phrase lb in
    let phr = Ppx.preprocess_phrase phr in
    let success = Toploop.execute_phrase print_outcome ppf_answer phr in
    Format.pp_print_flush ppf_answer ();
    Lwt_js.yield () >>= fun () ->
    if success then loop () else return_success false in
  Lwt.catch loop
    (function
      | End_of_file -> return_success true
      | exn ->
        flush_all ();
        return_error (error_of_exn exn)) >>= fun result ->
  flush_all ();
  Lwt.return result

let use_string ?(filename = "//toplevel//") ~ppf_answer print_outcome code =
  let lb = Lexing.from_string code in
  init_loc lb filename;
  warnings := [];
  Lwt.catch
    (fun () ->
       Lwt_list.iter_s
         (fun phr ->
            if not (Toploop.execute_phrase print_outcome ppf_answer phr) then
              Lwt.fail Exit
            else begin
              Format.pp_print_flush ppf_answer ();
              Lwt_js.yield ()
            end)
         (List.map Ppx.preprocess_phrase (!Toploop.parse_use_file lb))
       >>= fun () -> return_success true)
    (function
      | Exit ->
        flush_all ();
        Format.pp_print_flush ppf_answer ();
        return_success false
      | exn ->
        return_error (error_of_exn exn))

let parse_mod_string modname sig_code impl_code =
  let open Parsetree in
  let open Ast_helper in
  let str =
    let impl_lb = Lexing.from_string impl_code in
    init_loc impl_lb (String.uncapitalize modname ^ ".ml");
    Parse.implementation impl_lb in
  let m =
    match sig_code with
    | None -> (Mod.structure str)
    | Some sig_code ->
        let sig_lb = Lexing.from_string sig_code in
        init_loc sig_lb (String.uncapitalize modname ^ ".mli");
        let s = Parse.interface sig_lb in
        Mod.constraint_ (Mod.structure str) (Mty.signature s) in
  Ptop_def [ Str.module_ (Mb.mk (Location.mknoloc modname) m) ]

(** Introspection *)

let parse_lid name =
  match Regexp.(split (regexp "\\.") name) with
  | [] -> invalid_arg "Introspection.parse_lid"
  | id :: args ->
      List.fold_left
        (fun lid name -> Longident.Ldot (lid, name))
        (Longident.Lident id)
        args

let insert_in_env (type t) name (ty : t Ty.ty) (value : t) =
  if name = "" then invalid_arg "Tryocaml_toploop.insert_in_env (1)";
  let ty =
    Typetexp.transl_type_scheme !Toploop.toplevel_env (Ty.obj ty) in
  Toploop.toplevel_env := begin
    if String.uncapitalize name = name then
      Env.add_value
        (Ident.create name)
        { Types.
          val_type = ty.Typedtree.ctyp_type;
          val_kind = Types.Val_reg;
          val_attributes = [];
          val_loc = Location.none }
        !Toploop.toplevel_env
    else
      let open Typedtree in
      match ty.ctyp_desc with
      | Ttyp_package { pack_type } ->
        Env.add_module
          (Ident.create name)
          pack_type
          !Toploop.toplevel_env
      | _ -> invalid_arg "Tryocaml_toploop.insert_in_env (2)"
  end;
  Toploop.setvalue name (Obj.repr value)

let use_mod_string ~ppf_answer print_outcome modname ?sig_code impl_code =
  if String.capitalize modname <> modname then
    invalid_arg
      "Tryocaml_toploop.use_mod_string: \
       the module name must start with a capital letter.";
  warnings := [];
  Lwt.catch
    (fun () ->
       let phr =
         Ppx.preprocess_phrase @@
         parse_mod_string modname sig_code impl_code in
       let res = Toploop.execute_phrase print_outcome ppf_answer phr in
       Format.pp_print_flush ppf_answer ();
       Lwt_js.yield () >>= fun () ->
       flush_all ();
       return_success res)
    (function exn -> return_error (error_of_exn exn))

let insert_mod_ast_in_env ~var_name impl_code =
  warnings := [];
  Lwt.catch
    (fun () ->
       let phr =
         Ppx.preprocess_phrase @@
         parse_mod_string (String.capitalize var_name) None impl_code in
       let open Parsetree in
       (match phr with
        | Ptop_def [ { pstr_desc =
                         Pstr_module { pmb_expr = { pmod_desc =
                                                      Pmod_structure s } }}]
        | Ptop_def [ { pstr_desc =
                         Pstr_module { pmb_expr = { pmod_desc =
                                                      Pmod_constraint ({ pmod_desc =
                                                                           Pmod_structure s }, _) } }}] ->
            let ty = Ty.repr (Ast_helper.(Typ.constr (Location.mknoloc (parse_lid "Parsetree.structure")) [])) in
            insert_in_env var_name (ty : Parsetree.structure Ty.ty) s
        | _ (* should not happen *) -> ()) ;
       return_success ())
    (function exn -> return_error (error_of_exn exn))

type 'a value =
  | Absent
  | Present of 'a
  | Incompatible of string

let treat_lookup_errors fn = match fn () with
  | result -> result
  | exception Not_found ->
      Absent
  | exception Failure msg ->
      Incompatible msg
  | exception Ctype.Unify args ->
      Incompatible
        (Format.asprintf "%a@."
           (Typetexp.report_error !Toploop.toplevel_env)
           (Typetexp.Type_mismatch args))
  | exception exn ->
      match Location.error_of_exn exn with
      | None -> Incompatible (Format.asprintf "%a@." Toploop.print_untyped_exception (Obj.repr exn))
      | Some { Location.msg } -> Incompatible msg

let compatible_type nexp ngot =
  treat_lookup_errors @@ fun () ->
  let path_exp, decl_exp = Env.lookup_type nexp !Toploop.toplevel_env in
  let path_got, decl_got = Env.lookup_type ngot !Toploop.toplevel_env in
  let texp = Ctype.newconstr path_exp (List.map (fun _ -> Ctype.newvar ()) decl_exp.Types.type_params) in
  let tgot = Ctype.newconstr path_got (List.map (fun _ -> Ctype.newvar ()) decl_got.Types.type_params) in
  Ctype.unify !Toploop.toplevel_env tgot texp ;
  Present ()

let get_value lid ty =
  treat_lookup_errors @@ fun () ->
  match Ty.obj ty, String.get (Longident.last lid) 0 with
  | { Parsetree.ptyp_desc = Parsetree.Ptyp_package (n, rews) }, 'A'.. 'Z' ->
      begin match Env.lookup_module ~load:false lid !Toploop.toplevel_env with
        | exception Not_found -> Absent
        | path ->
            let { Types.md_type ; md_loc } = Env.find_module path !Toploop.toplevel_env in
            let phrase =
              let open Ast_helper in
              with_default_loc md_loc @@ fun () ->
              let pack_expr =
                Exp.constraint_
                  (Exp.pack (Mod.ident (Location.mkloc lid md_loc)))
                  (Typ.package n rews) in
              Parsetree.Ptop_def
                [Str.value Asttypes.Nonrecursive
                   [Vb.mk (Pat.var (Location.mkloc "%fake%" md_loc)) pack_expr ]] in
            let buf = Buffer.create 300 in
            let ppf = Format.formatter_of_buffer buf in
            if Toploop.execute_phrase false ppf phrase then
              let fake_path, _ = Env.lookup_value (Longident.Lident "%fake%") !Toploop.toplevel_env in
              Present (Obj.obj @@ Toploop.eval_path !Toploop.toplevel_env fake_path)
            else
              let msg = Format.fprintf ppf "@." ; Buffer.contents buf in
              failwith msg
      end
  | _ ->
      let { Typedtree.ctyp_type = exp_type } =
        Typetexp.transl_type_scheme !Toploop.toplevel_env (Ty.obj ty) in
      let path, { Types.val_type } =
        Env.lookup_value lid !Toploop.toplevel_env in
      if Ctype.moregeneral !Toploop.toplevel_env true val_type exp_type then
        Present (Obj.obj @@ Toploop.eval_path !Toploop.toplevel_env path)
      else
        failwith (Format.asprintf "Wrong type %a." Printtyp.type_sch val_type)

let print_value ppf v ty =
  let { Typedtree.ctyp_type = ty } =
    Typetexp.transl_type_scheme !Toploop.toplevel_env (Ty.obj ty) in
  let needs_parentheses =
    let state = ref `Start in
    let tmp_ppf =
      Format.make_formatter
        (fun s ofs len ->
           if len = 0 then () else
             match !state, String.get s ofs with
             | `Decided _, _ -> ()
             | `Start, ('(' | '{' | '[' | '<' | '\'' | '"') ->
                 state := `Decided false ;
                 raise Exit
             | (`Start | `Undecided), _ ->
                 state := `Undecided ;
                 for i = ofs to ofs + len - 1 do
                   match String.get s i with
                   | ' ' | '\n' | '\r' | '\t' ->
                       state := `Decided true ;
                       raise Exit
                   | _ ->  ()
                 done)
        (fun () -> ()) in
    begin try
        Toploop.print_value !Toploop.toplevel_env (Obj.repr v) tmp_ppf ty ;
        Format.pp_print_flush tmp_ppf ()
      with Exit -> () end ;
    match !state with `Start | `Decided false | `Undecided -> false | `Decided true -> true in
  if needs_parentheses then begin
    Format.fprintf ppf "@[<hv 1>(" ;
    Toploop.print_value !Toploop.toplevel_env (Obj.repr v) ppf ty ;
    Format.fprintf ppf ")@]"
  end else begin
    Format.fprintf ppf "@[<hv 0>" ;
    Toploop.print_value !Toploop.toplevel_env (Obj.repr v) ppf ty ;
    Format.fprintf ppf "@]"
  end

let sample_value ty =
  let { Typedtree.ctyp_type = ty } =
    Typetexp.transl_type_scheme !Toploop.toplevel_env (Ty.obj ty) in
  let lid = Format.asprintf "sample_%04X" (Random.int 0xFFFF) in
  let phrase =
    let open Asttypes in
    let open Types in
    let open Ast_helper in
    let rec phrase ty = match ty.desc with
      | Tconstr (path, [], _) ->
          let lid = (Location.mknoloc (Longident.Lident ("sample_" ^ Path.name path))) in
          Exp.ident lid
      | Tconstr (path, tl, _) ->
          let lid = (Location.mknoloc (Longident.Lident ("sample_" ^ Path.name path))) in
          Exp.apply (Exp.ident lid) (List.map (fun arg -> "", phrase arg) tl)
      | _ -> failwith "unsamplable type"
    in
    let lid = Location.mknoloc lid in
    Parsetree.Ptop_def
      [Str.value Nonrecursive
         [Vb.mk (Pat.var lid) (phrase ty)]]
  in
  let buf = Buffer.create 100 in
  let ppf = Format.formatter_of_buffer buf in
  if Toploop.execute_phrase false ppf phrase then
    let path, { Types.val_type } =
      Env.lookup_value (Longident.Lident lid) !Toploop.toplevel_env in
    let gty = Types.{ty with desc = Tarrow ("", Predef.type_unit, ty, Cok) } in
    if Ctype.moregeneral !Toploop.toplevel_env true val_type gty then
      (Obj.obj @@ Toploop.eval_path !Toploop.toplevel_env path)
    else (failwith "sampler has the wrong type !")
  else (failwith ("sampler could not be defined, " ^ Buffer.contents buf))

let register_callback name ty f =
  let unit =
    Ast_helper.(Typ.constr (Location.mknoloc (Longident.Lident "unit")) []) in
  let ty = Ty.curry ty (Ty.repr unit) in
  insert_in_env name ty f

let allow_introspection () =

  let module Introspection = struct

    type 'a t = 'a value =
      | Absent
      | Present of 'a
      | Incompatible of string
    type 'a value = 'a t =
      | Absent
      | Present of 'a
      | Incompatible of string

    let parse_lid name = parse_lid name

    let get_value name ty =
      let lid = parse_lid name in
      get_value lid ty

    let compatible_type name_exp name_got =
      compatible_type
        (parse_lid name_exp)
        (parse_lid name_got)

    let print_value = print_value

    let create ch =
      let buffer = Buffer.create 512 in
      let excess = ref false in
      Sys_js.set_channel_flusher ch (fun s ->
          if Buffer.length buffer + String.length s > 32_768 then
            excess := true
          else
            Buffer.add_string buffer s);
      fun () ->
        flush ch;
        let res = Buffer.contents buffer in
        Buffer.clear buffer;
        let fail = !excess in
        excess := false ;
        if fail then failwith "EXCESS" ;
        res

    let flush_stdout = create stdout
    let flush_stderr = create stderr

    let get_printer ty = fun ppf v -> print_value ppf v ty
    let get_sampler ty = sample_value ty
  end in

  insert_in_env
    "Introspection"
    (* [%ty: (module Tryocaml_messages.INTROSPECTION)] *)
    (Ty.repr {
        Parsetree.ptyp_desc =
          (Parsetree.Ptyp_package
             ({
               Asttypes.txt =
                 (Longident.Ldot
                    ((Longident.Lident "Tryocaml_messages"), "INTROSPECTION"));
               Asttypes.loc = (Pervasives.(!) Ast_helper.default_loc)
             }, []));
        Parsetree.ptyp_loc = (Pervasives.(!) Ast_helper.default_loc);
        Parsetree.ptyp_attributes = []
      })
    (module Introspection : Tryocaml_messages.INTROSPECTION)

let ref_lid =
  Location.mknoloc Longident.(Ldot(Lident "Pervasives", "ref"))

let create_ref name ty v =
  let ty = Ty.repr @@ Ast_helper.Typ.constr ref_lid [Ty.obj ty] in
  let r = ref v in
  insert_in_env name ty r;
  (fun () -> !r)

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
        (!Toploop.parse_use_file lb) in
    if setenv then Toploop.toplevel_env := env;
    return_success ()
  with
  | End_of_file -> return_success ()
  | exn -> return_exn exn
