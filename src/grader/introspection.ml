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

(** Introspection *)

let split s c =
  let rec loop i =
    match String.index_from s i c with
    | exception Not_found when i <> 0 ->
        if i = 0 then [] else [String.sub s i (String.length s - i)]
    | exception _ -> []
    | j -> String.sub s i (j - i) :: loop (j + 1) in
  loop 0

let parse_lid name =
  match split name '.' with
  | [] -> Longident.Lident name
  | id :: args ->
      List.fold_left
        (fun lid name -> Longident.Ldot (lid, name))
        (Longident.Lident id)
        args

type 'a value =
  | Absent
  | Present of 'a
  | Incompatible of string

let insert_in_env (type t) name (ty : t Ty.ty) (value : t) =
  if name = "" then invalid_arg "Learnocaml_toplevel_toploop.insert_in_env (1)";
  let ty =
    Typetexp.transl_type_scheme !Toploop.toplevel_env (Ty.obj ty) in
  Toploop.toplevel_env := begin
    if String.uncapitalize_ascii name = name then
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
      | Ttyp_package { pack_type; _ } ->
        Env.add_module
          (Ident.create name)
          pack_type
          !Toploop.toplevel_env
      | _ -> invalid_arg "Learnocaml_toplevel_toploop.insert_in_env (2)"
  end;
  Toploop.setvalue name (Obj.repr value)

let insert_mod_ast_in_env ~var_name impl_code =
  let init_loc lb filename =
    Location.input_name := filename;
    Location.input_lexbuf := Some lb;
    Location.init lb filename in
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
    Ptop_def [ Str.module_ (Mb.mk (Location.mknoloc modname) m) ] in
  let phr =
    Toploop_ext.Ppx.preprocess_phrase @@
    parse_mod_string (String.capitalize_ascii var_name) None impl_code in
  let open Parsetree in
  (match phr with
   | Ptop_def [ { pstr_desc =
                    Pstr_module { pmb_expr = { pmod_desc =
                                                 Pmod_structure s; _ }; _ }; _}]
   | Ptop_def [ { pstr_desc =
                    Pstr_module { pmb_expr = { pmod_desc =
                                                 Pmod_constraint ({ pmod_desc =
                                                                      Pmod_structure s; _ }, _); _ }; _ }; _}] ->
       let ty = Ty.repr (Ast_helper.(Typ.constr (Location.mknoloc (parse_lid "Parsetree.structure")) [])) in
       insert_in_env var_name (ty : Parsetree.structure Ty.ty) s
   | _ (* should not happen *) -> assert false)

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
      | Some { Location.msg; _ } -> Incompatible msg

let compatible_type nexp ngot =
  treat_lookup_errors @@ fun () ->
  let path_exp = Env.lookup_type nexp !Toploop.toplevel_env in
  let decl_exp = Env.find_type path_exp !Toploop.toplevel_env in
  let path_got = Env.lookup_type ngot !Toploop.toplevel_env in
  let decl_got = Env.find_type path_got !Toploop.toplevel_env in
  let texp = Ctype.newconstr path_exp (List.map (fun _ -> Ctype.newvar ()) decl_exp.Types.type_params) in
  let tgot = Ctype.newconstr path_got (List.map (fun _ -> Ctype.newvar ()) decl_got.Types.type_params) in
  Ctype.unify !Toploop.toplevel_env tgot texp ;
  Present ()

let get_value lid ty =
  treat_lookup_errors @@ fun () ->
  match Ty.obj ty, String.get (Longident.last lid) 0 with
  | { Parsetree.ptyp_desc = Parsetree.Ptyp_package (n, rews); _ }, 'A'.. 'Z' ->
      begin match Env.lookup_module ~load:false lid !Toploop.toplevel_env with
        | exception Not_found -> Absent
        | path ->
            let { Types.md_loc; _ } = Env.find_module path !Toploop.toplevel_env in
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
      let { Typedtree.ctyp_type = exp_type; _ } =
        Typetexp.transl_type_scheme !Toploop.toplevel_env (Ty.obj ty) in
      let path, { Types.val_type; _ } =
        Env.lookup_value lid !Toploop.toplevel_env in
      if Ctype.moregeneral !Toploop.toplevel_env true val_type exp_type then
        Present (Obj.obj @@ Toploop.eval_path !Toploop.toplevel_env path)
      else
        failwith (Format.asprintf "Wrong type %a." Printtyp.type_sch val_type)

let print_value ppf v ty =
  let { Typedtree.ctyp_type = ty; _ } =
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
  let { Typedtree.ctyp_type = ty; _ } =
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
          Exp.apply (Exp.ident lid) (List.map (fun arg -> Asttypes.Nolabel, phrase arg) tl)
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
    let path, { Types.val_type; _ } =
      Env.lookup_value (Longident.Lident lid) !Toploop.toplevel_env in
    let gty = Types.{ty with desc = Tarrow (Asttypes.Nolabel, Predef.type_unit, ty, Cok) } in
    if Ctype.moregeneral !Toploop.toplevel_env true val_type gty then
      (Obj.obj @@ Toploop.eval_path !Toploop.toplevel_env path)
    else (failwith "sampler has the wrong type !")
  else (failwith ("sampler could not be defined, " ^ Buffer.contents buf))

let register_callback name ty f =
  let unit =
    Ast_helper.(Typ.constr (Location.mknoloc (Longident.Lident "unit")) []) in
  let ty = Ty.curry ty (Ty.repr unit) in
  insert_in_env name ty f


let ref_lid =
  Location.mknoloc Longident.(Ldot(Lident "Pervasives", "ref"))

let create_ref name (ty: 'a Ty.ty) (v: 'a) =
  let ty = Ty.repr @@ Ast_helper.Typ.constr ref_lid [Ty.obj ty] in
  let r = ref v in
  insert_in_env name ty r;
  (fun () -> !r)

let setup = lazy (Ast_mapper.register "ppx_metaquot" Ppx_metaquot.expander)

let allow_introspection ~divert =

  Lazy.force setup ;

  let module Introspection = struct

    type 'a t = 'a value =
      | Absent
      | Present of 'a
      | Incompatible of string
    type 'a value = 'a t =
      | Absent
      | Present of 'a
      | Incompatible of string

    let get_value name ty =
      let lid = parse_lid name in
      get_value lid ty

    let compatible_type name_exp name_got =
      compatible_type
        (parse_lid name_exp)
        (parse_lid name_got)

    let print_value = print_value

    exception Excess

    let divert name ch =
      let buffer = Buffer.create 503 in
      let excess = ref false in
      let append s =
        if Buffer.length buffer + String.length s > 32_768 - 9 then
          excess := true
        else
          Buffer.add_string buffer s in
      let flush = divert name ch append in
      fun () ->
        flush () ;
        let res = Buffer.contents buffer in
        Buffer.clear buffer;
        let fail = !excess in
        excess := false ;
        if fail then raise Excess ;
        res

    let bad_stdout_cb () = invalid_arg "Introspection.release_stdout"
    let stdout_cb = ref bad_stdout_cb
    let grab_stdout () =
      if !stdout_cb != bad_stdout_cb then
        invalid_arg "Introspection.grab_stdout" ;
      stdout_cb := divert "stdout" stdout
    let release_stdout () =
      let res = !stdout_cb () in
      stdout_cb := bad_stdout_cb ;
      res

    let bad_stderr_cb () = invalid_arg "Introspection.release_stderr"
    let stderr_cb = ref bad_stderr_cb
    let grab_stderr () =
      if !stderr_cb != bad_stderr_cb then
        invalid_arg "Introspection.grab_stderr" ;
      stderr_cb := divert "stderr" stderr
    let release_stderr () =
      let res = !stderr_cb () in
      stderr_cb := bad_stderr_cb ;
      res

    let get_printer ty = fun ppf v -> print_value ppf v ty
    let get_sampler ty = sample_value ty

    let parse_lid name = parse_lid name

  end in

  insert_in_env
    "Introspection"
    [%ty: (module Introspection_intf.INTROSPECTION)]
    (module Introspection : Introspection_intf.INTROSPECTION)
