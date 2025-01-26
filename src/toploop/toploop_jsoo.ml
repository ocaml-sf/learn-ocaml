(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml

module Jsoo_compiler_dynlink () = struct
  (* XXX Copy-pasted from js_of_ocaml_compiler_dynlink.ml (at 5.8.2-27-gccdb2ac69b), because we need to delay initialization *)

  open Js_of_ocaml_compiler.Stdlib
  open Js_of_ocaml_compiler
  module J = Jsoo_runtime.Js

  type bytecode_sections =
    { symb : Ocaml_compiler.Symtable.GlobalMap.t
    ; crcs : (string * Digest.t option) list
    ; prim : string list
    ; dlpt : string list
    }
  [@@ocaml.warning "-unused-field"]

  external get_bytecode_sections : unit -> bytecode_sections = "jsoo_get_bytecode_sections"

  let normalize_bytecode code =
    match Ocaml_version.compare Ocaml_version.current [ 5; 2 ] < 0 with
    | true -> code
    | false ->
        (* starting with ocaml 5.2, The toplevel no longer append [RETURN 1] *)
        let { Instr.opcode; _ } = Instr.find Instr.RETURN in
        let len = String.length code in
        let b = Bytes.create (len + 8) in
        Bytes.blit_string ~src:code ~src_pos:0 ~dst:b ~dst_pos:0 ~len;
        Bytes.set_int32_le b len (Int32.of_int opcode);
        Bytes.set_int32_le b (len + 4) 1l;
        Bytes.to_string b

  let init () = (* <Learn-ocaml patched/> *)
    let global = J.pure_js_expr "globalThis" in
    Config.Flag.set "use-js-string" (Jsoo_runtime.Sys.Config.use_js_string ());
    Config.Flag.set "effects" (Jsoo_runtime.Sys.Config.effects ());
    (* <Learn-ocaml patched>  -- @LG: are these still needed ? *)
    (* Workaround Marshal bug triggered by includemod.ml:607 *)
    Clflags.error_size := 0 ;
    (* Disable inlining of JSOO which may blow the JS stack *)
    Config.Flag.disable "inline" ;
    Topdirs.dir_directory "/cmis";
    (* </Learn-ocaml patched> *)
    (* this needs to stay synchronized with toplevel.js *)
    let toplevel_compile (s : string) (debug : Instruct.debug_event list array) :
      unit -> J.t =
      let s = normalize_bytecode s in
      let prims = Array.of_list (Ocaml_compiler.Symtable.all_primitives ()) in
      let b = Buffer.create 100 in
      let fmt = Pretty_print.to_buffer b in
      Driver.configure fmt;
      Driver.from_string ~prims ~debug s fmt;
      Format.(pp_print_flush std_formatter ());
      Format.(pp_print_flush err_formatter ());
      flush stdout;
      flush stderr;
      let js = Buffer.contents b in
      let res : string -> unit -> J.t =
        Obj.magic (J.get global (J.string "toplevelEval"))
      in
      res (js : string)
    in
    let toplevel_eval (x : string) : unit -> J.t =
      let f : J.t = J.eval_string x in
      fun () ->
        let res = J.fun_call f [| global |] in
        Format.(pp_print_flush std_formatter ());
        Format.(pp_print_flush err_formatter ());
        flush stdout;
        flush stderr;
        res
    in
    let toc = get_bytecode_sections () in
    let sym =
      let t : Ocaml_compiler.Symtable.GlobalMap.t = toc.symb in
      Ocaml_compiler.Symtable.GlobalMap.fold
        (fun i n acc -> StringMap.add (Ocaml_compiler.Symtable.Global.name i) n acc)
        t
        StringMap.empty
    in
    let toplevel_reloc (name : J.t) : int =
      let name = J.to_string name in
      match StringMap.find_opt name sym with
      | Some i -> i
      | None -> Js_of_ocaml_compiler.Ocaml_compiler.Symtable.reloc_ident name
    in
    J.set global (J.string "toplevelCompile") (Obj.magic toplevel_compile) (*XXX HACK!*);
    J.set global (J.string "toplevelEval") (Obj.magic toplevel_eval);
    J.set global (J.string "toplevelReloc") (Obj.magic toplevel_reloc)
end

let setup = lazy (
  let module M = Jsoo_compiler_dynlink() in
  M.init ()
)

let initialize cmi_dirs  =
  List.iter Topdirs.dir_directory cmi_dirs;
  Lazy.force setup;
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "//toplevel//"


type redirection =
  { channel : out_channel ;
    name : string ;
    tee : string -> string -> unit ;
    callback : string -> unit ;
    prev : redirection option }

let redirections : (out_channel * redirection ref) list ref = ref []

let redirect_channel ?(tee = (fun _ _ -> ())) name channel callback =
  try
    flush channel ;
    let cur = List.assq channel !redirections in
    cur := { channel ; name ; tee ; callback ; prev = Some !cur } ;
    !cur
  with Not_found ->
    let cur = ref { channel ; name ; tee ; callback ; prev = None } in
    redirections := (channel, cur) :: !redirections ;
    let append text =
      let { tee ; name ; callback ; _ } = !cur in
      tee name text ;
      callback text in
    Sys_js.set_channel_flusher channel append ;
    !cur

let flush_redirected_channel redir =
  flush redir.channel (* [Sys_js] should do the rest *)

let stop_channel_redirection redir =
  let fail () = invalid_arg "Toploop_jsoo.stop_channel_redirection" in
  try
    let cur = List.assq redir.channel !redirections in
    if !cur != redir then fail () ;
    flush_redirected_channel redir ;
    match redir.prev with
    | Some prev -> cur := prev
    | None ->
        redirections :=
          List.filter (fun (ch, _) -> ch != redir.channel) !redirections ;
        let append text =
          Firebug.console##(log (Js.string text)) in
        Sys_js.set_channel_flusher redir.channel append ;
  with Not_found ->
    fail ()

let use_compiled_string code =
  ignore (Js.Unsafe.eval_string code);
  Toploop_ext.register_pending_printers ()

let () = Toploop_ext.set_inject_global_hook @@ fun id ->
  Js_of_ocaml.Js.Unsafe.set
    (Js_of_ocaml.Js.Unsafe.js_expr "jsoo_runtime.caml_global_data")
    (Js_of_ocaml.Js.string (Ident.name id))
    (Symtable.get_global_value id)
