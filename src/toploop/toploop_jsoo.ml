(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Js_of_ocaml_compiler

let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  Array.of_list(split 0 0)

let setup = lazy (
  Hashtbl.add Toploop.directive_table "enable"
    (Toploop.Directive_string Option.Optim.enable);
  Hashtbl.add Toploop.directive_table "disable"
    (Toploop.Directive_string Option.Optim.disable);
  Hashtbl.add Toploop.directive_table "debug_on"
    (Toploop.Directive_string Option.Debug.enable);
  Hashtbl.add Toploop.directive_table "debug_off"
    (Toploop.Directive_string Option.Debug.disable);
  Hashtbl.add Toploop.directive_table "tailcall"
    (Toploop.Directive_string (Option.Param.set "tc"));
  (* Workaround Marshal bug triggered by includemod.ml:607 *)
  Clflags.error_size := 0 ;
  (* Disable inlining of JSOO which may blow the JS stack *)
  Option.Optim.disable "inline" ;
  Topdirs.dir_directory "/cmis";
  let initial_primitive_count =
    Array.length (split_primitives (Symtable.data_primitive_names ())) in

  let compile s =
    let prims =
      split_primitives (Symtable.data_primitive_names ()) in
    let unbound_primitive p =
      try ignore (Js.Unsafe.eval_string p); false with _ -> true in
    let stubs = ref [] in
    Array.iteri
      (fun i p ->
         if i >= initial_primitive_count && unbound_primitive p then
           stubs :=
             Format.sprintf
               "function %s(){caml_failwith(\"%s not implemented\")}" p p
             :: !stubs)
      prims;
    let output_program = Driver.from_string prims s in
    let b = Buffer.create 100 in
    output_program (Pretty_print.to_buffer b);
    Format.(pp_print_flush std_formatter ());
    Format.(pp_print_flush err_formatter ());
    flush stdout; flush stderr;
    let res = Buffer.contents b in
    let res = String.concat "" !stubs ^ res in
    Js.Unsafe.global##(toplevelEval res)
  in
  Js.Unsafe.global##.toplevelCompile := compile (*XXX HACK!*);
  Js.Unsafe.global##.toplevelEval := (fun x ->
      let f : < .. > Js.t -> < .. > Js.t = Js.Unsafe.eval_string x in
      (fun () ->
         let res = f Js.Unsafe.global in
         Format.(pp_print_flush std_formatter ());
         Format.(pp_print_flush err_formatter ());
         flush stdout; flush stderr;
         res)))

let initialize () =
  Lazy.force setup ;
  Toploop.initialize_toplevel_env ()

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
