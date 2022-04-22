(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2022 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

include module type of Toploop_results

(** Parse and typecheck a given source code.

    @param setenv should the resulting environment replace the current
                  environment ?

    @return [Ok ()] in case of success and [Error err]
            where [err] contains the error message otherwise.

*)
val check: ?setenv:bool -> string -> unit toplevel_result


(** Execute a given source code. The evaluation stops after the
    first toplevel phrase (as terminated by ";;") that fails to
    parse/typecheck/compile or for which the evaluation raises an
    uncaught exception.

    @param ppf_code a formatter were the source code will be printed
           before its execution. The printing might be interleaved
           with call to "pp_answer" when a line finishes by ";;".

    @param ppf_answer a formatter were the compiler outputs will be
           printed.

    @param print_outcome should the toplevel print the computed
           values and their types ?

    @return [Error err] when parsing or typechecking failed, where
            [err] contains the error message. It returns [Ok true]
            when the code evaluation finished without uncaught
            exception, and [Ok false] otherwise. In the last case,
            the exception has been pretty-printed in [ppf_answer].
*)
val execute:
  ?ppf_code:Format.formatter ->
  ?print_outcome:bool ->
  ppf_answer:Format.formatter ->
  string -> bool toplevel_result


(** Execute a given source code. The code is parsed all at once
    before to typecheck/compile/evaluate phrase by phrase.

    @param filename a faked filename which will be used in error messages

    @param ppf_answer see {!val:execute}.

    @param print_outcome see {!val:execute}.

    @return as {!val:execute}.

*)
val use_string:
  ?filename:string ->
  ?print_outcome:bool ->
  ppf_answer:Format.formatter ->
  string -> bool toplevel_result

(** Wrap a given source code into a module and bind it with a given
    name.

    @param filename used for locations in error messages

    @param print_outcome see {!val:execute}.

    @param ppf_answer see {!val:execute}.

    @param modname the module name, it must start with a capital
           character.

    @param sig_code source code for the module signature.

    @return as {!val:execute}.

*)
val use_mod_string:
  ?filename:string ->
  ?print_outcome:bool ->
  ppf_answer:Format.formatter ->
  modname:string ->
  ?sig_code:string ->
  string -> bool toplevel_result

(** Registers the given cmi files contents into the running toplevel *)
val load_cmi_from_string:
  string -> unit

(** Registers a global into the toplevel. Can be used to dynamically create
    compilation units ([inject_global "Foo" (Obj.repr (module Foo))]). Does not
    affect the environment (suppose a corresponding .cmi) *)
val inject_global: string -> Obj.t -> unit

(** Register a hook to be called after inject_global on the newly registered
    ident. Useful for jsoo which has additional registrations required. *)
val set_inject_global_hook: (Ident.t -> unit) -> unit

(** Helpers to embed PPX into the toplevel. *)
module Ppx : sig
  val preprocess_structure: Parsetree.structure -> Parsetree.structure
  val preprocess_signature: Parsetree.signature -> Parsetree.signature
  val preprocess_phrase: Parsetree.toplevel_phrase -> Parsetree.toplevel_phrase
end

module Printer : Genprintval.S with type t = Obj.t

(** Used by our ppx *)
val install_printer: string -> string -> string -> ('a -> 'b) -> unit

(** Hook to be called after loading units so that the registered printers are
    present also in the toplevel's built-in printer. *)
val register_pending_printers: unit -> unit
