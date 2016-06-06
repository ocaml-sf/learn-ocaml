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

(** An extension to the OCaml's module [Toploop]. {ul {- The error
    message are made explicit in a value of type {!type:result}
    instead of being printed to the standard error or being raised as
    an exception. } {- We add a minimalistic set of typesafe
    self-introspection functions. } {- We usr [Lwt] in order to
    [yield] to the browser's display thread between the evaluation
    of two toplevel phrases. } }

*)

(** The type of the error returned by the compiler. Compared to
    [Location.error], the [sub]-error have been collected into a
    single error message and [if_highlight] is never empty but a copy
    of [msg].

*)
type error =
  { msg: string;
    locs: Location.t list;
    if_highlight: string; }

type warning = error

(** A [result] type for all the toplevel functions. *)
type 'a result =
  | Success of 'a * warning list
  | Error of error * warning list

(** Convert any exception to an [error] using the pretty-printers
    registred in [Location] or using [Printexc.to_string]. *)
val error_of_exn: exn -> error


(** Parse and typecheck a given source code

    @param setenv should the resulting environment replace the current
                  environment ?

    @return [Success ()] in case of success and [Error err]
            where [err] contains the error message otherwise.

*)
val check: ?setenv:bool -> string -> unit result Lwt.t


(** Execute a given source code. The evaluation stops after the first
    toplevel phrase (as terminated by ";;") that fails to compile or
    for which the evaluation raises an uncaught exception.

    @param ppf_code a formatter were the source code will be printed
           before its execution. The printing might be interleaved
           with call to "pp_answer" when a line finishes by ";;".

    @param pp_answer a formatter were the compiler outputs will be
           printed.

    @param print_outcome should the toplevel print the computed
           values and their types ?

    @return [Error err] when parsing or typechecking failed, where
            [err] contains the error message. It returns [Success true]
            when the code evaluation finished without uncaught
            exception, and [Success false] otherwise.
*)
val execute:
  ?ppf_code:Format.formatter ->
  ppf_answer:Format.formatter ->
  bool -> string -> bool result Lwt.t


(** Execute a given source code. The code is parsed and
    typechecked all at once before to start the evalution.

    @param pp_answer see {!val:execute}.

    @param print_outcome see {!val:execute}.

    @return as {!val:execute}.

*)
val use_string:
  ?filename:string ->
  ppf_answer:Format.formatter ->
  bool -> string -> bool result Lwt.t


(** Wrap a given source code into a module and bind it with a given name.

    @param pp_answer see {!val:execute}.

    @param print_outcome see {!val:execute}.

    @param modname the module name, it must start with a capital
           character.

    @param sig_code source code for the module signature.

    @param keep_ast the toplevel variable name in which the module AST
           will be dumped (if needed)

    @param keep_warnings the toplevel variable name in which the warnings
           that happened during the module compilation will be dumped (if needed)

    @return as {!val:execute}.

*)
val use_mod_string:
  ppf_answer:Format.formatter ->
  bool -> string ->
  ?sig_code:string -> string -> bool result Lwt.t

(** TODO *)
val insert_mod_ast_in_env: var_name: string -> string -> unit result Lwt.t

(** Insert in the toplevel environment a module named [Introspection]
    that respect the interface {!modtype:Tryocaml_messages.INTROSPECTION}. *)
val allow_introspection: unit -> unit

(** Insert a callback in the toplevel environment. *)
val register_callback : string -> 'a Ty.ty -> ('a -> unit) -> unit

(** Insert in the toplevel environment a reference. I. e. [create_ref
    name ty v] inserts a reference named [name] of type [ty] and
    initialised with [v]. It returns an accesor function. *)
val create_ref: string -> 'a Ty.ty -> 'a -> unit -> 'a
