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

module type S = sig

  type 'a ast_checker =
    ?on_expression: (Parsetree.expression -> Learnocaml_report.report) ->
    ?on_pattern: (Parsetree.pattern -> Learnocaml_report.report) ->
    ?on_structure_item: (Parsetree.structure_item -> Learnocaml_report.report) ->
    ?on_external: (Parsetree.value_description -> Learnocaml_report.report) ->
    ?on_include: (Parsetree.include_declaration -> Learnocaml_report.report) ->
    ?on_open: (Parsetree.open_description -> Learnocaml_report.report) ->
    ?on_module_occurence: (string -> Learnocaml_report.report) ->
    ?on_variable_occurence: (string -> Learnocaml_report.report) ->
    ?on_function_call: ((Parsetree.expression * (string * Parsetree.expression) list) -> Learnocaml_report.report) ->
    'a -> Learnocaml_report.report

  val ast_check_expr : Parsetree.expression ast_checker
  val ast_check_structure : Parsetree.structure ast_checker

  val ast_location_stripper : Ast_mapper.mapper

  val forbid : string -> ('a -> string) -> 'a list -> ('a -> Learnocaml_report.report)
  val restrict : string -> ('a -> string) -> 'a list -> ('a -> Learnocaml_report.report)
  val require : string -> ('a -> string) -> 'a -> ('a -> Learnocaml_report.report)
  val forbid_expr : string -> Parsetree.expression list -> (Parsetree.expression -> Learnocaml_report.report)
  val restrict_expr : string -> Parsetree.expression list -> (Parsetree.expression -> Learnocaml_report.report)
  val require_expr : string -> Parsetree.expression -> (Parsetree.expression -> Learnocaml_report.report)
  val forbid_syntax : string -> (_ -> Learnocaml_report.report)
  val require_syntax : string -> (_ -> Learnocaml_report.report)
  val (@@@) : ('a -> Learnocaml_report.report) -> ('a -> Learnocaml_report.report) -> ('a -> Learnocaml_report.report)

  val ast_sanity_check : ?modules: string list -> Parsetree.structure -> (unit -> Learnocaml_report.report) -> Learnocaml_report.report

  val find_binding : Parsetree.structure -> string -> (Parsetree.expression -> Learnocaml_report.report) -> Learnocaml_report.report

  (*----------------------------------------------------------------------------*)

  val test_ref :
    'a Ty.ty -> 'a ref -> 'a -> Learnocaml_report.report

  val test_variable :
    'a Ty.ty -> string -> 'a -> Learnocaml_report.report

  val test_variable_property :
    'a Ty.ty -> string -> ('a -> Learnocaml_report.report) -> Learnocaml_report.report

  val test_variable_against_solution :
    'a Ty.ty -> string -> Learnocaml_report.report

  (*----------------------------------------------------------------------------*)

  val compatible_type : expected:string -> string -> Learnocaml_report.report

  val existing_type : ?score:int -> string -> bool * Learnocaml_report.report

  val abstract_type : ?allow_private:bool -> ?score:int -> string -> bool * Learnocaml_report.report

  val test_student_code : 'a Ty.ty -> ('a -> Learnocaml_report.report) -> Learnocaml_report.report

  val test_module_property :
    'a Ty.ty -> string -> ('a -> Learnocaml_report.report) -> Learnocaml_report.report

  (*----------------------------------------------------------------------------*)

  type 'a result =
    | Ok of 'a
    | Error of exn

  val exec : (unit -> 'a) -> ('a * string * string) result
  val result : (unit -> 'a) -> 'a result

  type 'a tester =
    'a Ty.ty -> 'a result -> 'a result -> Learnocaml_report.report

  val test_ignore : 'a tester
  val test : 'a tester
  val test_eq : ('a result -> 'a result -> bool) -> 'a tester
  val test_eq_ok : ('a -> 'a -> bool) -> 'a tester
  val test_eq_exn : (exn -> exn -> bool) -> 'a tester
  val test_canon : ('a result -> 'a result) -> 'a tester
  val test_canon_ok : ('a -> 'a) -> 'a tester
  val test_canon_error : (exn -> exn) -> 'a tester
  val test_translate : ('a -> 'b) -> 'b tester -> 'b Ty.ty -> 'a tester

  type io_tester =
    string -> string -> Learnocaml_report.report

  val io_test_ignore : io_tester
  val io_test_equals :
    ?trim: char list -> ?drop: char list -> io_tester
  val io_test_lines :
    ?trim: char list -> ?drop: char list ->
    ?skip_empty: bool -> ?test_line: io_tester -> io_tester
  val io_test_items :
    ?split: char list -> ?trim: char list -> ?drop: char list ->
    ?skip_empty: bool -> ?test_item: io_tester -> io_tester

  type 'arg arg_mutation_test_callbacks =
    { before_reference : 'arg -> unit ;
      before_user : 'arg -> unit ;
      test : 'ret. ?test_result: 'ret tester -> 'ret tester }

  val arg_mutation_test_callbacks:
    ?test: 'a tester -> dup: ('a -> 'a) -> blit:('a -> 'a -> unit) -> 'a Ty.ty ->
    'a arg_mutation_test_callbacks

  val array_arg_mutation_test_callbacks:
    ?test: 'a array tester -> 'a array Ty.ty ->
    'a array arg_mutation_test_callbacks

  val ref_arg_mutation_test_callbacks:
    ?test: 'a ref tester -> 'a ref Ty.ty ->
    'a ref arg_mutation_test_callbacks


  (*----------------------------------------------------------------------------*)

  val test_function_1 :
    ?test: 'b tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before : ('a -> unit) ->
    ?after : ('a -> ('b * string * string) -> ('b * string * string) -> Learnocaml_report.report) ->
    ('a -> 'b) Ty.ty -> string -> ('a * 'b * string * string) list -> Learnocaml_report.report

  val test_function_1_against :
    ?gen: int ->
    ?test: 'b tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before_reference : ('a -> unit) ->
    ?before_user : ('a -> unit) ->
    ?after : ('a -> ('b * string * string) -> ('b * string * string) -> Learnocaml_report.report) ->
    ?sampler : (unit -> 'a) ->
    ('a -> 'b) Ty.ty -> string -> ('a -> 'b) -> 'a list -> Learnocaml_report.report

  val test_function_1_against_solution :
    ?gen: int ->
    ?test: 'b tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before_reference : ('a -> unit) ->
    ?before_user : ('a -> unit) ->
    ?after : ('a -> ('b * string * string) -> ('b * string * string) -> Learnocaml_report.report) ->
    ?sampler : (unit -> 'a) ->
    ('a -> 'b) Ty.ty -> string -> 'a list -> Learnocaml_report.report

  (*----------------------------------------------------------------------------*)

  val test_function_2 :
    ?test: 'c tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before : ('a -> 'b -> unit) ->
    ?after : ('a -> 'b -> ('c * string * string) -> ('c * string * string) -> Learnocaml_report.report) ->
    ('a -> 'b -> 'c) Ty.ty -> string -> ('a * 'b * 'c * string * string) list -> Learnocaml_report.report

  val test_function_2_against :
    ?gen: int ->
    ?test: 'c tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before_reference : ('a -> 'b -> unit) ->
    ?before_user : ('a -> 'b -> unit) ->
    ?after : ('a -> 'b -> ('c * string * string) -> ('c * string * string) -> Learnocaml_report.report) ->
    ?sampler : (unit -> 'a * 'b) ->
    ('a -> 'b -> 'c) Ty.ty -> string -> ('a -> 'b -> 'c) -> ('a * 'b) list -> Learnocaml_report.report

  val test_function_2_against_solution :
    ?gen: int ->
    ?test: 'c tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before_reference : ('a -> 'b -> unit) ->
    ?before_user : ('a -> 'b -> unit) ->
    ?after : ('a -> 'b -> ('c * string * string) -> ('c * string * string) -> Learnocaml_report.report) ->
    ?sampler : (unit -> 'a * 'b) ->
    ('a -> 'b -> 'c) Ty.ty -> string -> ('a * 'b) list -> Learnocaml_report.report

  (*----------------------------------------------------------------------------*)

  val test_function_3 :
    ?test: 'd tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before : ('a -> 'b -> 'c -> unit) ->
    ?after : ('a -> 'b -> 'c -> ('d * string * string) -> ('d * string * string) -> Learnocaml_report.report) ->
    ('a -> 'b -> 'c -> 'd) Ty.ty -> string -> ('a * 'b * 'c * 'd * string * string) list -> Learnocaml_report.report

  val test_function_3_against :
    ?gen: int ->
    ?test: 'd tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before_reference : ('a -> 'b -> 'c -> unit) ->
    ?before_user : ('a -> 'b -> 'c -> unit) ->
    ?after : ('a -> 'b -> 'c -> ('d * string * string) -> ('d * string * string) -> Learnocaml_report.report) ->
    ?sampler : (unit -> 'a * 'b * 'c) ->
    ('a -> 'b -> 'c -> 'd) Ty.ty -> string -> ('a -> 'b -> 'c -> 'd) -> ('a * 'b * 'c) list -> Learnocaml_report.report

  val test_function_3_against_solution :
    ?gen: int ->
    ?test: 'd tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before_reference : ('a -> 'b -> 'c -> unit) ->
    ?before_user : ('a -> 'b -> 'c -> unit) ->
    ?after : ('a -> 'b -> 'c -> ('d * string * string) -> ('d * string * string) -> Learnocaml_report.report) ->
    ?sampler : (unit -> 'a * 'b * 'c) ->
    ('a -> 'b -> 'c -> 'd) Ty.ty -> string -> ('a * 'b * 'c) list -> Learnocaml_report.report

  (*----------------------------------------------------------------------------*)

  val test_function_4 :
    ?test: 'e tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before : ('a -> 'b -> 'c -> 'd -> unit) ->
    ?after : ('a -> 'b -> 'c -> 'd -> ('e * string * string) -> ('e * string * string) -> Learnocaml_report.report) ->
    ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty -> string -> ('a * 'b * 'c * 'd * 'e * string * string) list -> Learnocaml_report.report

  val test_function_4_against :
    ?gen: int ->
    ?test: 'e tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before_reference : ('a -> 'b -> 'c -> 'd -> unit) ->
    ?before_user : ('a -> 'b -> 'c -> 'd -> unit) ->
    ?after : ('a -> 'b -> 'c -> 'd -> ('e * string * string) -> ('e * string * string) -> Learnocaml_report.report) ->
    ?sampler : (unit -> 'a * 'b * 'c * 'd) ->
    ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty -> string -> ('a -> 'b -> 'c -> 'd -> 'e)
    -> ('a * 'b * 'c * 'd) list -> Learnocaml_report.report

  val test_function_4_against_solution :
    ?gen: int ->
    ?test: 'e tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before_reference : ('a -> 'b -> 'c -> 'd -> unit) ->
    ?before_user : ('a -> 'b -> 'c -> 'd -> unit) ->
    ?after : ('a -> 'b -> 'c -> 'd -> ('e * string * string) -> ('e * string * string) -> Learnocaml_report.report) ->
    ?sampler : (unit -> 'a * 'b * 'c * 'd) ->
    ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty -> string -> ('a * 'b * 'c * 'd) list -> Learnocaml_report.report

  (*----------------------------------------------------------------------------*)

  (** The type of arguments, represented as heterogeneous lists.

  Usage: [arg 3 @@ arg "word" @@ last false] *)
  type ('arrow, 'uarrow, 'ret) args
  val last :
    'a ->
    ('a -> 'ret, 'a -> unit, 'ret) args
  val arg :
    'a ->
    ('ar -> 'row, 'ar -> 'urow, 'ret) args ->
    ('a -> 'ar -> 'row, 'a -> 'ar -> 'urow, 'ret) args

  val apply : ('ar -> 'row) -> ('ar -> 'row, 'ar -> 'urow, 'ret) args -> 'ret

  (** The type of function prototypes.

  Usage: [arg_ty [%ty: int] @@ arg_ty [%ty: string] @@ last_ty [%ty: bool] [%ty: unit]] *)
  type ('arrow, 'uarrow, 'ret) prot
  val last_ty :
    'a Ty.ty ->
    'ret Ty.ty ->
    (('a -> 'ret) Ty.ty, 'a -> unit, 'ret) prot
  val arg_ty :
    'a Ty.ty ->
    (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot ->
    (('a -> 'ar -> 'row) Ty.ty, ('a -> 'ar -> 'urow), 'ret) prot

  type 'a lookup = unit -> [ `Found of string * Learnocaml_report.report * 'a | `Unbound of string * Learnocaml_report.report ]

  val lookup : 'a Ty.ty -> ?display_name: string -> string -> 'a lookup
  val lookup_student : 'a Ty.ty -> string -> 'a lookup
  val lookup_solution : 'a Ty.ty -> string -> 'a lookup
  val found : string -> 'a -> 'a lookup
  val name : 'a lookup -> string

  val test_value : 'a lookup -> ('a -> Learnocaml_report.report) -> Learnocaml_report.report

  val test_function :
    ?test: 'ret tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before :
      (('ar -> 'row, 'ar -> 'urow, 'ret) args ->
       unit) ->
    ?after :
      (('ar -> 'row, 'ar -> 'urow, 'ret) args ->
       ('ret * string * string) ->
       ('ret * string * string) ->
       Learnocaml_report.report) ->
    (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot ->
    ('ar -> 'row) lookup ->
    (('ar -> 'row, 'ar -> 'urow, 'ret) args * (unit -> 'ret)) list ->
    Learnocaml_report.report

  val test_function_against :
    ?gen: int ->
    ?test: 'ret tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before_reference :
      (('ar -> 'row, 'ar -> 'urow, 'ret) args -> unit) ->
    ?before_user :
      (('ar -> 'row, 'ar -> 'urow, 'ret) args -> unit) ->
    ?after :
      (('ar -> 'row, 'ar -> 'urow, 'ret) args ->
       ('ret * string * string) ->
       ('ret * string * string) ->
       Learnocaml_report.report) ->
    ?sampler:
      (unit -> ('ar -> 'row, 'ar -> 'urow, 'ret) args) ->
    (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) prot ->
    ('ar -> 'row) lookup -> ('ar -> 'row) lookup ->
    ('ar -> 'row, 'ar -> 'urow, 'ret) args list ->
    Learnocaml_report.report

  (*----------------------------------------------------------------------------*)

  val set_result : Learnocaml_report.report -> unit

  (*----------------------------------------------------------------------------*)

  type 'a sampler = unit -> 'a
  val sample_int : int sampler
  val sample_float : float sampler
  val sample_list : ?min_size: int -> ?max_size: int -> ?dups: bool -> ?sorted: bool -> 'a sampler -> 'a list sampler
  val sample_array : ?min_size: int -> ?max_size: int -> ?dups: bool -> ?sorted: bool -> 'a sampler -> 'a array sampler
  val sample_option : 'a sampler -> 'a option sampler
  val sample_string : string sampler
  val sample_char : char sampler
  val sample_bool : bool sampler
  val sample_cases : 'a list -> 'a sampler
  val sample_alternatively : 'a sampler list -> 'a sampler

  val printable_fun : string -> (_ -> _ as 'f) -> 'f

end

module Make : functor
  (Params : sig
     val results : Learnocaml_report.report option ref
     val set_progress : string -> unit
     val timeout : int option
     module Introspection : Introspection_intf.INTROSPECTION
   end) -> S
