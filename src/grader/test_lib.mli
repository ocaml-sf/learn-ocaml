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
  (** {1 AST checker} 

   Various functions to explore the student's code AST and
   enforce restrictions on the student's code.  *)

  (** {2 Checker} *)
  
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

  (** {2 Functions for optional arguments of checkers} *)
    
  val forbid : string -> ('a -> string) -> 'a list -> ('a -> Learnocaml_report.report)
  val restrict : string -> ('a -> string) -> 'a list -> ('a -> Learnocaml_report.report)
  val require : string -> ('a -> string) -> 'a -> ('a -> Learnocaml_report.report)
  val forbid_expr : string -> Parsetree.expression list -> (Parsetree.expression -> Learnocaml_report.report)
  val restrict_expr : string -> Parsetree.expression list -> (Parsetree.expression -> Learnocaml_report.report)
  val require_expr : string -> Parsetree.expression -> (Parsetree.expression -> Learnocaml_report.report)
  val forbid_syntax : string -> (_ -> Learnocaml_report.report)
  val require_syntax : string -> (_ -> Learnocaml_report.report)
  val (@@@) : ('a -> Learnocaml_report.report) -> ('a -> Learnocaml_report.report) -> ('a -> Learnocaml_report.report)

  (** {2 AST sanity check } *)
    
  val ast_sanity_check : ?modules: string list -> Parsetree.structure -> (unit -> Learnocaml_report.report) -> Learnocaml_report.report


  (** {2 Finding in AST}*)
    
  (** [find_binding code_ast name cb] looks for variable [name] in
     [code_ast] and returns an {!LearnOcaml_report.Informative} report
     concated with the report resulting of [cb] applied to the
     Parsetree expression associated to variable [name] in [code_ast]
     if the variable is found and returns a
     {!LearnOcaml_report.Failure} report else.  *) 
  val find_binding : Parsetree.structure -> string -> (Parsetree.expression -> Learnocaml_report.report) -> Learnocaml_report.report

  (*----------------------------------------------------------------------------*)
    
  (** {1 Test functions for references and variables } *)

  (** [test_ref ty got exp] returns {!LearnOcaml_report.Success 1}
     report if reference [got] value is equal to [exp] and
     {!LearnOcaml_report.Failure} report else.  *)
  val test_ref :
    'a Ty.ty -> 'a ref -> 'a -> Learnocaml_report.report

  (** [test_variable ty name r] returns {!LearnOcaml_report.Success 1}
     report if variable named [name] exists and is equal to [r] and
     returns {!LearnOcaml_report.Failure} report else.*)
  val test_variable :
    'a Ty.ty -> string -> 'a -> Learnocaml_report.report

  (** [test_variable_property ty name cb] returns the report resulting
     of application of cb to variable named [name] if it exists and
     returns {!LearnOcaml_report.Failure} report else.  *)
  val test_variable_property :
    'a Ty.ty -> string -> ('a -> Learnocaml_report.report) -> Learnocaml_report.report


  (** [test_variable ty name r] returns {!LearnOcaml_report.Success 1}
     report if variable named [name] exists and is equal to variable
     with the same name defined in solution and returns
     {!LearnOcaml_report.Failure} report else.*)             
  val test_variable_against_solution :
    'a Ty.ty -> string -> Learnocaml_report.report

  (*----------------------------------------------------------------------------*)
  (** {1 Test functions for types} *)
 
  val compatible_type : expected:string -> string -> Learnocaml_report.report

  val existing_type : ?score:int -> string -> bool * Learnocaml_report.report

  val abstract_type : ?allow_private:bool -> ?score:int -> string -> bool * Learnocaml_report.report

  val test_student_code : 'a Ty.ty -> ('a -> Learnocaml_report.report) -> Learnocaml_report.report

  val test_module_property :
    'a Ty.ty -> string -> ('a -> Learnocaml_report.report) -> Learnocaml_report.report

  (*----------------------------------------------------------------------------*)
  (** {1 Test functions for functions }*)

  (** {2 Result} *)
    
  type 'a result =
    | Ok of 'a
    | Error of exn

  (** [exec v] executes [v ()] and returns [Ok (r, stdout, stderr)]
     if no exception is raised and where [r] is the result of [v ()],
     [stdout] the standard output string (possibly empty) and [stderr]
     the standard error string (possibly empty) or returns [Error exn]
     is exception [exn] is raised. Mays also return a timeout
     error. *)  
  val exec : (unit -> 'a) -> ('a * string * string) result

  (** [result v] executes [v ()] and returns [Ok r] where [r] is the
     result or [Error exn] if exception [exn] is raised. Mays also
     return a timeout error. *)
  val result : (unit -> 'a) -> 'a result

  (** {2 Tester} *)

  (** Testers are essentially used for the optional argument [~test]
     of test functions *) 
    
  type 'a tester =
    'a Ty.ty -> 'a result -> 'a result -> Learnocaml_report.report

  (** {3 Pre-defined testers and tester builders} *)
    
  val test_ignore : 'a tester
  val test : 'a tester
  val test_eq : ('a result -> 'a result -> bool) -> 'a tester
  val test_eq_ok : ('a -> 'a -> bool) -> 'a tester
  val test_eq_exn : (exn -> exn -> bool) -> 'a tester
  val test_canon : ('a result -> 'a result) -> 'a tester
  val test_canon_ok : ('a -> 'a) -> 'a tester
  val test_canon_error : (exn -> exn) -> 'a tester
  val test_translate : ('a -> 'b) -> 'b tester -> 'b Ty.ty -> 'a tester


  (** {2 IO tester} *)

  (** IO testers are essentially used for the optional arguments
     [~test_stdout] [~test_stderr] of test functions *) 
    
  type io_tester =
    string -> string -> Learnocaml_report.report

    
  (** {3 Pre-defined IO testers and IO tester builders} *)
    
  val io_test_ignore : io_tester
  val io_test_equals :
    ?trim: char list -> ?drop: char list -> io_tester
  val io_test_lines :
    ?trim: char list -> ?drop: char list ->
    ?skip_empty: bool -> ?test_line: io_tester -> io_tester
  val io_test_items :
    ?split: char list -> ?trim: char list -> ?drop: char list ->
    ?skip_empty: bool -> ?test_item: io_tester -> io_tester

  (** {2 Mutation observer builders} *)
    
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

  (** {2 Test functions}*)

  (** Three test functions for functions are defined for arity one to
     four functions:

  - [test_function_<args_nb> ty name tests]

  - [test_function_<args_nb>_against ty name rf tests]

  - [test_function_<args_nb>_against_solution ty name tests]

     They tests the [args_nb]-arity function named [name] with
     non-polymorphic type [ty] (a polymorphic function must be tested
     on a completly determined type) through tests [tests]. If a
     function named [name] is defined in the student code and has the
     right type (or a more generic one), tests [tests] are checked one
     by one and the function returns a report concatening reports of each
     test. Else a {!Learnocaml_report.Failure} report is returned.*)
    
  (** {3 For unary functions}*)
    
  (** [test_function_1 ty name tests] tests the function named [name]
     by directly comparing obtained outputs against expected outputs.

     A test [(arg-1, r, out, err)] results of a
     {!LearnOcaml_report.Success 1} report if the student function
     applied to [arg-1] is equal to [r] and if standard output and
     standard error messages match [out] and [err] respectively. The
     result of a test is a {!Learnocaml_report.Failure} report else.

     @see <#optional_arguments> for information about optional arguments. *)
  val test_function_1 :
    ?test: 'b tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before : ('a -> unit) ->
    ?after : ('a -> ('b * string * string) -> ('b * string * string) -> Learnocaml_report.report) ->
    ('a -> 'b) Ty.ty -> string -> ('a * 'b * string * string) list -> Learnocaml_report.report


  (** [test_function_1_against ty name rf tests] tests the function
     named [name] by comparing outputs obtained with the student
     function against outputs of [rf].

     A test [arg-1] results of a {!LearnOcaml_report.Success 1} report if
     the student function applied to [arg-1] gives the same result than
     the solution function [rf] applied to [arg-1]. Else the result of a
     test is a {!Learnocaml_report.Failure} report.

     @see <#optional_arguments> for information about optional arguments. *)
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



  (** [test_function_1_against ty name tests] tests the function named
     [name] by comparison to solution function which must be defined
     under name [name] in the corresponding [solution.ml] file. Same
     than [test_function_1_against] else.

     @see <#optional_arguments> for information about optional
     arguments. *)
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

  (** {3 For binary functions }*)
    
  (** [test_function_2 ty name tests] tests the function named [name]
     by directly comparing obtained outputs against expected outputs.

     A test [(arg-1, arg-2, r, out, err)] results of a
     {!LearnOcaml_report.Success 1} report if the student function
     applied to [arg-1] and [arg-2] is equal to [r] and if standard
     output and standard error messages match [out] and [err]
     respectively. The result of a test is a
     {!Learnocaml_report.Failure} report else.

     @see <#optional_arguments> for information about optional
     arguments. *)
  val test_function_2 :
    ?test: 'c tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before : ('a -> 'b -> unit) ->
    ?after : ('a -> 'b -> ('c * string * string) -> ('c * string * string) -> Learnocaml_report.report) ->
    ('a -> 'b -> 'c) Ty.ty -> string -> ('a * 'b * 'c * string * string) list -> Learnocaml_report.report

  (** [test_function_2_against ty name rf tests] tests the function
     named [name] by comparing outputs obtained with the student
     function against outputs of [rf].

     A test [(arg-1, arg-2)] results of a {!LearnOcaml_report.Success
     1} report if the student function applied to [arg-1] and [arg-2]
     gives the same result than the solution function [rf] applied to
     the same arguments. Else the result of a test is a
     {!Learnocaml_report.Failure} report.

     @see <#optional_arguments> for information about optional
     arguments. *)
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


  (** [test_function_2_against ty name tests] tests the function named
     [name] by comparison to solution function which must be defined
     under name [name] in the corresponding [solution.ml] file. Same
     than [test_function_2_against] else.

     @see <#optional_arguments> for information about optional
     arguments. *)
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

  (** {3 For three-arguments functions }*)
    
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

  (** {3 For four-arguments functions }*)
    
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


  (** {2:optional_arguments Optional arguments for test functions} *)

  (** The various test functions use numerous commun optional
     argument. Here is a list in alphabetic order of each of them and
     a comment on their utilities. *)

  (** {3 ?⁠after} [('a ‑> .. -> ('b * string * string) ‑> ('b * string
     * string) ‑> Learnocaml_report.report) ]

     {3 ?before} [('a ‑> .. -> unit)]

     {3 ?before_reference} [('a ‑> .. -> unit)]

     {3 ?before_user} [('a ‑> .. -> unit)]

     {3 ?gen} [~gen: n]: defined number [n] of automatically generated
     tested inputs. By default, [gen] is [max 5 (10 - List.length
     tests)]. Inputs generation is done using either predefined
     sampler or function defined with [~sampler] optional argument.

     @see <#predefined_samplers>.

    {3 ?sampler} [~sampler] defined the function used to automatically
     generated inputs. If not used, the test function checks if a
     sampler is defined for each input type in the current
     environment. Such sampler for a type [some-type] must be named
     [sample_some-type] and have a type [unit -> some-type] if not
     parametric or [(unit -> 'a) -> (unit -> 'b) -> ... -> unit ->
     some-type] else.

     @see <#predefined_samplers>.

    {3 ?test} ['b tester]

    {3 ?test_sdterr} [io_tester]

    {3 ?test_sdtout} [io_tester] *)

  (*----------------------------------------------------------------------------*)

  (** {1 WIP }*)
    
  (* Usage: (arg 3 @@ arg "word" @@ last false *)
  type ('arrow, 'uarrow, 'ret) args
  val last :
    'a ->
    ('a -> 'ret, 'a -> unit, 'ret) args
  val arg :
    'a ->
    ('ar -> 'row, 'ar -> 'urow, 'ret) args ->
    ('a -> 'ar -> 'row, 'a -> 'ar -> 'urow, 'ret) args

  val apply : ('ar -> 'row) -> ('ar -> 'row, 'ar -> 'urow, 'ret) args -> 'ret

  (* Usage: (arg [%ty: int] @@ arg [%ty: string] @@ last [%ty: bool] *)
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

  (** {1:predefined_samplers Predefined samplers } *)

  type 'a sampler = unit -> 'a

  (** {2 Samplers} *)
                  
  (** [sample_int ()] returns a random integer between -5 and 5. *)
  val sample_int : int sampler

  (** [sample_float ()] returns a random float betwenn -5. and 5. *)
  val sample_float : float sampler

  (** [sample_string ()] returns a randomly long random string. *)
  val sample_string : string sampler

  (** [sample_char ()] returns an alphabet letter randomly. *)
  val sample_char : char sampler

  (** [sample_bool ()] returns randomly [false] or [true]. *)
  val sample_bool : bool sampler

  (** {2 Sampler builders} *)

  (** [sample_list ~min_size ~max_size sample] returns a list sampler
     that generates a list of random length between [min_size] ([0] by
     default) and [max_size] ([10] by default) where each element are
     generated using [sample].

     If [~sorted:true] ([false] by default) the generated list is
     sorted.

     If [~dups:false] ([true] by default), all elements of generated
     list are unique.*)
  val sample_list : ?min_size: int -> ?max_size: int -> ?dups: bool -> ?sorted: bool -> 'a sampler -> 'a list sampler

  (** [sample_array ~min_size ~max_size sample] returns an array
     sampler that generates an array of random length between
     [min_size] ([0] by default) and [max_size] ([10] by default)
     where each element are generated using [sample].

     If [~sorted:true] ([false] by default) the generated array is
     sorted.

     If [~dups:false] ([true] by default), all elements of generated
     array are unique.*)
  val sample_array : ?min_size: int -> ?max_size: int -> ?dups: bool -> ?sorted: bool -> 'a sampler -> 'a array sampler

  (** [sample_alternatively s] returns a sampler that mimics randomly
     the behavior of one of [s] sampler and change at each call. *)
  val sample_alternatively : 'a sampler list -> 'a sampler

  (** [sample_case cases] returns a sampler that generates randomly
     one of the value of cases. *)
  val sample_cases : 'a list -> 'a sampler
    
  (** [sample_option sample] returns a sampler that generates an
     ['a option] value using [sample] to generate an ['a] value if
     necessary. *)
  val sample_option : 'a sampler -> 'a option sampler

  (**  {1 Utilities} *)
    
  val printable_fun : string -> (_ -> _ as 'f) -> 'f

end

module Make : functor
  (Params : sig
     val results : Learnocaml_report.report option ref
     val set_progress : string -> unit
     val timeout : int option
     module Introspection : Introspection_intf.INTROSPECTION
   end) -> S
