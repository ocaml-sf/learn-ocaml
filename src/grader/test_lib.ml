(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

module type S = sig

  val set_result : Learnocaml_report.t -> unit

  type nonrec 'a result = ('a, exn) result

  (*----------------------------------------------------------------------------*)

  module Ast_checker : sig
    type 'a ast_checker =
      ?on_expression: (Parsetree.expression -> Learnocaml_report.t) ->
      ?on_pattern: (Parsetree.pattern -> Learnocaml_report.t) ->
      ?on_structure_item: (Parsetree.structure_item -> Learnocaml_report.t) ->
      ?on_external: (Parsetree.value_description -> Learnocaml_report.t) ->
      ?on_include: (Parsetree.include_declaration -> Learnocaml_report.t) ->
      ?on_open: (Parsetree.open_description -> Learnocaml_report.t) ->
      ?on_module_occurence: (string -> Learnocaml_report.t) ->
      ?on_variable_occurence: (string -> Learnocaml_report.t) ->
      ?on_function_call: ((Parsetree.expression * (string * Parsetree.expression) list) -> Learnocaml_report.t) ->
      'a -> Learnocaml_report.t

    val ast_check_expr : Parsetree.expression ast_checker
    val ast_check_structure : Parsetree.structure ast_checker

    val find_binding : Parsetree.structure -> string -> (Parsetree.expression -> Learnocaml_report.t) -> Learnocaml_report.t

    val forbid : string -> ('a -> string) -> 'a list -> ('a -> Learnocaml_report.t)
    val restrict : string -> ('a -> string) -> 'a list -> ('a -> Learnocaml_report.t)
    val require : string -> ('a -> string) -> 'a -> ('a -> Learnocaml_report.t)

    val forbid_expr : string -> Parsetree.expression list -> (Parsetree.expression -> Learnocaml_report.t)
    val restrict_expr : string -> Parsetree.expression list -> (Parsetree.expression -> Learnocaml_report.t)
    val require_expr : string -> Parsetree.expression -> (Parsetree.expression -> Learnocaml_report.t)
    val forbid_syntax : string -> (_ -> Learnocaml_report.t)
    val require_syntax : string -> (_ -> Learnocaml_report.t)

    val ast_sanity_check : ?modules: string list -> Parsetree.structure -> (unit -> Learnocaml_report.t) -> Learnocaml_report.t

  end

  (*----------------------------------------------------------------------------*)

  type 'a tester =
    'a Ty.ty -> 'a result -> 'a result -> Learnocaml_report.t

  type io_tester =
    string -> string -> Learnocaml_report.t

  type io_postcond =
    string -> Learnocaml_report.t

  exception Timeout of int

  (*----------------------------------------------------------------------------*)

  module Tester : sig

    val test : 'a tester
    val test_ignore : 'a tester
    val test_eq : ('a result -> 'a result -> bool) -> 'a tester
    val test_eq_ok : ('a -> 'a -> bool) -> 'a tester
    val test_eq_exn : (exn -> exn -> bool) -> 'a tester
    val test_canon : ('a result -> 'a result) -> 'a tester
    val test_canon_ok : ('a -> 'a) -> 'a tester
    val test_canon_error : (exn -> exn) -> 'a tester
    val test_translate : ('a -> 'b) -> 'b tester -> 'b Ty.ty -> 'a tester

    val io_test_ignore : io_tester
    val io_test_equals :
      ?trim: char list -> ?drop: char list -> io_tester
    val io_test_lines :
      ?trim: char list -> ?drop: char list ->
      ?skip_empty: bool -> ?test_line: io_tester -> io_tester
    val io_test_items :
      ?split: char list -> ?trim: char list -> ?drop: char list ->
      ?skip_empty: bool -> ?test_item: io_tester -> io_tester

  end

  (*----------------------------------------------------------------------------*)

  module Mutation : sig

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

  end

  (*----------------------------------------------------------------------------*)

  module Sampler : sig
    type 'a sampler = unit -> 'a
    val sample_int : int sampler
    val sample_float : float sampler
    val sample_string : string sampler
    val sample_char : char sampler
    val sample_bool : bool sampler
    val sample_list : ?min_size: int -> ?max_size: int -> ?dups: bool -> ?sorted: bool -> 'a sampler -> 'a list sampler
    val sample_array : ?min_size: int -> ?max_size: int -> ?dups: bool -> ?sorted: bool -> 'a sampler -> 'a array sampler
    val sample_pair : 'a sampler -> 'b sampler -> ('a * 'b) sampler
    val sample_alternatively : 'a sampler list -> 'a sampler
    val sample_cases : 'a list -> 'a sampler
    val sample_option : 'a sampler -> 'a option sampler

    val printable_fun : string -> (_ -> _ as 'f) -> 'f
  end

(*----------------------------------------------------------------------------*)

  module Test_functions_ref_var : sig

    val test_ref :
      'a Ty.ty -> 'a ref -> 'a -> Learnocaml_report.t

    val test_variable :
      'a Ty.ty -> string -> 'a -> Learnocaml_report.t

    val test_variable_property :
      'a Ty.ty -> string -> ('a -> Learnocaml_report.t) -> Learnocaml_report.t

    val test_variable_against_solution :
      'a Ty.ty -> string -> Learnocaml_report.t

  end

  (*-----------------------------------------------------------------------------*)

  module Test_functions_vg_var : sig

    val test_vg :
      int -> int -> Vg.image -> Vg.image -> Learnocaml_report.t

    val test_vg_against_solution:
      int -> int -> string -> Learnocaml_report.t
  end

  (*----------------------------------------------------------------------------*)

  module Test_functions_types : sig
    val compatible_type : expected:string -> string -> Learnocaml_report.t

    val existing_type : ?score:int -> string -> bool * Learnocaml_report.t

    val abstract_type : ?allow_private:bool -> ?score:int -> string -> bool * Learnocaml_report.t

    val test_student_code : 'a Ty.ty -> ('a -> Learnocaml_report.t) -> Learnocaml_report.t

    val test_module_property :
      'a Ty.ty -> string -> ('a -> Learnocaml_report.t) -> Learnocaml_report.t
  end

  (*----------------------------------------------------------------------------*)

  module Test_functions_function : sig

    val test_function_1 :
      ?test: 'b tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before : ('a -> unit) ->
      ?after : ('a -> ('b * string * string) -> ('b * string * string) -> Learnocaml_report.t) ->
      ('a -> 'b) Ty.ty -> string -> ('a * 'b * string * string) list -> Learnocaml_report.t

    val test_function_1_against :
      ?gen: int ->
      ?test: 'b tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> unit) ->
      ?before_user : ('a -> unit) ->
      ?after : ('a -> ('b * string * string) -> ('b * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a) ->
      ('a -> 'b) Ty.ty -> string -> ('a -> 'b) -> 'a list -> Learnocaml_report.t

    val test_function_1_against_solution :
      ?gen: int ->
      ?test: 'b tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> unit) ->
      ?before_user : ('a -> unit) ->
      ?after : ('a -> ('b * string * string) -> ('b * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a) ->
      ('a -> 'b) Ty.ty -> string -> 'a list -> Learnocaml_report.t

    val test_function_1_against_postcond :
      ?gen: int ->
      ?test_stdout: io_postcond ->
      ?test_stderr: io_postcond ->
      ?before_reference : ('a -> unit) ->
      ?before_user : ('a -> unit) ->
      ?after : ('a -> ('b * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a) ->
      ('a -> 'b Ty.ty -> 'b result -> Learnocaml_report.t) ->
      ('a -> 'b) Ty.ty -> string -> 'a list -> Learnocaml_report.t

    (*----------------------------------------------------------------------------*)

    val test_function_2 :
      ?test: 'c tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before : ('a -> 'b -> unit) ->
      ?after : ('a -> 'b -> ('c * string * string) -> ('c * string * string) -> Learnocaml_report.t) ->
      ('a -> 'b -> 'c) Ty.ty -> string -> ('a * 'b * 'c * string * string) list -> Learnocaml_report.t

    val test_function_2_against :
      ?gen: int ->
      ?test: 'c tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> 'b -> unit) ->
      ?before_user : ('a -> 'b -> unit) ->
      ?after : ('a -> 'b -> ('c * string * string) -> ('c * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b) ->
      ('a -> 'b -> 'c) Ty.ty -> string -> ('a -> 'b -> 'c) -> ('a * 'b) list -> Learnocaml_report.t

    val test_function_2_against_solution :
      ?gen: int ->
      ?test: 'c tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> 'b -> unit) ->
      ?before_user : ('a -> 'b -> unit) ->
      ?after : ('a -> 'b -> ('c * string * string) -> ('c * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b) ->
      ('a -> 'b -> 'c) Ty.ty -> string -> ('a * 'b) list -> Learnocaml_report.t

    val test_function_2_against_postcond :
      ?gen: int ->
      ?test_stdout: io_postcond ->
      ?test_stderr: io_postcond ->
      ?before_reference : ('a -> 'b -> unit) ->
      ?before_user : ('a -> 'b -> unit) ->
      ?after : ('a -> 'b -> ('c * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b) ->
      ('a -> 'b -> 'c Ty.ty -> 'c result -> Learnocaml_report.t) ->
      ('a -> 'b -> 'c) Ty.ty -> string -> ('a * 'b) list -> Learnocaml_report.t

    (*----------------------------------------------------------------------------*)

    val test_function_3 :
      ?test: 'd tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before : ('a -> 'b -> 'c -> unit) ->
      ?after : ('a -> 'b -> 'c -> ('d * string * string) -> ('d * string * string) -> Learnocaml_report.t) ->
      ('a -> 'b -> 'c -> 'd) Ty.ty -> string -> ('a * 'b * 'c * 'd * string * string) list -> Learnocaml_report.t

    val test_function_3_against :
      ?gen: int ->
      ?test: 'd tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> 'b -> 'c -> unit) ->
      ?before_user : ('a -> 'b -> 'c -> unit) ->
      ?after : ('a -> 'b -> 'c -> ('d * string * string) -> ('d * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b * 'c) ->
      ('a -> 'b -> 'c -> 'd) Ty.ty -> string -> ('a -> 'b -> 'c -> 'd) -> ('a * 'b * 'c) list -> Learnocaml_report.t

    val test_function_3_against_solution :
      ?gen: int ->
      ?test: 'd tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> 'b -> 'c -> unit) ->
      ?before_user : ('a -> 'b -> 'c -> unit) ->
      ?after : ('a -> 'b -> 'c -> ('d * string * string) -> ('d * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b * 'c) ->
      ('a -> 'b -> 'c -> 'd) Ty.ty -> string -> ('a * 'b * 'c) list -> Learnocaml_report.t

    val test_function_3_against_postcond :
      ?gen: int ->
      ?test_stdout: io_postcond ->
      ?test_stderr: io_postcond ->
      ?before_reference : ('a -> 'b -> 'c -> unit) ->
      ?before_user : ('a -> 'b -> 'c -> unit) ->
      ?after : ('a -> 'b -> 'c -> ('d * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b * 'c) ->
      ('a -> 'b -> 'c -> 'd Ty.ty -> 'd result -> Learnocaml_report.t) ->
      ('a -> 'b -> 'c -> 'd) Ty.ty -> string -> ('a * 'b * 'c) list -> Learnocaml_report.t

    (*----------------------------------------------------------------------------*)

    val test_function_4 :
      ?test: 'e tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?after : ('a -> 'b -> 'c -> 'd -> ('e * string * string) -> ('e * string * string) -> Learnocaml_report.t) ->
      ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty -> string -> ('a * 'b * 'c * 'd * 'e * string * string) list -> Learnocaml_report.t

    val test_function_4_against :
      ?gen: int ->
      ?test: 'e tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?before_user : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?after : ('a -> 'b -> 'c -> 'd -> ('e * string * string) -> ('e * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b * 'c * 'd) ->
      ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty -> string -> ('a -> 'b -> 'c -> 'd -> 'e)
      -> ('a * 'b * 'c * 'd) list -> Learnocaml_report.t

    val test_function_4_against_solution :
      ?gen: int ->
      ?test: 'e tester ->
      ?test_stdout: io_tester ->
      ?test_stderr: io_tester ->
      ?before_reference : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?before_user : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?after : ('a -> 'b -> 'c -> 'd -> ('e * string * string) -> ('e * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b * 'c * 'd) ->
      ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty -> string -> ('a * 'b * 'c * 'd) list -> Learnocaml_report.t

    val test_function_4_against_postcond :
      ?gen: int ->
      ?test_stdout: io_postcond ->
      ?test_stderr: io_postcond ->
      ?before_reference : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?before_user : ('a -> 'b -> 'c -> 'd -> unit) ->
      ?after : ('a -> 'b -> 'c -> 'd -> ('e * string * string) -> Learnocaml_report.t) ->
      ?sampler : (unit -> 'a * 'b * 'c * 'd) ->
      ('a -> 'b -> 'c -> 'd -> 'e Ty.ty -> 'e result -> Learnocaml_report.t) ->
      ('a -> 'b -> 'c -> 'd -> 'e) Ty.ty -> string -> ('a * 'b * 'c * 'd) list -> Learnocaml_report.t

  end

  (*----------------------------------------------------------------------------*)

  module Test_functions_generic : sig

    val run_timeout : (unit -> 'a) -> 'a

    val exec : (unit -> 'a) -> ('a * string * string) result

    val result : (unit -> 'a) -> 'a result

    (*----------------------------------------------------------------------------*)

    include (module type of Fun_ty
                            with type ('a, 'b, 'c) args = ('a, 'b, 'c) Fun_ty.args
                            and type ('a, 'b, 'c) fun_ty = ('a, 'b, 'c) Fun_ty.fun_ty)

    val ty_of_prot :
      (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty -> ('ar -> 'row) Ty.ty
    [@@ocaml.deprecated "Use ty_of_fun_ty instead."]

    type 'a lookup = unit -> [ `Found of string * Learnocaml_report.t * 'a | `Unbound of string * Learnocaml_report.t ]

    val lookup : 'a Ty.ty -> ?display_name: string -> string -> 'a lookup
    val lookup_student : 'a Ty.ty -> string -> 'a lookup
    val lookup_solution : 'a Ty.ty -> string -> 'a lookup
    val found : string -> 'a -> 'a lookup
    val name : 'a lookup -> string

    val test_value : 'a lookup -> ('a -> Learnocaml_report.t) -> Learnocaml_report.t

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
       Learnocaml_report.t) ->
    (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty ->
    ('ar -> 'row) lookup ->
    (('ar -> 'row, 'ar -> 'urow, 'ret) args * (unit -> 'ret)) list ->
    Learnocaml_report.t

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
       Learnocaml_report.t) ->
    ?sampler:
      (unit -> ('ar -> 'row, 'ar -> 'urow, 'ret) args) ->
    (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty ->
    ('ar -> 'row) lookup -> ('ar -> 'row) lookup ->
    ('ar -> 'row, 'ar -> 'urow, 'ret) args list ->
    Learnocaml_report.t

  val test_function_against_solution :
    ?gen:int ->
    ?test: 'ret tester ->
    ?test_stdout: io_tester ->
    ?test_stderr: io_tester ->
    ?before_reference:
      (('ar -> 'row, 'ar -> 'urow, 'ret) args -> unit) ->
    ?before_user:
      (('ar -> 'row, 'ar -> 'urow, 'ret) args -> unit) ->
    ?after:
      (('ar -> 'row, 'ar -> 'urow, 'ret) args ->
        'ret * string * string ->
        'ret * string * string ->
        Learnocaml_report.item list) ->
    ?sampler:
      (unit -> ('ar -> 'row, 'ar -> 'urow, 'ret) args) ->
    (('ar -> 'row) Ty.ty, 'ar -> 'urow, 'ret) fun_ty ->
    string ->
    ('ar -> 'row, 'ar -> 'urow, 'ret) args list ->
    Learnocaml_report.item list

  val (==>) : 'params -> 'ret -> 'params * (unit -> 'ret)

  end

  val (@@@) : ('a -> Learnocaml_report.t) -> ('a -> Learnocaml_report.t) -> ('a -> Learnocaml_report.t)
  val (@@>) : Learnocaml_report.t -> (unit -> Learnocaml_report.t) -> Learnocaml_report.t
  val (@@=) : Learnocaml_report.t -> (unit -> Learnocaml_report.t) -> Learnocaml_report.t

  (**/**)
  include (module type of Ast_checker)
  include (module type of Tester)
  include (module type of Mutation)
  include (module type of Sampler)
  include (module type of Test_functions_types)
  include (module type of Test_functions_ref_var)
  include (module type of Test_functions_vg_var)
  include (module type of Test_functions_function)
  include (module type of Test_functions_generic)

end

module Make
    (Params : sig
       val results : Learnocaml_report.t option ref
       (* val set_progress : string -> unit *)
       val timeout : int option
       module Introspection : Introspection_intf.INTROSPECTION
     end) : S = struct

  let set_result report =
      Params.results := Some report

  type nonrec 'a result = ('a, exn) result

  (*----------------------------------------------------------------------------*)

  module Ast_checker = struct
    type 'a ast_checker =
      ?on_expression: (Parsetree.expression -> Learnocaml_report.t) ->
      ?on_pattern: (Parsetree.pattern -> Learnocaml_report.t) ->
      ?on_structure_item: (Parsetree.structure_item -> Learnocaml_report.t) ->
      ?on_external: (Parsetree.value_description -> Learnocaml_report.t) ->
      ?on_include: (Parsetree.include_declaration -> Learnocaml_report.t) ->
      ?on_open: (Parsetree.open_description -> Learnocaml_report.t) ->
      ?on_module_occurence: (string -> Learnocaml_report.t) ->
      ?on_variable_occurence: (string -> Learnocaml_report.t) ->
      ?on_function_call: ((Parsetree.expression * (string * Parsetree.expression) list) -> Learnocaml_report.t) ->
      'a -> Learnocaml_report.t

    let ast_check f
          ?(on_expression = fun _ -> [])
          ?(on_pattern = fun _ -> [])
          ?(on_structure_item = fun _ -> [])
          ?(on_external = fun _ -> [])
          ?(on_include = fun _ -> [])
          ?(on_open = fun _ -> [])
          ?(on_module_occurence = fun _ -> [])
          ?(on_variable_occurence = fun _ -> [])
          ?(on_function_call = fun _ -> [])
          arg =
      let open Parsetree in
      let open Ast_mapper in
      let res : Learnocaml_report.t list ref  = ref [] in
      let add l = res := l :: !res in
      let name { Location.txt; _ } = String.concat "." (Longident.flatten txt) in
      let variables = ref [] in
      let modules = ref [] in
      let treat_module_prefixes { Location.txt; _ } =
        match Longident.flatten txt with
        | fst :: _ when List.mem fst !modules (* shadowed *) -> ()
        | txt ->
           let rec all = function
             | [] -> ()
             | _ :: rest as l ->
                add @@ on_module_occurence (String.concat "." (List.rev l)) ;
                all rest in
           match List.rev txt with
           | [] -> ()
           | _ :: prefixes -> all prefixes in
      let treat_module ({ Location.txt; _ } as ident) =
        treat_module_prefixes ident ;
        match Longident.flatten txt with
        | [ m ] | m :: _  when List.mem m !modules (* shadowed *) -> ()
        | _ -> add @@ on_module_occurence (name ident) in
      let treat_variable ({ Location.txt; _ } as ident) =
        treat_module_prefixes ident ;
        match Longident.flatten txt with
        | m :: _ :: _  when List.mem m !modules (* shadowed *) -> ()
        | [ v ] when List.mem v !variables (* shadowed *) -> ()
        | _ -> add @@ on_variable_occurence (name ident) in
      let expr mapper expr =
        add @@ on_expression expr ;
        match expr with
        | { pexp_desc = Pexp_open (popen_override, popen_lid, iexpr); _ } ->
           let o = { popen_lid ; popen_override ;
                     popen_loc = Location.none ;
                     popen_attributes = [] } in
           let before = !modules in
           add @@ on_open o ;
           treat_module popen_lid ;
           variables := [] ;
           modules := [] (* over approximation *) ;
           ignore (mapper.expr mapper iexpr) ;
           modules := before ;
           expr
        | { pexp_desc = Pexp_letmodule ({ Location.txt = name; _ }, mexpr, iexpr); _ } ->
           let before = !modules in
           let variables_before = !variables in
           ignore (mapper.module_expr mapper mexpr) ;
           modules := name :: before ;
           variables := variables_before ;
           ignore (mapper.expr mapper iexpr) ;
           modules := before ;
           expr
        | { pexp_desc = Pexp_apply (fn, args); _ } as e ->
           let args = List.map
                        (function
                         | (Asttypes.Nolabel, v) -> ("", v)
                         | ((Asttypes.Labelled n | Asttypes.Optional n), v) -> (n, v))
                        args in
           add @@ on_function_call (fn, args) ;
           default_mapper.expr mapper e
        | { pexp_desc = Pexp_ident ident; _ } as e ->
           treat_variable ident ;
           default_mapper.expr mapper e
        | { pexp_desc = Pexp_construct (ident, _); _ } as e ->
           treat_module_prefixes ident ;
           default_mapper.expr mapper e
        | { pexp_desc = Pexp_record (fields, _); _ } as e ->
           List.iter (fun (ident, _) -> treat_module_prefixes ident) fields ;
           default_mapper.expr mapper e
        | { pexp_desc = Pexp_field (_, ident); _ } as e ->
           treat_module_prefixes ident ;
           default_mapper.expr mapper e
        | { pexp_desc = Pexp_setfield (_, ident, _); _ } as e ->
           treat_module_prefixes ident ;
           default_mapper.expr mapper e
        | { pexp_desc = Pexp_new ident; _ } as e ->
           treat_module_prefixes ident ;
           default_mapper.expr mapper e
        | { pexp_desc = Pexp_let (Asttypes.Nonrecursive, pvs, iexpr); _ } ->
           let before = !variables in
           let modules_before = !modules in
           List.iter (fun { pvb_expr; _ } ->
               ignore (mapper.expr mapper pvb_expr))
             pvs ;
           List.iter (fun { pvb_pat; _ } ->
               ignore (mapper.pat mapper pvb_pat))
             pvs ;
           ignore (mapper.expr mapper iexpr) ;
           variables := before ;
           modules := modules_before ;
           expr
        | { pexp_desc = Pexp_let (Asttypes.Recursive, pvs, iexpr); _ } ->
           let before = !variables in
           let modules_before = !modules in
           List.iter (fun { pvb_pat; _ } ->
               ignore (mapper.pat mapper pvb_pat))
             pvs ;
           List.iter (fun { pvb_expr; _ } ->
               ignore (mapper.expr mapper pvb_expr))
             pvs ;
           ignore (mapper.expr mapper iexpr) ;
           variables := before ;
           modules := modules_before ;
           expr
        | { pexp_desc = Pexp_for (pat, sexpr, eexpr, _, iexpr); _ } ->
           let before = !variables in
           let modules_before = !modules in
           ignore (mapper.expr mapper sexpr) ;
           ignore (mapper.expr mapper eexpr) ;
           ignore (mapper.pat mapper pat) ;
           ignore (mapper.expr mapper iexpr) ;
           variables := before ;
           modules := modules_before ;
           expr
        | { pexp_desc = Pexp_fun (label, vexpr, pat, iexpr); _ } ->
           let before = !variables in
           let modules_before = !modules in
           (match vexpr with
            | Some vexpr -> ignore (mapper.expr mapper vexpr)
            | None -> ()) ;
           (match label with
            | Asttypes.Nolabel -> ()
            | Asttypes.Labelled label
              | Asttypes.Optional label ->
               variables := label :: !variables) ;
           ignore (mapper.pat mapper pat) ;
           ignore (mapper.expr mapper iexpr) ;
           variables := before ;
           modules := modules_before ;
           expr
        | e -> default_mapper.expr mapper e in
      let structure_item mapper structure_item =
        add @@ on_structure_item structure_item ;
        match structure_item with
        | { pstr_desc = Pstr_module { pmb_name ; pmb_expr; _ }; _ } ->
           let before = !modules in
           let variables_before = !variables in
           ignore (mapper.module_expr mapper pmb_expr) ;
           modules := pmb_name.Location.txt :: before ;
           variables := variables_before ;
           structure_item
        | { pstr_desc = Pstr_recmodule mbs; _ } ->
           let variables_before = !variables in
           List.iter (fun { pmb_name; _ } -> modules := pmb_name.Location.txt :: !modules) mbs  ;
           let before = !modules in
           List.iter (fun { pmb_expr; _ } ->
               ignore (mapper.module_expr mapper pmb_expr) ;
               variables := variables_before ;
               modules := before) mbs  ;
           structure_item
        | { pstr_desc = Pstr_open o; _ } as si ->
           add @@ on_open o ;
           treat_module o.popen_lid ;
           ignore (default_mapper.structure_item mapper si) ;
           variables := [] ;
           modules := [] (* over approximation *) ;
           structure_item
        | { pstr_desc = Pstr_include i; _ } as si ->
           add @@ on_include i ;
           ignore (default_mapper.structure_item mapper si) ;
           variables := [] ;
           modules := [] (* over approximation *) ;
           structure_item
        | { pstr_desc = Pstr_primitive p; _ } as si ->
           add @@ on_external p ;
           ignore (default_mapper.structure_item mapper si) ;
           variables := p.pval_name.Location.txt :: !variables ;
           structure_item
        | si -> default_mapper.structure_item mapper si in
      let typ mapper typ =
        match typ with
        | { ptyp_desc = Ptyp_constr (ident, _); _ } as t ->
           treat_module_prefixes ident ;
           default_mapper.typ mapper t
        | { ptyp_desc = Ptyp_class (lid, _); _ } as t ->
           treat_module_prefixes lid ;
           default_mapper.typ mapper t
        | typ -> default_mapper.typ mapper typ
      in
      let module_expr mapper module_expr =
        match module_expr with
        | { pmod_desc = Pmod_ident ident; _ } as me ->
           treat_module ident ;
           default_mapper.module_expr mapper me
        | me -> default_mapper.module_expr mapper me
      in
      let pat mapper pat =
        add @@ on_pattern pat ;
        match pat with
        | { ppat_desc = (Ppat_var n | Ppat_alias (_, n)); _ } as p ->
           variables := n.Location.txt :: !variables ;
           default_mapper.pat mapper p
        | { ppat_desc = Ppat_unpack n; _ } as p ->
           modules := n.Location.txt :: !modules ;
           default_mapper.pat mapper p
        | { ppat_desc = Ppat_construct (ident, _); _ } as p ->
           treat_module_prefixes ident ;
           default_mapper.pat mapper p
        | { ppat_desc = Ppat_record (fields, _); _ } as p ->
           List.iter (fun (ident, _) -> treat_module_prefixes ident) fields ;
           default_mapper.pat mapper p
        | { ppat_desc = Ppat_type ident; _ } as p ->
           treat_module_prefixes ident ;
           default_mapper.pat mapper p
        | p -> default_mapper.pat mapper p in
      let case mapper ({ pc_lhs ; pc_guard ; pc_rhs } as case) =
        let before = !variables in
        let modules_before = !modules in
        ignore (mapper.pat mapper pc_lhs) ;
        (match pc_guard with Some pc_guard -> ignore (mapper.expr mapper pc_guard) | None -> ()) ;
        ignore (mapper.expr mapper pc_rhs) ;
        variables := before ;
        modules := modules_before ;
        case in
      let mapper = { default_mapper with
                     case ; expr ; structure_item ; pat ; typ ; module_expr } in
      f (mapper.expr mapper, mapper.structure mapper) arg ; List.flatten (List.rev !res)

    let ast_location_stripper =
      let open Ast_mapper in
      { default_mapper with location = (fun _ _ -> Location.none) }

    let ast_check_expr : Parsetree.expression ast_checker =
      ast_check @@ fun (check_expr, _) expr ->
      let expr =
        ast_location_stripper.Ast_mapper.expr ast_location_stripper expr
      in
      ignore @@ check_expr expr

    let ast_check_structure =
      ast_check @@ fun (_, check_structure) structure ->
      let structure = ast_location_stripper.Ast_mapper.structure ast_location_stripper structure in
      ignore @@ check_structure structure

    let forbid_syntax n = let already = ref false in fun _ ->
        if !already then [] else begin
          already := true ;
          Learnocaml_report.[ Message ([ Text "The " ; Code n ;
                              Text " syntax is forbidden" ], Failure) ]
        end

      let require_syntax n = let already = ref false in fun _ ->
        if !already then [] else begin
          already := true ;
          Learnocaml_report.[ Message ([ Text "The " ; Code n ;
                              Text " syntax has been found, as expected" ], Success 5) ]
        end

      let forbid k pr ls =
        let t = Hashtbl.create 10 in
        List.iter (fun e -> Hashtbl.add t e false) ls ;
        fun n ->
        match Hashtbl.find t n with
        | false ->
           Hashtbl.add t n true ;
           Learnocaml_report.[ Message ([ Text "The " ; Code (pr n) ; Text " " ; Text k ;
                                          Text " is forbidden" ], Failure) ]
        | true -> []
        | exception Not_found -> []

      let restrict k pr ls =
        let t = Hashtbl.create 10 in
        List.iter (fun e -> Hashtbl.add t e ()) ls ;
        fun n ->
        try Hashtbl.find t n ; [] with Not_found ->
          Hashtbl.add t n () ;
          Learnocaml_report.[ Message ([ Text "The " ; Code (pr n) ; Text " " ; Text k ;
                                         Text " is not allowed" ], Failure) ]

      let require k pr _ =
        let already = ref false in
        fun n ->
        if !already then [] else begin
            already := true ;
            Learnocaml_report.[ Message ([ Text "Found " ;  Text k ; Text " " ; Code (pr n) ], Success 5) ]
          end

      let print_exp = Format.asprintf "%a" Pprintast.expression
      let stripper = ast_location_stripper.Ast_mapper.expr ast_location_stripper

      let restrict_expr name exprs =
        restrict name print_exp (List.map stripper exprs)

      let forbid_expr name exprs =
        forbid name print_exp (List.map stripper exprs)

      let require_expr name expr =
        require name print_exp (stripper expr)

      let ast_sanity_check ?(modules = []) ast cb =
        let modules =
          (* Some may not even be present, we just want to display a message. *)
          [ "Obj" ; "Marshal" ; "Pervasives" ; "Sys" ;
            "Test_lib" ; "Introspection" ; "Report" ;
            "Js" ; "Toploop" ; "Compiler" ; "Unix" ] @ modules in
        let sanity_report =
          ast_check_structure
            ~on_external:(forbid_syntax "external")
            ~on_module_occurence:(forbid "module" (fun name -> name) modules)
            ast |> List.sort_uniq compare in
        if snd (Learnocaml_report.result sanity_report) then
          sanity_report
        else
          cb ()

      let find_binding code_ast name cb =
        let open Parsetree in
        let open Learnocaml_report in
        let rec findlet = function
          | [] -> [ Message ([ Text "I could not find " ; Code name ; Text "." ;
                               Break ;
                               Text "Check that it is defined as a simple " ; Code "let" ;
                               Text " at top level." ], Failure) ]
          | { pstr_desc = Pstr_value (_, bds); _ } :: rest ->
             let rec findvar = function
               | [] -> findlet rest
               | { pvb_pat = { ppat_desc = Ppat_var { Location.txt; _ }; _ } ; pvb_expr; _ } :: _ when txt = name ->
                  Message ([ Text "Found a toplevel definition for " ; Code name ; Text "."], Informative)
                  :: cb pvb_expr
               | _ :: rest -> findvar rest in
             findvar bds
          | _ :: rest -> findlet rest
        in findlet (List.rev code_ast)

  end

  (*----------------------------------------------------------------------------*)

  module Test_functions_types = struct
    open Params

    let compatible_type ~expected:exp got =
      let open Learnocaml_report in
      [ Message ([ Text "Checking that " ; Code got ;
                   Text "is compatible with " ; Code exp ], Informative) ;
        match Introspection.compatible_type exp ("Code." ^ got) with
        | Introspection.Absent ->
           Message ([ Text "Type not found" ], Failure)
        | Introspection.Incompatible msg ->
           Message ([ Text msg ], Failure)
        | Introspection.Present () ->
           Message ([ Text "Type found and compatible" ], Success 5) ]

    let existing_type ?(score = 1) name =
      let open Learnocaml_report in
      try let path = Env.lookup_type Longident.(parse ("Code." ^ name)) !Toploop.toplevel_env in
          let _ = Env.find_type path !Toploop.toplevel_env in
          true, [ Message ( [ Text "Type" ; Code name ; Text "found" ], Success score ) ]
      with Not_found -> false, [ Message ( [ Text "type" ; Code name ; Text "not found" ], Failure ) ]

    let abstract_type ?(allow_private = true) ?(score = 5) name =
      let open Learnocaml_report in
      try let path = Env.lookup_type Longident.(parse ("Code." ^ name)) !Toploop.toplevel_env in
          match Env.find_type path !Toploop.toplevel_env with
          | { Types. type_kind = Types.Type_abstract ; Types. type_manifest = None; _ } ->
             true, [ Message ([Text "Type" ; Code name ; Text "is abstract as expected." ], Success score) ]
          | { Types. type_kind = _ ; type_private = Asttypes.Private; _ } when allow_private ->
             true, [ Message ([Text "Type" ; Code name ; Text "is private, I'll accept that :-)." ], Success score) ]
          | { Types. type_kind = _; _ } ->
             false, [ Message ([Text "Type" ; Code name ; Text "should be abstract!" ], Failure) ]
      with Not_found -> false, [ Message ( [Text "Type" ; Code name ; Text "not found." ], Failure) ]

    let test_student_code ty cb =
      let open Learnocaml_report in
      match Introspection.get_value "Code" ty with
      | Introspection.Present v -> cb v
      | Introspection.Absent -> assert false
      | Introspection.Incompatible msg ->
         [ Message ([ Text "Your code doesn't match the expected signature." ; Break ;
                      Code msg (* TODO: hide or fix locations *) ], Failure) ]

    let test_module_property ty name cb =
      let open Learnocaml_report in
      match Introspection.get_value ("Code." ^ name) ty with
      | Introspection.Present v -> cb v
      | Introspection.Absent ->
         [ Message ([ Text "Module" ; Code name ; Text "not found." ], Failure) ]
      | Introspection.Incompatible msg ->
         [ Message ([ Text "Module" ; Code name ; Text "doesn't match the expected signature." ;
                      Break ; Code msg (* TODO: hide or fix locations *) ], Failure) ]

  end

  (*----------------------------------------------------------------------------*)

  type 'a tester =
    'a Ty.ty -> 'a result -> 'a result -> Learnocaml_report.t

  type io_tester =
    string -> string -> Learnocaml_report.t

  type io_postcond =
    string -> Learnocaml_report.t

  let typed_printer ty ppf v =
    Introspection.print_value ppf v ty

  exception Timeout of int

  (*----------------------------------------------------------------------------*)

  module Tester = struct

    let print_with ty = Format.asprintf "%a" (typed_printer ty)
    let exn_to_string = print_with [%ty: exn]

    let test_generic eq canon ty va vb =
    let to_string = print_with ty in
    if eq (canon va) (canon vb) then
      let txt_msg, code_msg =
        match va with
        | Ok v ->      "Correct value"     , to_string v
        | Error exn -> "Correct exception" , exn_to_string exn
      in Learnocaml_report.[ Message ([Text txt_msg; Code code_msg], Success 1) ]
    else
      let txt_msgs =
        match va with
        | Ok v ->
           Learnocaml_report.[Text "Wrong value" ; Code (to_string v)]
        | Error (Failure s) when s = "EXCESS" ->
           Learnocaml_report.[Text "Your code exceeded the output buffer size limit."]
        | Error Stack_overflow ->
           Learnocaml_report.[Text "Stack overflow. Too many recursions?"]
        | Error (Timeout limit) ->
           Learnocaml_report.[Text (Format.sprintf "Your code exceeded the time limit of %d seconds." limit)]
        | Error exn ->
           Learnocaml_report.[Text "Wrong exception" ; Code (exn_to_string exn)]
      in Learnocaml_report.[ Message (txt_msgs, Failure) ]

  let test_ignore ty va vb =
    match va, vb with
    | Ok _, Ok _ -> []
    | Ok v, Error _ ->
        Learnocaml_report.[ Message ([ Text "Unexpected result" ;
                            Code (print_with ty v) ;
                            Text "instead of exception" ], Failure) ]
    | Error (Failure s), _ when s = "EXCESS" ->
        Learnocaml_report.[ Message ([ Text "Your code exceeded the output buffer size limit." ], Failure) ]
    | Error Stack_overflow, _ ->
        Learnocaml_report.[ Message ([ Text "Stack overflow. Too many recursions?" ], Failure) ]
    | Error _, Error _ -> []
    | Error exn, Ok _ ->
        Learnocaml_report.[ Message ([ Text "Unexpected exception" ; Code (exn_to_string exn) ], Failure) ]

  let test ty va vb =
    test_generic (=) (fun x -> x) ty va vb
  let test_eq eq =
    test_generic eq (fun x -> x)
  let test_eq_ok eq =
    let eq ra rb = match ra, rb with
      | Ok a, Ok b -> eq a b
      | _ -> ra = rb in
    test_generic eq (fun x -> x)
  let test_eq_exn eq =
    let eq ra rb = match ra, rb with
      | Error a, Error b -> eq a b
      | _ -> ra = rb in
    test_generic eq (fun x -> x)
  let test_canon canon =
    test_generic (=) canon
  let test_canon_ok canon =
    let canon = function | Ok v -> Ok (canon v) | err -> err in
    test_generic (=) canon
  let test_canon_error canon =
    let canon = function | Error v -> Error (canon v) | ok -> ok in
    test_generic (=) canon
  let test_translate conv test wit _ got exp =
    let conv = function Error exn -> Error exn | Ok v -> Ok (conv v) in
    test wit (conv got) (conv exp)

  (*----------------------------------------------------------------------------*)

  let io_test_ignore _ _ = []

  let splitter chars =
    if chars = [] then
      fun s -> [ s ]
    else
      let pattern = Array.make 256 false in
      List.iter (fun c -> pattern.(Char.code c) <- true) chars ;
      fun s ->
        let len = String.length s in
        let rec loop acc i j =
          if j >= len then
            List.rev (if i >= len then acc else String.sub s i (j - i) :: acc)
          else if pattern.(Char.code (String.get s j)) then
            loop (String.sub s i (j - i) :: acc) (j + 1) (j + 1)
          else
            loop acc i (j + 1) in
        loop [] 0 0

  let trimmer chars =
    if chars = [] then
      fun s -> s
    else
      let pattern = Array.make 256 false in
      List.iter (fun c -> pattern.(Char.code c) <- true) chars ;
      fun s ->
        let len = String.length s in
        let rec start i =
          if i >= len then
            stop i (len - 1)
          else if pattern.(Char.code (String.get s i)) then
            start (i + 1)
          else
            stop i (len - 1)
        and stop i j =
          if j < 0 then
            (i, 0)
          else if pattern.(Char.code (String.get s j)) then
            stop i (j - 1)
          else
            (i, j + 1) in
        let i, j = start 0 in
        if j <= i then "" else String.sub s i (j - i)

  let dropper chars =
    if chars = [] then
      fun s -> s
    else
      let pattern = Array.make 256 false in
      List.iter (fun c -> pattern.(Char.code c) <- true) chars ;
      fun s ->
        let len = String.length s in
        let buf = Bytes.create len in
        let rec drop i m =
          if i = len then m else
            let c = String.get s i in
            if pattern.(Char.code c) then
              drop (i + 1) m
            else begin
              Bytes.set buf m c ;
              drop (i + 1) (m + 1)
            end in
        Bytes.sub_string buf 0 (drop 0 0)

  let io_test_equals ?(trim = []) ?(drop = []) =
    let open Learnocaml_report in
    let trim = trimmer trim in
    let drop = dropper drop in
    let tr s = trim (drop s) in
    fun got expected ->
      if tr got = tr expected then
        [ Message ([ Text "Expected output" ; Output got ], Success 5) ]
      else
        [ Message ([ Text "Unexpected output" ; Output got ], Failure) ]

  let io_test_items
      ?(split = []) ?(trim = []) ?(drop = [])
      ?(skip_empty = false) ?(test_item = io_test_equals ~trim:[] ~drop:[]) =
    let open Learnocaml_report in
    let split = splitter split in
    let trim = trimmer trim in
    let drop = dropper drop in
    let tr s = trim (drop s) in
    fun sgot sexpected ->
      let got = List.map tr (split sgot) in
      let expected = List.map tr (split sexpected) in
      let got = if skip_empty then List.filter ((<>) "") got else got in
      let expected = if skip_empty then List.filter ((<>) "") expected else expected in
      let rec test_items = function
        | [], [] ->
            [ Message ([ Text "Expected output" ; Output sgot ], Success 5) ]
        | got :: gots, expected :: expecteds ->
            if snd (result (test_item got expected)) then
              [ Message ([ Text "Unexpected output" ; Output sgot ], Failure) ]
            else
              test_items (gots, expecteds)
        | _, _ ->
            [ Message ([ Text "Unexpected output" ; Output sgot ], Failure) ]
      in test_items (got, expected)

  let io_test_lines
        ?(trim = []) ?(drop = [])
        ?(skip_empty = false) ?(test_line = io_test_equals ~trim:[] ~drop:[]) got expected =
    io_test_items ~split: [ '\n' ] ~trim ~drop ~skip_empty ~test_item: test_line got expected
  end

  module Mutation = struct
    open Tester

    type 'arg arg_mutation_test_callbacks =
      { before_reference : 'arg -> unit ;
        before_user : 'arg -> unit ;
        test : 'ret. ?test_result: 'ret tester -> 'ret tester }

    let arg_mutation_test_callbacks ?(test = test)  ~dup ~blit ty =
      let sam = ref None in
      let got = ref None in
      let exp = ref None in
      let before_reference a =
        sam := Some (dup a) in
      let before_user a =
        exp := Some (dup a) ;
        got := Some a ;
        match !sam with None -> () | Some src -> blit src a in
      let test ?(test_result = test_ignore) ret_ty va vb =
        let result_report = test_result ret_ty va vb in
        let report = match va, vb with
          | Ok _, Ok _ ->
             let got = match !got with Some g -> g | None -> invalid_arg "arg_mutation_test_callbacks" in
             let exp = match !exp with Some e -> e | None -> invalid_arg "arg_mutation_test_callbacks" in
             test ty (Ok got) (Ok exp)
          | Error ea, Ok _ ->
             let exp = match !exp with Some e -> e | None -> invalid_arg "arg_mutation_test_callbacks" in
             test ty (Error ea) (Ok exp)
          | Ok _, Error eb ->
             let got = match !got with Some g -> g | None -> invalid_arg "arg_mutation_test_callbacks" in
             test ty (Ok got) (Error eb)
          | Error ea, Error eb ->
             test ty (Error ea) (Error eb) in
        result_report @ report in
      { before_reference ; before_user ; test }

  let array_arg_mutation_test_callbacks ?(test = test) ty =
    let blit src dst = Array.blit src 0 dst 0 (Array.length dst) in
    let dup = Array.copy in
    arg_mutation_test_callbacks ~test ~blit ~dup ty

  let ref_arg_mutation_test_callbacks ?(test = test) ty =
    let blit src dst = dst := !src in
    let dup r = ref !r in
    arg_mutation_test_callbacks ~test ~blit ~dup ty

  end

  (*----------------------------------------------------------------------------*)

  module Test_functions_generic = struct
    open Params
    open Tester

    let sigalrm_handler time =
      Sys.Signal_handle (fun _ -> raise (Timeout time))

    let run_timeout ~time v =
      let old_behavior = Sys.signal Sys.sigalrm (sigalrm_handler time) in
      let reset_sigalrm () = Sys.set_signal Sys.sigalrm old_behavior
      in ignore (Unix.alarm time);
         try
           let res = v () in
           reset_sigalrm (); res
         with exc ->
           reset_sigalrm (); raise exc

    let run_timeout v =
      match Params.timeout with
      | Some time ->
          run_timeout ~time v
      | None ->
          v()

    let exec v =
      Introspection.grab_stdout () ;
      Introspection.grab_stderr () ;
      try
        let res = run_timeout v in
        let out = Introspection.release_stdout () in
        let err = Introspection.release_stderr () in
        Ok (res, out, err)
      with exn ->
        ignore (Introspection.release_stdout ()) ;
        ignore (Introspection.release_stderr ()) ;
        Error exn

    let result v = match exec v with
      | Ok (v, _, _) -> Ok v
      | Error exn -> Error exn


    (*----------------------------------------------------------------------------*)

    let verify
          ?(test_stdout = fun _ -> []) ?(test_stderr = fun _ -> [])
          ?(pre = (fun _ -> ())) ?(post = (fun _ -> [])) test ty v =
      let v = pre (); exec v in
      match v with
      | Ok (v, out, err) ->
         let post_report = post (v, out, err) in
         let report = test ty (Ok v) in
         let stdout_report = test_stdout out in
         let stderr_report = test_stderr err in
         report @ stdout_report @ stderr_report @ post_report
      | Error exn -> test ty (Error exn)

    let expect
          ?(test = test) ?(test_stdout = io_test_ignore) ?(test_stderr = io_test_ignore)
          ?(pre = (fun _ -> ())) ?(post = (fun _ _ -> [])) ty va vb  =
      let vb = exec vb in
      let va = pre () ; exec va in
      match va, vb with
      | Ok (va, outa, erra), Ok (vb, outb, errb) ->
         let post_report = post (va, outa, erra) (vb, outb, errb) in
         let report = test ty (Ok va) (Ok vb) in
         let stdout_report = test_stdout outa outb in
         let stderr_report = test_stderr erra errb in
         report @ stdout_report @ stderr_report @ post_report
      | Ok (va, _, _), Error exnb ->
         test ty (Ok va) (Error exnb)
      | Error exna, Ok (vb, _, _) ->
         test ty (Error exna) (Ok vb)
      | Error exna, Error exnb ->
         test ty (Error exna) (Error exnb)

    (*----------------------------------------------------------------------------*)

    include Fun_ty

    (* The GADT [args] & [last, arg] are defined in [fun_ty.ml] *)

    (* The GADT [fun_ty] &
       [last_ty, arg_ty, ty_of_fun_ty, apply, get_ret_ty, print, get_sampler]
       are defined in [fun_ty.ml] *)

    let ty_of_prot = ty_of_fun_ty
    [@@ocaml.deprecated "Use ty_of_fun_ty instead."]

    module Aux = struct
      let typed_printer = typed_printer
      let typed_sampler = Introspection.get_sampler
    end
    module FunTyAux = Make(Aux)

    (*----------------------------------------------------------------------------*)

    type 'a lookup = unit -> [ `Found of string * Learnocaml_report.t * 'a | `Unbound of string * Learnocaml_report.t ]

    let lookup ty ?display_name name =
      let display_name = match display_name with None -> name | Some name -> name in
      let open Learnocaml_report in
      let res = match Introspection.get_value name ty with
        | Introspection.Present v ->
           let msg =
             [ Message ([ Text "Found" ; Code display_name ;
                          Text "with compatible type." ], Informative) ] in
           `Found (display_name, msg, v)
        | Introspection.Absent ->
           `Unbound
             (name, [ Message ([ Text "Cannot find " ; Code display_name ], Failure) ])
        | Introspection.Incompatible msg ->
           `Unbound
             (name, [ Message ([ Text "Found" ; Code display_name ;
                                 Text "with unexpected type:" ; Break ;
                                 Code msg ], Failure) ]) in
      fun () -> res

    let lookup_student ty name =
      let open Learnocaml_report in
      let res = match Introspection.get_value ("Code." ^ name) ty with
        | Introspection.Present v ->
           let msg =
             [ Message ([ Text "Found" ; Code name ;
                          Text "with compatible type." ], Informative) ] in
           `Found (name, msg, v)
        | Introspection.Absent ->
           `Unbound
             (name, [ Message ([ Text "Cannot find " ; Code name ], Failure) ])
        | Introspection.Incompatible msg ->
           `Unbound
             (name, [ Message ([ Text "Found" ; Code name ;
                                 Text "with unexpected type:" ; Break ;
                                 Code msg ], Failure) ]) in
      fun () -> res

    let lookup_solution ty name =
      let open Learnocaml_report in
      let res = match Introspection.get_value ("Solution." ^ name) ty with
        | Introspection.Present v ->
           `Found (name, [], v)
        | Introspection.Absent ->
           `Unbound
             (name, [ Message ([ Text "Looking for " ; Code name  ], Informative) ;
                      Message ([ Text "Solution not found!" ], Failure) ])
        | Introspection.Incompatible msg ->
           `Unbound
             (name, [ Message ([ Text "Looking for " ; Code name  ], Informative) ;
                      Message ([ Text "Solution is wrong!" ; Break ; Code msg ], Failure) ]) in
      fun () -> res

    let name f = match f () with `Unbound (n, _) | `Found (n, _, _) -> n

    let found n v () = `Found (n, [], v)

    let test_value lookup cb =
      match lookup () with
      | `Unbound (_, report) -> report
      | `Found (_, report, v) -> report @ cb v

    (*----------------------------------------------------------------------------*)

    let run_test
          ?(before = (fun _ -> ())) ~after name prot tests for_case =
      let before args () = before args in
      let ty = Fun_ty.ty_of_fun_ty prot in
      let for_casel case =
        let args, ret = case () in
        let code = Format.asprintf "@[<hv 2>%s%a@]" name (FunTyAux.print prot) args in
        let ret_ty = Fun_ty.get_ret_ty ty args in
        Learnocaml_report.(Message ([ Text "Computing" ; Code code ], Informative)) ::
          for_case (before args) (after args) args ret_ty ret
      in List.flatten @@ List.map for_casel tests

    let test_function_generic
          ?test ?test_stdout ?test_stderr
          ?before ?(after = (fun _ _ _ -> []))
          prot uf tests =
      test_value uf @@ fun ruf ->
      let for_case pre post args ret_ty =
          expect ~pre ~post
            ?test ?test_stdout ?test_stderr
             ret_ty (fun () -> Fun_ty.apply ruf args)
      in run_test ?before ~after (name uf) prot tests for_case

    let test_function_generic_postcond
          ?test_stdout ?test_stderr
          ?before ?(after = (fun _ _ -> []))
          test name prot tests =
      let for_case pre post args =
          verify ~pre ~post
            ?test_stdout ?test_stderr
            (test args)
       in run_test ?before ~after name prot tests for_case

    let test_function
          ?test ?test_stdout ?test_stderr
          ?before ?after
          prot uf tests =
      test_function_generic
        ?test ?test_stdout ?test_stderr
        ?before ?after
        prot uf
        (List.map (fun x () -> x) tests)

    let make_tests ?gen ?sampler ?(before_reference = fun _ -> ()) prot rf tests =
      let gen = match gen with
        | Some n -> n
        | None -> max 5 (10 - List.length tests) in
      let tests = match gen with
        | 0 -> List.map (fun x () -> x) tests
        | _ ->
           let sampler =
             match sampler with
             | None -> FunTyAux.get_sampler prot
             | Some sampler -> sampler in
           let rec make i =
             if i <= 0 then [] else sampler :: make (i - 1) in
           List.map (fun x () -> x) tests @ make gen in
      List.map
        (fun a () -> let a = a () in (a, (fun () -> before_reference a ; Fun_ty.apply rf a)))
        tests

    let test_function_against_generic ?gen
          ?test ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler
          prot uf rf tests =
      test_value rf @@ fun rf ->
      let tests = make_tests ?gen ?sampler ?before_reference prot rf tests in
      test_function_generic
        ?test ?test_stdout ?test_stderr
        ?before:before_user ?after prot uf tests

    let test_function_against_generic_postcond ?gen
          ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler
          test name prot rf tests =
      test_value rf @@ fun rf ->
      let tests = make_tests ?gen ?sampler ?before_reference prot rf tests in
      test_function_generic_postcond
        ?test_stdout ?test_stderr
        ?before:before_user ?after test name prot tests

    let test_function_against ?gen
          ?test ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler prot uf rf tests =
      test_function_against_generic ?gen
        ?test ?test_stdout ?test_stderr
        ?before_reference ?before_user ?after ?sampler prot uf rf tests

    let test_function_against_solution ?gen
          ?test ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler prot name tests =
      let ty = Fun_ty.ty_of_fun_ty prot in
      test_function_against_generic ?gen
        ?test ?test_stdout ?test_stderr
        ?before_reference ?before_user ?after ?sampler prot
        (lookup_student ty name) (lookup_solution ty name) tests

  let (==>) params ret = (params, fun () -> ret)

  end

  (*----------------------------------------------------------------------------*)

  module Test_functions_ref_var = struct
    open Test_functions_generic
    open Tester

    let test_ref ty got exp =
      let open Learnocaml_report in
      let mk_txt str =
        [ Text str; Code (print_with ty !got)] in
      if !got = exp then
        [ Message (mk_txt "Correct value", Success 1) ]
      else
        [ Message (mk_txt "Wrong value"  , Failure) ]

    let test_variable ty name r =
      test_value (lookup_student ty name) @@ fun v ->
      expect ~test ty (fun () -> v) (fun () -> r)

    let test_variable_property ty name cb =
      test_value (lookup_student ty name) cb

    let test_variable_against_solution ty name =
      test_value (lookup_solution ty name) @@ fun sol ->
      test_variable ty name sol

  end

  (*----------------------------------------------------------------------------*)

  module Test_functions_vg_var = struct
    open Gg
    open Vg
    open Bigarray
    open Test_functions_generic

    let render_array w h img =
      let size = Size2.v (float_of_int w) (float_of_int h) in
      let view = Box2.v P2.o size in
      let stride = 3 * w in
      let data =
        Array1.create int8_unsigned c_layout (stride * h)
      in
      let target = Vgr_bigarray.target data in
      let r = Vgr.create target `Other in
      ignore (Vgr.render r (`Image (size, view, img)));
      ignore (Vgr.render r `End );
      data

    let compute_diff_array f_dist w h got exp =
      let got = render_array w h got in
      let exp = render_array w h exp in
      let size = Array1.dim exp in
      let kind = Array1.kind exp in
      let layout = Array1.layout exp in
      let output = Array1.create kind layout size in
      let rec compute_array k =
        if k >= size then output
        else
          let p_exp =
            Array1.get exp k, Array1.get exp (k+1), Array1.get exp (k+2)
          in
          let p_got =
            Array1.get got k, Array1.get got (k+1), Array1.get got (k+2)
          in
          let r, g, b = f_dist p_exp p_got in
          Array1.set output k r;
          Array1.set output (k+1) g;
          Array1.set output (k+2) b;
          compute_array (k+3)
      in got, (compute_array 0)

    let compute_dist p_exp p_got =
      let r_exp, g_exp, b_exp = p_exp in
      let r_got, g_got, b_got = p_got in
      let partial_dist c_exp c_got =
        (c_exp - c_got |> float_of_int) ** 2.0
      in
      let r = partial_dist r_exp r_got in
      let g = partial_dist g_exp g_got in
      let b = partial_dist b_exp b_got in
      let d = sqrt (r +. g +. b) |> int_of_float in
      let gray = (d * 255) / (3. *. (255. ** 2.) |> sqrt |> int_of_float) in
      gray, gray, gray

    let test_vg w h got exp =
      let open Learnocaml_report in
      let got, diff = compute_diff_array compute_dist w h got exp in
      let size = Array1.dim diff in
      let rec check_image k =
        if k >= size then
          [Message ([Text "Correct value" ; Break; Image (got, w, h)], Success 1)]
        else
          let r, g, b =
            Array1.get diff k, Array1.get diff (k+1), Array1.get diff (k+2)
          in
          if r != 0 || g != 0 || b != 0 then
            [Message ([Text "Wrong value" ; Break ; Image (diff, w, h)], Failure)]
          else check_image (k+3)
      in check_image 0

    let test_vg_against_solution w h name =
      let ty = [%ty: Vg.image ] in
      test_value (lookup_solution ty name) @@ fun sol ->
      test_value (lookup_student ty name) @@ fun got ->
      test_vg w h got sol

  end

  (*----------------------------------------------------------------------------*)

  module Test_functions_function = struct
    open Test_functions_generic

    let function_1_adapter_pre sampler ty =
      let pre = function
        | None -> (fun _ -> ())
        | Some pre -> Fun_ty.apply_args_1 pre in
      let sampler = match sampler with
        | None -> None
        | Some sampler -> Some (fun () -> Fun_ty.last (sampler ())) in
      let arg_ty, ret_ty = Ty.domains ty in
      let prot = Fun_ty.last_ty arg_ty @@ ret_ty in
      pre, sampler, prot

    let function_1_adapter after sampler ty =
      let after = match after with
        | None -> fun _ _ _ -> []
        | Some after -> Fun_ty.apply_args_1 after
      in
      let pre, sampler, prot = function_1_adapter_pre sampler ty in
      after, pre, sampler, prot

    let test_function_1
          ?test ?test_stdout ?test_stderr
          ?before ?after ty name tests =
      let tests = List.map (fun (x, r, out, err) ->
          (Fun_ty.last x, (fun () -> output_string stdout out ; output_string stderr err ; r)))
          tests in
      let after, pre, _, prot = function_1_adapter after None ty in
      test_function
        ?test ?test_stdout ?test_stderr
        ~before:(pre before)
        ~after prot (lookup_student ty name) tests

    let test_function_1_against ?gen
          ?test ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler ty name rf tests =
      let tests = List.map (fun x -> Fun_ty.last x) tests in
      let after, pre, sampler, prot = function_1_adapter after sampler ty in
      test_function_against ?gen
        ?test ?test_stdout ?test_stderr
        ~before_reference:(pre before_reference)
        ~before_user:(pre before_user)
        ~after ?sampler prot (lookup_student ty name) (found name rf) tests

    let test_function_1_against_solution ?gen
          ?test ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler ty name tests =
      let tests = List.map (fun x -> Fun_ty.last x) tests in
      let after, pre, sampler, prot = function_1_adapter after sampler ty in
      test_function_against ?gen
        ?test ?test_stdout ?test_stderr
        ~before_reference:(pre before_reference)
        ~before_user:(pre before_user)
        ~after ?sampler prot (lookup_student ty name) (lookup_solution ty name) tests

    let test_function_1_against_postcond ?gen
          ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler test ty name tests =
      let tests = List.map (fun x -> Fun_ty.last x) tests in
      let after = match after with
        | None -> fun _ _ -> []
        | Some after -> Fun_ty.apply_args_1 after
      in
      let pre, sampler, prot = function_1_adapter_pre sampler ty in
      test_function_against_generic_postcond ?gen
        ?test_stdout ?test_stderr
        ~before_reference:(pre before_reference)
        ~before_user:(pre before_user)
        ~after ?sampler (Fun_ty.apply_args_1 test) name prot (lookup_student ty name) tests

    (*----------------------------------------------------------------------------*)

    let function_2_adapter_pre sampler ty =
      let pre = function
        | None -> (fun _ -> ())
        | Some pre -> Fun_ty.apply_args_2 pre in
      let sampler = match sampler with
        | None -> None
        | Some sampler ->
           Some (fun () -> let a, b = sampler () in Fun_ty.arg a (Fun_ty.last b)) in
      let arg1_ty, ret_ty = Ty.domains ty in
      let arg2_ty, ret_ty = Ty.domains ret_ty in
      let prot = Fun_ty.arg_ty arg1_ty @@ Fun_ty.last_ty arg2_ty @@ ret_ty in
      pre, sampler, prot

    let function_2_adapter after sampler ty =
      let after = match after with
        | None -> (fun _ _ _ -> [])
        | Some after -> Fun_ty.apply_args_2 after in
      let pre, sampler, prot = function_2_adapter_pre sampler ty in
      after, pre, sampler, prot

    let test_function_2
          ?test ?test_stdout ?test_stderr
          ?before ?after ty name tests =
      let tests = List.map (fun (x, y, r, out, err) ->
        (Fun_ty.arg x @@ Fun_ty.last y, (fun () ->  output_string stdout out ; output_string stderr err ; r)))
        tests in
      let after, pre, _, prot = function_2_adapter after None ty in
      test_function
        ?test ?test_stdout ?test_stderr
        ~before:(pre before)
        ~after prot (lookup_student ty name) tests

    let test_function_2_against ?gen
          ?test ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler ty name rf tests =
      let tests = List.map (fun (x, y) -> Fun_ty.arg x @@ Fun_ty.last y) tests in
      let after, pre, sampler, prot = function_2_adapter after sampler ty in
      test_function_against ?gen
        ?test ?test_stdout ?test_stderr
        ~before_reference:(pre before_reference)
        ~before_user:(pre before_user)
        ~after ?sampler prot (lookup_student ty name) (found name rf) tests

    let test_function_2_against_solution ?gen
          ?test ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler ty name tests =
      let tests = List.map (fun (x, y) -> Fun_ty.arg x @@ Fun_ty.last y) tests in
      let after, pre, sampler, prot = function_2_adapter after sampler ty in
      test_function_against ?gen
        ?test ?test_stdout ?test_stderr
        ~before_reference:(pre before_reference)
        ~before_user:(pre before_user)
        ~after ?sampler prot (lookup_student ty name) (lookup_solution ty name) tests

    let test_function_2_against_postcond ?gen
          ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler test ty name tests =
      let tests = List.map (fun (x, y) -> Fun_ty.arg x @@ Fun_ty.last y) tests in
      let after = match after with
        | None -> (fun _ _ -> [])
        | Some after -> Fun_ty.apply_args_2 after in
      let pre, sampler, prot = function_2_adapter_pre sampler ty in
      test_function_against_generic_postcond ?gen
        ?test_stdout ?test_stderr
        ~before_reference:(pre before_reference)
        ~before_user:(pre before_user)
        ~after ?sampler (Fun_ty.apply_args_2 test) name prot (lookup_student ty name) tests

    (*----------------------------------------------------------------------------*)

    let function_3_adapter_pre sampler ty =
      let pre = function
        | None -> (fun _ -> ())
        | Some pre -> Fun_ty.apply_args_3 pre in
      let sampler = match sampler with
        | None -> None
        | Some sampler ->
           Some
             (fun () ->
               let a, b, c = sampler () in
               Fun_ty.arg a (Fun_ty.arg b (Fun_ty.last c))) in
      let arg1_ty, ret_ty = Ty.domains ty in
      let arg2_ty, ret_ty = Ty.domains ret_ty in
      let arg3_ty, ret_ty = Ty.domains ret_ty in
      let prot = Fun_ty.arg_ty arg1_ty @@ Fun_ty.arg_ty arg2_ty @@ Fun_ty.last_ty arg3_ty @@ ret_ty in
      pre, sampler, prot

    let function_3_adapter after sampler ty =
      let after = match after with
        | None -> (fun _ _ _-> [])
        | Some after -> Fun_ty.apply_args_3 after in
      let pre, sampler, prot = function_3_adapter_pre sampler ty in
      after, pre, sampler, prot

    let test_function_3
          ?test ?test_stdout ?test_stderr
          ?before ?after ty name tests =
      let tests = List.map (fun (w, x, y, r, out, err) ->
       (Fun_ty.arg w @@ Fun_ty.arg x @@ Fun_ty.last y, (fun () -> output_string stdout out ; output_string stderr err ; r)))
       tests in
      let after, pre, _, prot = function_3_adapter after None ty in
      test_function
        ?test ?test_stdout ?test_stderr
        ~before:(pre before)
        ~after prot (lookup_student ty name) tests

    let test_function_3_against ?gen
          ?test ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler ty name rf tests =
      let tests = List.map (fun (w, x, y) -> Fun_ty.arg w @@ Fun_ty.arg x @@ Fun_ty.last y) tests in
      let after, pre, sampler, prot = function_3_adapter after sampler ty in
      test_function_against ?gen
        ?test ?test_stdout ?test_stderr
        ~before_reference:(pre before_reference)
        ~before_user:(pre before_user)
        ~after ?sampler prot (lookup_student ty name) (found name rf) tests

    let test_function_3_against_solution ?gen
          ?test ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler ty name tests =
      let tests = List.map (fun (w, x, y) -> Fun_ty.arg w @@ Fun_ty.arg x @@ Fun_ty.last y) tests in
      let after, pre, sampler, prot = function_3_adapter after sampler ty in
      test_function_against ?gen
        ?test ?test_stdout ?test_stderr
        ~before_reference:(pre before_reference)
        ~before_user:(pre before_user)
        ~after ?sampler prot (lookup_student ty name) (lookup_solution ty name) tests

    let test_function_3_against_postcond ?gen
          ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler test ty name tests =
      let tests = List.map (fun (w, x, y) -> Fun_ty.arg w @@ Fun_ty.arg x @@ Fun_ty.last y) tests in
      let after = match after with
        | None -> (fun _ _ -> [])
        | Some after -> Fun_ty.apply_args_3 after in
      let pre, sampler, prot = function_3_adapter_pre sampler ty in
      test_function_against_generic_postcond ?gen
        ?test_stdout ?test_stderr
        ~before_reference:(pre before_reference)
        ~before_user:(pre before_user)
        ~after ?sampler (Fun_ty.apply_args_3 test) name prot (lookup_student ty name) tests

    (*----------------------------------------------------------------------------*)

    let function_4_adapter_pre sampler ty =
      let pre = function
        | None -> (fun _ -> ())
        | Some pre -> Fun_ty.apply_args_4 pre in
      let sampler = match sampler with
        | None -> None
        | Some sampler ->
           Some
             (fun () ->
               let a, b, c, d = sampler () in
               Fun_ty.arg a (Fun_ty.arg b (Fun_ty.arg c (Fun_ty.last d)))) in
      let arg1_ty, ret_ty = Ty.domains ty in
      let arg2_ty, ret_ty = Ty.domains ret_ty in
      let arg3_ty, ret_ty = Ty.domains ret_ty in
      let arg4_ty, ret_ty = Ty.domains ret_ty in
      let prot =
        Fun_ty.arg_ty arg1_ty @@ Fun_ty.arg_ty arg2_ty @@ Fun_ty.arg_ty arg3_ty @@
          Fun_ty.last_ty arg4_ty @@ ret_ty in
      pre, sampler, prot

    let function_4_adapter after sampler ty =
      let after = match after with
        | None -> (fun _ _ _-> [])
        | Some after -> Fun_ty.apply_args_4 after in
      let pre, sampler, prot = function_4_adapter_pre sampler ty in
      after, pre, sampler, prot

    let test_function_4
          ?test ?test_stdout ?test_stderr
          ?before ?after ty name tests =
      let tests = List.map (fun (w, x, y, z, r, out, err) ->
       (Fun_ty.arg w @@ Fun_ty.arg x @@ Fun_ty.arg y @@ Fun_ty.last z, (fun () ->
        output_string stdout out ; output_string stderr err ; r)))
        tests in
      let after, pre, _, prot = function_4_adapter after None ty in
      test_function
        ?test ?test_stdout ?test_stderr
        ~before:(pre before)
        ~after prot (lookup_student ty name) tests

    let test_function_4_against ?gen
          ?test ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler ty name rf tests =
      let tests = List.map (fun (w, x, y, z) -> Fun_ty.arg w @@ Fun_ty.arg x @@
                                                  Fun_ty.arg y @@ Fun_ty.last z) tests in
      let after, pre, sampler, prot = function_4_adapter after sampler ty in
      test_function_against ?gen
        ?test ?test_stdout ?test_stderr
        ~before_reference:(pre before_reference)
        ~before_user:(pre before_user)
        ~after ?sampler prot (lookup_student ty name) (found name rf) tests

    let test_function_4_against_solution ?gen
          ?test ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler ty name tests =
      let tests = List.map (fun (w, x, y, z) -> Fun_ty.arg w @@ Fun_ty.arg x @@
                                                  Fun_ty.arg y @@ Fun_ty.last z) tests in
      let after, pre, sampler, prot = function_4_adapter after sampler ty in
      test_function_against ?gen
        ?test ?test_stdout ?test_stderr
        ~before_reference:(pre before_reference)
        ~before_user:(pre before_user)
        ~after ?sampler prot (lookup_student ty name) (lookup_solution ty name) tests

    let test_function_4_against_postcond ?gen
          ?test_stdout ?test_stderr
          ?before_reference ?before_user ?after ?sampler test ty name tests =
      let tests = List.map (fun (w, x, y, z) -> Fun_ty.arg w @@ Fun_ty.arg x @@
                                                  Fun_ty.arg y @@ Fun_ty.last z) tests in
      let after = match after with
        | None -> (fun _ _ -> [])
        | Some after -> Fun_ty.apply_args_4 after in
      let pre, sampler, prot = function_4_adapter_pre sampler ty in
      test_function_against_generic_postcond ?gen
        ?test_stdout ?test_stderr
        ~before_reference:(pre before_reference)
        ~before_user:(pre before_user)
        ~after ?sampler (Fun_ty.apply_args_4 test) name prot (lookup_student ty name) tests

  end

  (*----------------------------------------------------------------------------*)

  module Sampler = struct
    type 'a sampler = unit -> 'a

    let sample_bool () = Random.bool ()

    let sample_int () = Random.int 10 - 5

    let sample_float () = Random.float 10. -. 5.

    let sample_char () =
      Char.chr (Random.int 26 + Char.code 'a')

    let sample_string () =
      let sample_piece () =
        [| "ba" ; "be" ; "4456" ;
           ", " ; "-" ; " " ;
           "OCaml" ; "OCP" ; "//" ; "#" |].(Random.int 10) in
      let length = Random.int 10 in
      let rec make = function 0 -> [] | n -> sample_piece () :: make (n - 1) in
      String.concat "" (make length)

    let sample_alternatively samplers =
      let samplers = Array.of_list samplers in
      let samplers =
        if Random.bool () then
          Array.concat [ samplers ; samplers ]
        else samplers in
      let cycle = Array.length samplers in
      let t = ref (cycle - 1) in
      fun () ->
      t := (!t + 1) mod cycle ;
      if !t = 0 then Array.sort (fun _ _ -> Random.int 2 * 2 - 1) samplers ;
      samplers.(!t) ()

    let sample_cases cases =
      let cases = List.map (fun case () -> case) cases in
      sample_alternatively cases

    let sample_option sample =
      let none () = None in
      let some () = Some (sample ()) in
      sample_alternatively [ none ; some ; some ; some ]

    let sample_array ?(min_size = 0) ?(max_size = 10) ?(dups = true) ?(sorted = false) sample () =
      let sample =
        if dups then sample else
          let prev = Hashtbl.create max_size in
          let rec sample_new steps =
            if steps = 0 then invalid_arg "sample_array" else
              let s = sample () in
              try Hashtbl.find prev s ; sample_new (steps - 1)
              with Not_found -> Hashtbl.add prev s () ; s in
          fun () -> sample_new 100 in
      let len = Random.int (max_size - min_size + 1) + min_size in
      let arr = Array.init len (fun _ -> sample ()) in
      if sorted then Array.sort compare arr ;
      arr

    let sample_list ?min_size ?max_size ?dups ?sorted sample () =
      Array.to_list (sample_array ?min_size ?max_size ?dups ?sorted sample ())

    let sample_pair sample1 sample2 () =
      (sample1 (), sample2 ())

    let printable_funs = ref []

    let fun_printer ppf f =
      let rec find = function
        | (f', n) :: rest ->
           if f == f' then
             Format.fprintf ppf "%s" n
           else
             find rest
        | [] -> Format.fprintf ppf "<fun>"
      in
      find !printable_funs

    let printable_fun n f =
      printable_funs := (Obj.repr f, n) :: !printable_funs ; f

    let () =
      let path = Path.Pident (Ident.create "fun_printer") in
      let ty = Typetexp.transl_type_scheme !Toploop.toplevel_env (Ty.obj [%ty: _ -> _ ]) in
      Toploop.install_printer path ty.Typedtree.ctyp_type fun_printer
  end

  let (@@@) f g = fun x -> f x @ g x
  let (@@>) r1 f = if snd (Learnocaml_report.result r1) then r1 else f ()
  let (@@=) r1 f = if snd (Learnocaml_report.result r1) then r1 else r1 @ f ()

  (**/**)
  include Ast_checker
  include Tester
  include Mutation
  include Sampler
  include Test_functions_types
  include Test_functions_ref_var
  include Test_functions_vg_var
  include Test_functions_function
  include Test_functions_generic

end

let () =
  Random.self_init ()
