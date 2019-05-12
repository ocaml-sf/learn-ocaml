(* Copy of the Parsetree structure, augmented with scoping and typing
   information for the constructs we use in class.
 *)

open Asttypes

type expression = {
  sexp_desc: expression_desc;
  sexp_env: Env.t;
  sexp_type: Types.type_expr;
  sexp_loc: Location.t;
}

and expression_desc =
  | Sexp_ident of Path.t * Longident.t loc
  | Sexp_constant of Parsetree.constant
  | Sexp_let of rec_flag * value_binding list * expression
  | Sexp_function of case list
  | Sexp_fun of arg_label * expression option * pattern * expression
  | Sexp_apply of expression * (arg_label * expression) list
  | Sexp_match of expression * case list
  | Sexp_try of expression * case list
  | Sexp_tuple of expression list
  | Sexp_construct of Longident.t loc * expression option
  | Sexp_record of (Longident.t loc * expression) list * expression option
  | Sexp_field of expression * Longident.t loc
  | Sexp_setfield of expression * Longident.t loc * expression
  | Sexp_ifthenelse of expression * expression * expression option
  | Sexp_sequence of expression * expression
  | Sexp_constraint of expression * Parsetree.core_type
  | Sexp_assert of expression
  | Sexp_open of override_flag * Longident.t loc * expression

and value_binding = {
  svb_pat: pattern;
  svb_expr: expression
}

and case = {
  sc_lhs: pattern;
  sc_guard: expression option;
  sc_rhs: expression
}

and pattern =
  | Spat_any
  | Spat_var of Ident.t * string loc
  | Spat_alias of pattern * Ident.t * string loc
  | Spat_constant of Parsetree.constant
  | Spat_tuple of pattern list
  | Spat_construct of Longident.t loc * pattern option
  | Spat_record of (Longident.t loc * pattern) list * closed_flag
  | Spat_or of pattern * pattern
  | Spat_constraint of pattern * Parsetree.core_type

and module_expr =
  | Smod_ident of Longident.t loc
  | Smod_structure of structure
  | Smod_functor of string loc * Parsetree.module_type option * module_expr
  | Smod_apply of module_expr * module_expr
  | Smod_constraint of module_expr * Parsetree.module_type

and structure = {
  sstr_items: structure_item list;
  sstr_type: Types.signature;
  sstr_env: Env.t
}

and structure_item =
  | Sstr_eval of expression
  | Sstr_value of rec_flag * value_binding list
  | Sstr_primitive of Parsetree.value_description
  | Sstr_type of rec_flag * Parsetree.type_declaration list
  | Sstr_exception of Parsetree.extension_constructor
  | Sstr_module of module_binding
  | Sstr_modtype of Parsetree.module_type_declaration
  | Sstr_open of Parsetree.open_description

and module_binding = {
  smb_name: string loc;
  smb_expr: module_expr
}
