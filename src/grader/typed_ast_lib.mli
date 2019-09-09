(* Working with Typed_ast structures *)

exception UnimplementedConstruct of string

val tast_of_parsetree_structure: Parsetree.structure -> Typed_ast.structure
val tast_of_parsetree_pattern: Parsetree.pattern -> Typed_ast.pattern
val tast_of_parsetree_expression: Parsetree.expression -> Typed_ast.expression

val parsetree_of_tast_expression: Typed_ast.expression -> Parsetree.expression

val string_of_tast_structure: ?unique_ids: bool -> Typed_ast.structure -> string
val string_of_tast_expression: ?unique_ids: bool -> Typed_ast.expression -> string

(* replace (e', e) exp replaces all instances of e occurring in exp with the
   expression e'. Since identifiers in the Typed_ast include scoping
   information, this can be used to implement substitution.
   The boolean component of the result is true if at least one replacement was
   performed, false otherwise.
 *)
val replace:
  (Typed_ast.expression * Typed_ast.expression) -> Typed_ast.expression
  -> (bool * Typed_ast.expression)

module VarSet : (Set.S with type elt = Path.t)
module StringSet : (Set.S with type elt = String.t)

(* A set of all variable names that occur in an expression, either
   free or bound.
 *)
val variable_names: Typed_ast.expression -> StringSet.t
val variables: Typed_ast.expression -> VarSet.t
val variables_bound_by_pattern: Typed_ast.pattern -> VarSet.t
val bound_variables: Typed_ast.expression -> VarSet.t
val free_variables: Typed_ast.expression -> VarSet.t

(* AST checkers for Typed_asts *)
type 'a checker = {
  expression:     'a checker -> Typed_ast.expression -> 'a list;
  pattern:        'a checker -> Typed_ast.pattern -> 'a list;
  case:           'a checker -> Typed_ast.case -> 'a list;
  cases:          'a checker -> Typed_ast.case list -> 'a list;
  module_binding: 'a checker -> Typed_ast.module_binding -> 'a list;
  module_expr:    'a checker -> Typed_ast.module_expr -> 'a list;
  structure:      'a checker -> Typed_ast.structure -> 'a list;
  structure_item: 'a checker -> Typed_ast.structure_item -> 'a list;
  value_binding:  'a checker -> Asttypes.rec_flag -> Typed_ast.value_binding -> 'a list;
  value_bindings: 'a checker -> Asttypes.rec_flag -> Typed_ast.value_binding list -> 'a list;
}

val default_checker: 'a checker
val ast_check_expr: 'a checker -> Typed_ast.expression -> 'a list
val ast_check_structure: 'a checker -> Typed_ast.structure -> 'a list

(* find_binding sstr name f finds the last toplevel binding for name
   in sstr and calls f on that value binding to produce a report.
   If no toplevel binding for name is found, produces a Failure report.
*)
val find_binding:
  Typed_ast.structure
  -> string
  -> (Asttypes.rec_flag -> Path.t -> Typed_ast.expression -> Learnocaml_report.t)
  -> Learnocaml_report.t

(* check_tailcalls id ~points expr checks that all calls to id in
   expr occur in tail position (ignoring the possibility of aliasing)
   and returns a Success report for the given number of points if so.
   If some call to id occurs in expr that is not in tail position,
   it returns a Failure report giving the parent expression containing
   the non-tailcall.
   The default value for points is 1.
*)
val check_tailcalls:
  Path.t -> ?points: int -> Typed_ast.expression -> Learnocaml_report.t

(* Looking up identifiers in the environment an expression was typed in.
   Raises Not_found if the identifier is not found in the environment.
*)
val lookup_in_expr_env: Typed_ast.expression -> string -> Path.t

(* Comparing Typed_ast fragments, ignoring some concrete syntax in
   favour of using the scoped paths *)
val same_expr: Typed_ast.expression -> Typed_ast.expression -> bool
val same_pattern : Typed_ast.pattern -> Typed_ast.pattern -> bool

(* Calculating the approximate "height" of an expression.
   This is mostly useful for determining if an expression is
   "large" or not, e.g. for style checker transformation
   purposes. *)
val expr_height: Typed_ast.expression -> int

(* Helpers for constructing Typed_ast fragments.
   The fields of these fragments are completely uninitialized:
   empty typing environment, type unit, no location or attributes, etc.
*)

val tast_of_desc: Typed_ast.expression_desc -> Typed_ast.expression
val structure_of_item: Typed_ast.structure_item -> Typed_ast.structure

(* Retrieving the path for a predefined identifier (including identifiers from
   the prelude). Useful for building Typed_ast fragments.
   Raises Not_found if the identifier is not predefined.
*)
val path_of_id: string -> Path.t

val tast_expr_of_ident: Ident.t -> Typed_ast.expression
val tast_pat_of_ident: Ident.t -> Typed_ast.pattern

val list_pat: Typed_ast.pattern list -> Typed_ast.pattern
val list_expr: Typed_ast.expression list -> Typed_ast.expression
val cons_pat: Typed_ast.pattern -> Typed_ast.pattern -> Typed_ast.pattern
val cons_expr: Typed_ast.expression -> Typed_ast.expression -> Typed_ast.expression
val apply_expr: Typed_ast.expression -> Typed_ast.expression list -> Typed_ast.expression
val match_expr: Typed_ast.expression -> Typed_ast.case list -> Typed_ast.expression

(* Helpers for performing dependency analysis *)
val depends_on: Typed_ast.structure -> Path.t -> Path.t -> bool
val dependencies: Typed_ast.structure -> Path.t -> Path.t list
val dump_deps: Typed_ast.structure -> string
