(* This module contains several style checkers that work over
   Typed_asts. These checkers focus on mistakes involving
   inappropriate use of OCaml constructs that students learning OCaml
   (or statically-typed functional programming in general) for the
   first time often make, and many of them suggest
   semantically-equivalent expressions to replace them with.

   All checkers are scope-safe, e.g. checkers that rewrite code using
   functions from the List module will not rewrite code in scopes
   where the List module is shadowed. Names of variables introduced
   when rewriting are different from all variable names, free or
   bound, that occur in the original expression. Rewritings that
   introduce a call to a particular function will not trigger in
   scopes where that function is not in scope.

   They will also continue to work when a module has been opened in
   scope, e.g. List.tl is treated the same as tl in a scope where the
   List module has been opened, but do not recognize module aliases.

   This module must be instantiated at runtime, and then can be used
   as in the following example (made to be self-contained, but one
   would probably want to construct the Typed_ast beforehand and
   store it in a variable):

   module Style_check = Style_checking.Make ()
   Style_check.ast_style_check_structure
     (Style_check.all_checkers ())
     (Typed_ast_lib.tast_of_parsetree_structure code_ast)
*)

module type S = sig
  (* Utilities for creating custom style checkers *)
  module Checkers : sig
    (* Report severities - this affects the status used for
       the style errors in the grading report.
    *)
    type report_severity = Suggestion | Warning

    (* Generating a style error report without a suggested rewriting.
       For example, this is used for the checker that reports an
       error when a submission has an excessive amount of match
       clauses.

       non_rewrite_report loc sev msg
       generates a report flagging a style error at location loc,
       with severity sev, displaying the error message msg.
    *)
    val non_rewrite_report:
      Location.t -> report_severity -> string ->
      (Location.t * Learnocaml_report.item) list

    (* Generating a style error report with a suggested rewriting.
       This is used to implement most of the built-in style checkers.

       The functions rewrite_report_expr and rewrite_report_vb, listed
       below, should suffice for the vast majority of use cases and are
       preferred over the lower-level rewrite_report function.

       rewrite_report ~details kind loc orig rewritten sev
       generates a report flagging a style error at location loc,
       with severity sev, where orig is a string containing the
       faulty code and rewritten is the suggested rewriting of that
       code. kind designates the class of the code being flagged,
       e.g. "expression", "pattern", or "binding". If details is not
       None, it contains some extra explanation that is added to the
       end of the error message.
    *)
    val rewrite_report:
      ?details: string option ->
      string ->
      Location.t ->
      string ->
      string ->
      report_severity ->
      (Location.t * Learnocaml_report.item) list

    (* rewrite_report_expr ~details orig rewritten sev
       generates a report flagging the expression orig as containing
       a style error, with the suggested rewriting rewritten, of
       severity sev. The optional argument details can contain
       some extra explanation that is added to the end of the error
       message.
    *)
    val rewrite_report_expr:
      ?details: string option ->
      Typed_ast.expression -> Typed_ast.expression -> report_severity ->
      (Location.t * Learnocaml_report.item) list

    (* Generates a report flagging the expression position of a
       value binding as containing a style error.

       rewrite_report_vb ~details rf pat orig rewritten sev
       is used to flag a value binding with the given rec flag
       binding pattern pat with expression orig, and suggest
       that orig be rewritten as the expressoin rewritten,
       with severity sev. The optional argument details can contain
       some extra explanation that is added to the end of the error
       message.
    *)
    val rewrite_report_vb:
      ?details: string option ->
      Asttypes.rec_flag -> Typed_ast.pattern ->
      Typed_ast.expression -> Typed_ast.expression -> report_severity ->
      (Location.t * Learnocaml_report.item) list

    (* Helper functions for creating style checkers *)
    module Helpers : sig

      (* Returns true if the identifier given by qualified_name can be
         referred to by simply name in the scope of the expression expr.
         Example usage:
           not_shadowed "Pervasives.not" "not" expr
           returns true if the identifier "not" refers to the same thing
           as "Pervasives.not" in the scope of expr.
         This is useful for writing style checkers that suggest introducing
         the usage of an identifier, to make sure that identifier is still
         in scope as expected.
      *)
      val not_shadowed: string -> string -> Typed_ast.expression -> bool

      (* Helpers for generating some commonly-used function calls, for
         comparison and AST building purposes.
         Example usage:
           list_hd expr
           returns a Typed_ast fragment representing the function call
           List.hd <expr>.
      *)
      val list_hd: Typed_ast.expression -> Typed_ast.expression
      val list_tl: Typed_ast.expression -> Typed_ast.expression
      val pervasives_not: Typed_ast.expression -> Typed_ast.expression
      val pervasives_or:
        Typed_ast.expression -> Typed_ast.expression -> Typed_ast.expression
      val pervasives_and:
        Typed_ast.expression -> Typed_ast.expression -> Typed_ast.expression

      (* Helpers for AST comparison with some commonly-used expressions. *)
      val is_equals: Typed_ast.expression -> bool
      val is_append: Typed_ast.expression -> bool
      val is_empty_list: Typed_ast.expression -> bool
      val is_true: Typed_ast.expression -> bool
      val is_false: Typed_ast.expression -> bool

      (* Commonly-used patterns for comparison and AST building purposes. *)
      val empty_list_pat: Typed_ast.pattern

      (* Generates a new variable name by adding a numeric suffix to
         base. This new variable name is guaranteed unequal to any
         variable in vars.
      *)
      val fresh: string -> Typed_ast_lib.StringSet.t -> string

      (* Generates a new variable name based on str that is not
         present in var_names. Returns two Typed_ast fragments for the
         new variable in expression form and pattern form, respectively.
      *)
      val make_fresh_var:
        string -> Typed_ast_lib.StringSet.t ->
        Typed_ast.expression * Typed_ast.pattern
    end
  end

  (* Runs all of the checkers in the list on the given Typed_ast and
     outputs a final report.

     The final report is sorted by location, with the earlier warnings
     appearing first.

     Checkers should be listed in order of priority with the more
     specific/important checkers appearing earlier in the list. When
     multiple warnings are raised for a given expression, only the
     one raised by the earliest checker in the list will be reported.
  *)
  val ast_style_check_structure:
    Typed_ast_lib.checker list -> Typed_ast.structure -> Learnocaml_report.t

  val all_checkers:
    ?max_match_clauses: int ->
    ?max_if_cases: int ->
    unit -> Typed_ast_lib.checker list

  (* Eliminate comparisons to boolean values, for example:

     Original code:
     <expression> = true

     Suggested rewriting:
     <expression>

     Original code:
     <expression> = false

     Suggested rewriting:
     not <expression>
  *)
  val comparison_to_bool: Typed_ast_lib.checker

  (* Replace if statements returning boolean values when
     appropriate, for example:

     Original code:
     if <expression> then true else false

     Suggested rewriting:
     <expression>

     Original code:
     if <expression> then false else true
     Suggested rewriting:
     not <expression>

     Original code:
     if <e1> then true else <e2>

     Suggested rewriting:
     <e1> || <e2>
  *)
  val if_returning_bool: Typed_ast_lib.checker

  (* Replace use of explicit list structure selectors with pattern
     matching, for example:

     Original code:
     if <list-expression> = [] then
       <base-case-expression>
     else
       <expression using (List.hd <list-expression>)
        and (List.tl <list-expression>)>

     Suggested rewriting:
     match <list-expression> with
     | [] -> <base-case-expression>
     | hd :: tl ->
       <expression with (List.hd <list-expression>)
        replaced by hd and (List.tl <list-expression>)
        replaced by tl>
  *)
  val list_selectors_to_match: Typed_ast_lib.checker

  (* Rewrite match expressions with a single case to
     let expressions, for example:

     Original code:
     match p with
     | (x, y) -> <expression using x and y>

     Suggested rewriting:
     let (x, y) = p in <expression using x and y>
  *)
  val single_match_to_let: Typed_ast_lib.checker

  (* Rewrite unnecessary usages of append, that could
     be replaced with cons or removed entirely, for example:

     Original code:
     l @ []

     Suggested rewriting:
     l

     Original code:
     [x] @ l

     Suggested rewriting:
     x :: l

     Does not suggest replacing append with cons when
     inside a chain of appends, e.g. l1 @ [x] @ l2.
  *)
  val unnecessary_append: Typed_ast_lib.checker

  (* limit_match_clauses n produces a checker that
     warns for match expressions with strictly more than
     n clauses. *)
  val limit_match_clauses: int -> Typed_ast_lib.checker

  (* limit_if_depth n produces a checker that warns
     for conditional expressions with strictly more
     than n cases.

     For example, the number of cases in the following
     conditional expression is n + 1:

     if b1 then e1
     else if b2 then e2
     else if b3 then e3
     ...
     else if bn then en
     else e

     Only conditions expressions of this form are
     considered, i.e. the contents of the then-branch
     are not counted in the nesting depth.
  *)
  val limit_if_cases: int -> Typed_ast_lib.checker

  (* Performs eta-reduction on function expressions.
     Suggested eta-reductions respect the relaxed value
     restriction, and as many parameters as possible will
     be factored out while preserving the level of
     polymorphism in the original function.
     Functions that are already weakly polymorphic will
     not be eta-reduced.

     For example:

     Original expression:
     let add x y = x + y

     Suggested rewriting:
     let add = (+)

     Original expression:
     let compose l x = List.fold_right (@@) l x

     Suggested rewriting:
     let compose l = List.fold_right (@@) l
  *)
  val eta_reduction: Typed_ast_lib.checker

end

(* Everything involving Typed_asts needs to be done at runtime, so
   that typechecking can be done in the proper Toplevel
   environment. We can instantiate the module at runtime by creating
   it with a functor.
*)
module Make () : S
