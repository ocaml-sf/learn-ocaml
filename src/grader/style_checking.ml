module type S = sig
  module Checkers: sig
    type report_severity = Suggestion | Warning

    val stateless_style_checker:
      ?on_expression: (Typed_ast.expression -> Typed_ast_lib.checker_result list) ->
      ?on_pattern: (Typed_ast.pattern -> Typed_ast_lib.checker_result list) ->
      ?on_structure_item: (Typed_ast.structure_item -> Typed_ast_lib.checker_result list) ->
      unit -> Typed_ast_lib.checker

    val non_rewrite_report:
      Location.t -> report_severity -> string ->
      Typed_ast_lib.checker_result list
    val rewrite_report:
      ?details: string option ->
      string -> Location.t -> string -> string -> report_severity ->
      Typed_ast_lib.checker_result list
    val rewrite_report_expr:
      ?details: string option ->
      Typed_ast.expression -> Typed_ast.expression -> report_severity ->
      Typed_ast_lib.checker_result list
    val rewrite_report_vb:
      ?details: string option ->
      Asttypes.rec_flag -> Typed_ast.pattern ->
      Typed_ast.expression -> Typed_ast.expression -> report_severity ->
      Typed_ast_lib.checker_result list

    module Helpers: sig
      val not_shadowed: string -> string -> Typed_ast.expression -> bool
      val list_hd: Typed_ast.expression -> Typed_ast.expression
      val list_tl: Typed_ast.expression -> Typed_ast.expression
      val pervasives_not: Typed_ast.expression -> Typed_ast.expression
      val pervasives_or:
        Typed_ast.expression -> Typed_ast.expression -> Typed_ast.expression
      val pervasives_and:
        Typed_ast.expression -> Typed_ast.expression -> Typed_ast.expression
      val is_equals: Typed_ast.expression -> bool
      val is_append: Typed_ast.expression -> bool
      val is_empty_list: Typed_ast.expression -> bool
      val is_true: Typed_ast.expression -> bool
      val is_false: Typed_ast.expression -> bool
      val empty_list_pat: Typed_ast.pattern
      val fresh: string -> Typed_ast_lib.StringSet.t -> string
      val make_fresh_var:
        string -> Typed_ast_lib.StringSet.t ->
        Typed_ast.expression * Typed_ast.pattern
    end
  end

  val ast_style_check_structure:
    Typed_ast_lib.checker list -> Typed_ast.structure -> Learnocaml_report.t
  val all_checkers:
    ?max_match_clauses: int ->
    ?max_if_cases: int ->
    unit -> Typed_ast_lib.checker list
  val comparison_to_bool: Typed_ast_lib.checker
  val if_returning_bool: Typed_ast_lib.checker
  val list_selectors_to_match: Typed_ast_lib.checker
  val single_match_to_let: Typed_ast_lib.checker
  val unnecessary_append: Typed_ast_lib.checker
  val limit_match_clauses: int -> Typed_ast_lib.checker
  val limit_if_cases: int -> Typed_ast_lib.checker
  val eta_reduction: Typed_ast_lib.checker
end

module Make () : S = struct
  open Typed_ast
  open Typed_ast_lib

  module Checkers = struct

    type report_severity =
      | Suggestion
      | Warning

    let stateless_style_checker
        ?(on_expression = fun _ -> [])
        ?(on_pattern = fun _ -> [])
        ?(on_structure_item = fun _ -> [])
        () =
      let expression sub expr =
        (on_expression expr) @ default_checker.expression sub expr
      in
      let pattern sub pat =
        (on_pattern pat) @ default_checker.pattern sub pat
      in
      let structure_item sub item =
        (on_structure_item item) @ default_checker.structure_item sub item
      in
      {default_checker with expression; pattern; structure_item}

    let line_number loc =
      let ln = loc.Location.loc_start.Lexing.pos_lnum in
      string_of_int ln

    let non_rewrite_report loc sev msg =
      let open Learnocaml_report in
      let status = match sev with
        | Suggestion -> Informative
        | Warning -> Warning
      in
      let line = line_number loc in
      [
        loc,
        Message (
          [Text ("On line " ^ line ^ ",");
           Text msg],
          status)
      ]

    let rewrite_report ?(details = None) kind loc orig rewritten sev =
      let open Learnocaml_report in
      let status, text = match sev with
        | Suggestion -> Informative, "could also be written"
        | Warning -> Warning, "should instead be written"
      in
      let line = line_number loc in
      let text =
        [Text ("On line " ^ line ^ ", the " ^ kind);
         Code orig;
         Text (text ^ " as the following:");
         Break;
         Code rewritten]
      in
      let text =
        match details with
        | None -> text
        | Some details ->
            text @ [Break; Text "Explanation:"; Text details]
      in
      [
        loc,
        Message (text, status)
      ]

    let rewrite_report_expr ?(details = None) orig rewritten =
      rewrite_report
        ~details
        "expression"
        orig.sexp_loc
        (string_of_tast_expression orig)
        (string_of_tast_expression rewritten)

    let rewrite_report_vb ?(details = None) rf pat orig rewritten =
      let old_item = Sstr_value (rf, [{svb_pat = pat; svb_expr = orig}]) in
      let new_item = Sstr_value (rf, [{svb_pat = pat; svb_expr = rewritten}]) in
      let old_str = structure_of_item old_item in
      let new_str = structure_of_item new_item in
      rewrite_report
        ~details
        "binding"
        orig.sexp_loc
        (string_of_tast_structure old_str)
        (string_of_tast_structure new_str)

    module Helpers = struct

      let not_shadowed qualified_name name expr =
        let expected = path_of_id qualified_name in
        let actual = lookup_in_expr_env expr name in
        Path.same expected actual

      let list_hd =
        let hd = tast_of_parsetree_expression [%expr List.hd] in
        fun x -> apply_expr hd [x]

      let list_tl =
        let tl = tast_of_parsetree_expression [%expr List.tl] in
        fun x -> apply_expr tl [x]

      let pervasives_not =
        let not_ = tast_of_parsetree_expression [%expr not] in
        fun x -> apply_expr not_ [x]

      let pervasives_or =
        let or_ = tast_of_parsetree_expression [%expr (||)] in
        fun x y -> apply_expr or_ [x; y]

      let pervasives_and =
        let and_ = tast_of_parsetree_expression [%expr (&&)] in
        fun x y -> apply_expr and_ [x; y]

      (* Comparisons with some common expressions *)

      let is_equals =
        same_expr (tast_of_parsetree_expression [%expr (=)])

      let is_append =
        same_expr (tast_of_parsetree_expression [%expr (@)])

      let is_empty_list =
        same_expr (tast_of_parsetree_expression [%expr []])

      let is_true = same_expr (tast_of_parsetree_expression [%expr true])
      let is_false = same_expr (tast_of_parsetree_expression [%expr false])

      (* Commonly-used patterns for comparison and AST building purposes *)

      let empty_list_pat = tast_of_parsetree_pattern [%pat? []]

      (* Generating new variable names *)

      let fresh base vars =
        if not (StringSet.mem base vars) then
          base
        else
          let rec loop i =
            let base_i = base ^ "_" ^ (string_of_int i) in
            if not (StringSet.mem base_i vars) then
              base_i
            else loop (i + 1)
          in
          loop 0

      let make_fresh_var str var_names =
        let fresh_var_name = fresh str var_names in
        let fresh_ident = Ident.create fresh_var_name in
        let exp = tast_expr_of_ident fresh_ident in
        let pat = tast_pat_of_ident fresh_ident in
        (exp, pat)
    end
  end
  open Checkers
  open Helpers

  let map_loc f {Asttypes.txt; loc} = {Asttypes.txt = f txt; loc}
  let append_map f = List.fold_left (fun acc x -> acc @ (f x)) []

  (* Not the greatest comparison, but it works for our purposes. *)
  let compare_loc =
    let open Location in
    let open Lexing in
    fun loc1 loc2 ->
    if loc1 = loc2 then 0
    else if loc1 = none then -1
    else if loc2 = none then 1
    else
      let loc1 = loc1.loc_start in
      let loc2 = loc2.loc_start in
      if loc1.pos_lnum < loc2.pos_lnum then -1
      else if loc2.pos_lnum < loc1.pos_lnum then 1
      else compare loc1.pos_cnum loc2.pos_cnum

  (* Run all of the checkers in the list on the structure, sort the
     reports by increasing location, and return a final report *)
  let ast_style_check_structure checkers str =
    let open Learnocaml_report in
    let report =
      append_map (fun checker -> ast_check_structure checker str) checkers
    in
    let sorted_report =
      List.stable_sort
        (fun (loc1, _) (loc2, _) -> compare_loc loc1 loc2)
        report
    in
    let final_report =
      match sorted_report with
      | [] -> [Message ([Text "No style issues found"], Important)]
      | (loc, report) :: xs ->
          (* Remove adjacent duplicates and strip location tags *)
          let rec uniq l acc last_seen =
            match l with
            | [] -> acc
            | (loc1, r1) :: xs ->
                if loc1 = last_seen then
                  uniq xs acc last_seen
                else
                  uniq xs (r1 :: acc) loc1
          in
          List.rev (uniq xs [report] loc)
    in
    [Section ([Text "Style report"], final_report)]

  (* BUILT-IN STYLE CHECKERS *)

  (* Comparison to bool checker *)
  type equals_tf_result =
    | Equals_true of Typed_ast.expression
    | Equals_false of Typed_ast.expression
    | None

  let comparison_to_bool =
    let equals_true_or_false expr = match expr.sexp_desc with
      | Sexp_apply (f, [(_, v1); (_, v2)]) when is_equals f ->
          if is_true v1 then Equals_true v2
          else if is_true v2 then Equals_true v1
          else if is_false v1 then Equals_false v2
          else if is_false v2 then Equals_false v1
          else None
      | _ -> None
    in
    let on_expression expr =
      match equals_true_or_false expr with
      | Equals_true expr' -> rewrite_report_expr expr expr' Warning
      | Equals_false expr' ->
          if not_shadowed "Pervasives.not" "not" expr then
            rewrite_report_expr expr (pervasives_not expr') Warning
          else
            []
      | None -> []
    in
    stateless_style_checker ~on_expression ()

  (* If returning bool checker *)
  let if_returning_bool =
    let on_expression expr =
      match expr.sexp_desc with
      | Sexp_ifthenelse (e1, e2, Some e3) ->
          if is_true e2 then
            if is_false e3 then
              rewrite_report_expr expr e1 Warning
            else
            if not_shadowed "Pervasives.||" "||" expr then
              rewrite_report_expr expr (pervasives_or e1 e3) Suggestion
            else
              []

          else if
            is_false e2
            && is_true e3
            && not_shadowed "Pervasives.not" "not" expr
          then
            rewrite_report_expr expr (pervasives_not e1) Warning

          else if is_false e3 && not_shadowed "Pervasives.&&" "&&" expr then
            rewrite_report_expr expr (pervasives_and e1 e2) Suggestion

          else []

      | _ -> []
    in
    stateless_style_checker ~on_expression ()

  (* List selectors to match checker *)
  let list_selectors_to_match =
    let equals_empty_list expr = match expr.sexp_desc with
      | Sexp_apply (f, [(_, v1); (_, v2)]) when is_equals f ->
          if is_empty_list v1 then Some v2
          else if is_empty_list v2 then Some v1
          else None
      | _ -> None
    in
    let on_expression expr =
      match expr.sexp_desc with
      | Sexp_ifthenelse (e1, e2, Some e3) ->
          begin
            match equals_empty_list e1 with
            | Some other ->
                let var_names = variable_names e3 in
                let hd_exp, hd_pat = make_fresh_var "hd" var_names in
                let tl_exp, tl_pat = make_fresh_var "tl" var_names in
                let (replaced_hd, e3') = replace (hd_exp, list_hd other) e3 in
                let (replaced_tl, e3'') = replace (tl_exp, list_tl other) e3' in
                if not (replaced_hd || replaced_tl) then
                  []
                else
                  let empty_case =
                    {sc_lhs = empty_list_pat; sc_guard = None; sc_rhs = e2}
                  in
                  let lhs =
                    cons_pat
                      (if replaced_hd then hd_pat else Spat_any)
                      (if replaced_tl then tl_pat else Spat_any)
                  in
                  let other_case = {sc_lhs = lhs;
                                    sc_guard = None;
                                    sc_rhs = e3''}
                  in
                  let expr' = match_expr other [empty_case; other_case] in
                  rewrite_report_expr expr expr' Warning
            | None -> []
          end
      | _ -> []
    in
    stateless_style_checker ~on_expression ()

  (* Match with a single clause to let checker *)
  let single_match_to_let =
    let details =
      Some ("A match-expression with a single clause "
            ^ "can be rewritten as a let-binding.")
    in
    let on_expression expr =
      match expr.sexp_desc with
      | Sexp_match (exp, [{sc_lhs; sc_guard = None; sc_rhs}]) ->
          let vb = {svb_pat = sc_lhs; svb_expr = exp} in
          let expr' = tast_of_desc
              (Sexp_let (Asttypes.Nonrecursive, [vb], sc_rhs))
          in
          rewrite_report_expr ~details expr expr' Warning
      | _ -> []
    in
    stateless_style_checker ~on_expression ()

  (* Unnecessary append checker *)
  let unnecessary_append =
    let is_append_expr expr =
      match expr.sexp_desc with
      | Sexp_apply (f, [_; _]) -> is_append f
      | _ -> false
    in
    let is_singleton_list_opt expr =
      match expr.sexp_desc with
      | Sexp_construct
          ({Asttypes.txt = Longident.Lident "::"; _},
           Some ({sexp_desc = Sexp_tuple [x; l]; _})) ->
          if is_empty_list l then
            Some x
          else
            None
      | _ -> None
    in
    let rec expression ?(inside_append_chain = false) sub expr =
      match expr.sexp_desc with
      | Sexp_apply (f, [(_, l1); (_, l2)]) when is_append f ->
          if is_empty_list l1 then
            rewrite_report_expr expr l2 Warning
            @
            expression ~inside_append_chain: true sub l2
          else if is_empty_list l2 then
            rewrite_report_expr expr l1 Warning
            @
            expression ~inside_append_chain: true sub l1
          else begin
            match is_singleton_list_opt l1 with
            | Some x ->
                if inside_append_chain || is_append_expr l2 then
                  let expression = expression ~inside_append_chain: true in
                  let sub = {default_checker with expression} in
                  default_checker.expression sub expr
                else
                  let expr' = cons_expr x l2 in
                  rewrite_report_expr expr expr' Suggestion
                  @
                  expression ~inside_append_chain sub x
                  @
                  expression ~inside_append_chain sub l2

            | None ->
                let expression = expression ~inside_append_chain in
                let sub = {default_checker with expression} in
                default_checker.expression sub expr
          end

      (* This resets inside_append_chain to false *)
      | _ ->
          let sub = {default_checker with expression} in
          default_checker.expression sub expr
    in
    {default_checker with expression}

  (* Max number of match clauses checker *)
  let limit_match_clauses n =
    let on_expression expr =
      match expr.sexp_desc with
      | Sexp_match (_, cases) when List.length cases > n ->
          non_rewrite_report
            expr.sexp_loc
            Warning
            ("your match expression has too many clauses"
             ^ " (more than " ^ (string_of_int n) ^ ")")
      | _ -> []
    in
    stateless_style_checker ~on_expression ()

  (* Max number of if cases checker *)
  let limit_if_cases n =
    (* Note: For a conditional expression with n cases,
       this returns n - 1 *)
    let depth_of_nested_if =
      let rec aux expr acc =
        match expr.sexp_desc with
        | Sexp_ifthenelse (_, _, exp) ->
            begin
              match exp with
              | None -> acc + 1
              | Some exp -> aux exp (acc + 1)
            end
        | _ -> acc
      in
      fun expr -> aux expr 0
    in
    let rec expression ?(inside_else=false) sub expr =
      match expr.sexp_desc with
      | Sexp_ifthenelse (b, exp1, Some exp2) ->
          (* If we're inside the else branch of an if, then the
             depth of this if-expression has already been counted.
             However, we still need to recurse into the
             sub-expressions to look for other conditional expressions
             that aren't direct sub-expressions of the else. *)
          if inside_else then
            let sub1 = {default_checker with expression} in
            expression sub1 b
            @
            expression sub1 exp1
            @
            expression ~inside_else: true sub1 exp2
          else
            let report =
              let cases = depth_of_nested_if expr in
              if cases >= n then
                non_rewrite_report
                  expr.sexp_loc
                  Warning
                  ("your conditional expression has too many cases"
                   ^ " (more than " ^ (string_of_int n) ^ ")")
              else
                []
            in
            report
            @
            expression sub b
            @
            expression sub exp1
            @
            expression ~inside_else: true sub exp2

      (* This resets inside_else to false *)
      | _ ->
          let sub = {default_checker with expression} in
          default_checker.expression sub expr
    in
    {default_checker with expression}

  (* Eta reduction checker *)
  let eta_reduction =
    (* Abort when faced with a situation we can't handle yet *)
    let exception Abort in
    (* Gathering all the parameters for something like
       fun x -> fun y -> fun z -> e *)
    let rec gather_params expr =
      match expr.sexp_desc with
      | Sexp_fun (Asttypes.Nolabel, _, pat, expr) ->
          let (params, body) = gather_params expr in (pat :: params, body)
      (* Don't handle functions with labelled arguments *)
      | Sexp_fun _ -> raise Abort
      | _ -> ([], expr)
    in
    (* Converting a pattern to its equivalent expression form,
       to check if it is used as an argument in an eta-expanded form *)
    let pat_to_expr pat =
      let desc =
        match pat with
        | Spat_var (id, name) ->
            Sexp_ident (Path.Pident id, map_loc Longident.parse name)
        (* For now, only handle pattern variables *)
        | _ -> raise Abort
      in
      tast_of_desc desc
    in
    (* We need to check if the function's parameter list and
       the list of arguments to the function call in its body
       share a common suffix that can be eta-reduced out.
       Since we're going to be repeatedly dropping parameters
       from the end of the list until we find a form that
       satisfies the value restriction, it's easiest to do
       this by reversing both lists and taking a common
       prefix.
    *)
    let rec common_prefix l1 l2 =
      match l1, l2 with
      | [], _
      | _, [] -> []
      | x :: xs, y :: ys ->
          if same_expr x y then
            x :: common_prefix xs ys
          else []
    in
    (* true iff typ is the type of a function that takes at least
       one labelled/optional argument *)
    let rec is_labelled_fn typ =
      let open Types in
      match typ.desc with
      | Tarrow (Asttypes.Nolabel, _, t2, _) -> is_labelled_fn t2
      | Tarrow _ -> true
      | _ -> false
    in
    (* Drop n elements from the beginning of l.
       Precondition: l has at least n elements. *)
    let rec drop l n =
      match n, l with
      | 0, _ -> l
      | _, [] -> assert false
      | _, _ :: xs -> drop xs (n - 1)
    in
    (* Remove n parameters from the end of params
       and produce the dropped parameters (in order)
       and the new function expression.
       num_params is the length of params, provided
       because we're going to call this function with
       the same value for params repeatedly.

       e.g. remove_params [x; y; z] 3 1 body
       will produce the expression:
       fun x -> fun y -> body
       and return [z] as the list of dropped
       parameters.
    *)
    let remove_params params num_params n body =
      let rec split_at n l =
        match n, l with
        | 0, _ -> ([], l)
        | _, [] -> assert false
        | _, x :: xs ->
            let (l1, l2) = split_at (n - 1) xs in
            (x :: l1, l2)
      in
      let (params, dropped_params) = split_at (num_params - n) params in
      let func =
      List.fold_right
        (fun param body ->
           let desc = Sexp_fun (Asttypes.Nolabel, None, param, body) in
           tast_of_desc desc)
        params
        body
      in
      (dropped_params, func)
    in
    (* true iff any of the variables bound by any of the
       patterns in pats occurs in expr *)
    let occurs_in pats expr =
      let fvs = free_variables expr in
      let bvs =
        List.fold_left
          (fun acc pat -> VarSet.union acc (variables_bound_by_pattern pat))
          VarSet.empty
          pats
      in
      not (VarSet.is_empty (VarSet.inter fvs bvs))
    in
    (* true if typ contains a weakly polymorphic type
       variable. Used to check if an eta reduction results
       in a weakly polymorphic function due to the value
       restriction.
    *)
    let rec is_weakly_polymorphic typ =
      let open Types in
      match typ.desc with
      | Tvar _ -> typ.level <> Btype.generic_level
      | Tarrow (_, typ1, typ2, _) ->
          is_weakly_polymorphic typ1 || is_weakly_polymorphic typ2
      | Ttuple typs -> List.exists is_weakly_polymorphic typs
      | Tconstr (_, typs, _) -> List.exists is_weakly_polymorphic typs
      | Tlink typ -> is_weakly_polymorphic typ
      | Tsubst typ -> is_weakly_polymorphic typ
      (* The other constructors should not appear when working with
         student code, but to be careful let's say they all might
         be weakly polymorphic
      *)
      | _ -> true
    in
    (* Eta-reduce expr by factoring out as many parameters
       as possible without making the resulting function weakly
       polymorphic.

       expr is an expression of the form:
       fun params -> f args
       where rev_args is args in reverse.

       common_args is the length of the common suffix in the
       params list and the args list.
    *)
    let check_partial_application expr f rev_args params common_args =
      let env = expr.sexp_env in
      let num_params = List.length params in
      let rec aux n =
        match n with
        (* Failed at removing any common arguments *)
        | 0 -> raise Abort

        (* Try removing n common arguments *)
        | _ ->
            let args = List.rev (drop rev_args n) in
            let sapp = apply_expr f args in
            let (dropped, expr') = remove_params params num_params n sapp in

            (* If any variables bound by the dropped patterns appear
               free in f, this isn't a valid reduction *)
            if occurs_in dropped f then
              aux (n - 1)
            else
              let pexpr = parsetree_of_tast_expression expr' in
              let texp = Typecore.type_expression env pexpr in
              let typ' = texp.Typedtree.exp_type in
              if is_weakly_polymorphic typ' then
                aux (n - 1)
              else
                expr'
      in
      aux common_args
    in
    let handle_fun expr =
      let params, body = gather_params expr in
      match body.sexp_desc with
      | Sexp_apply (f, args) ->
          if is_labelled_fn (f.sexp_type) then
            raise Abort
          else
            let rev_args = List.rev_map snd args in
            let rev_param_exprs = List.rev_map pat_to_expr params in
            let common_args =
              List.length (common_prefix rev_args rev_param_exprs)
            in
            if List.compare_length_with args common_args = 0 then
              (* Function was fully applied - eta-reduction is valid *)
              f
            else
              (* Function was partially applied:
                 check if any eta-reduction results in a function that
                 is not weakly polymorphic.
                 Technically, a weakly polymorphic function could be a
                 valid rewriting if the original function was weakly
                 polymorphic. However, let's just ignore that case.
              *)
              check_partial_application expr f rev_args params common_args
      | _ -> raise Abort
    in
    let expression sub expr =
      match expr.sexp_desc with
      | Sexp_fun _ ->
          begin
            try
              let expr' = handle_fun expr in
              rewrite_report_expr expr expr' Suggestion
            with Abort -> default_checker.expression sub expr
          end
      | _ -> default_checker.expression sub expr
    in
    let value_binding sub rf ({svb_pat; svb_expr} as vb) =
      match svb_expr.sexp_desc with
      | Sexp_fun _ ->
          begin
            try
              let expr' = handle_fun svb_expr in
              rewrite_report_vb rf svb_pat svb_expr expr' Suggestion
            with Abort -> default_checker.value_binding sub rf vb
          end
      | _ -> default_checker.value_binding sub rf vb
    in
    {default_checker with expression; value_binding}

  let all_checkers
      ?(max_match_clauses = 10)
      ?(max_if_cases = 4)
      () =
    [
      list_selectors_to_match;
      eta_reduction;
      single_match_to_let;
      unnecessary_append;
      comparison_to_bool;
      if_returning_bool;
      limit_match_clauses max_match_clauses;
      limit_if_cases max_if_cases;
    ]

end
