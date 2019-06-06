open Parsetree
open Ast_iterator
open Asttypes

(* TODO hash node *)

let hash_string_lst accr x xs=
  accr :=
    Digest.string @@
      String.concat "" (Digest.string x::xs)

let hash_lst accr f self x xs =
  let res =
    List.fold_left
      (fun acc x -> f self x; !accr :: acc)
      [] xs in
  hash_string_lst accr x (List.sort compare res)

let hash_lst_anon accr f self xs = hash_lst accr f self "" xs

let hash_option f =
  function
  | None -> Digest.string "None"
  | Some x -> f x

let hash_rec_flag = function
  | Recursive -> Digest.string "Recursive"
  | Nonrecursive -> Digest.string "Nonrecursive"

let hash_arg_label _ = Digest.string "arg_label" (* Important ? *)

let hash_iterator accr =
  let gaccr f x = f x; !accr in
  let attribute self (_,payload) =
    hash_string_lst accr
      "attribute"
      [gaccr (self.payload self) payload]
    in
  let attributes self =
    hash_lst accr self.attribute self "attributes"
  in
  let case self pc =
    hash_string_lst accr "case"
      [ gaccr (self.pat self) pc.pc_lhs
      ; hash_option (gaccr (self.expr self)) pc.pc_guard
      ; gaccr (self.expr self) pc.pc_rhs
      ]
  in
  let cases self =
    hash_lst accr self.case self "cases"
  in
  let hash_expr_desc self = function
    | Pexp_ident    _ -> accr := Digest.string "ident"
    | Pexp_constant _ -> accr := Digest.string "const"
    | Pexp_let (r,valbindl,e) ->
       hash_string_lst accr "let"
         [ hash_rec_flag r
         ; gaccr (hash_lst_anon accr self.value_binding self) valbindl
         ; gaccr (self.expr self) e
         ]
    | Pexp_function xs ->
       hash_string_lst accr
         "function"
         [gaccr (self.cases self) xs]
    | Pexp_fun (arg,oe,p,e) ->
       hash_string_lst accr "fun"
         [ hash_arg_label arg
         ; hash_option (gaccr (self.expr self)) oe
         ; gaccr (self.pat self) p
         ; gaccr (self.expr self) e
         ]
    | Pexp_apply (e,xs) ->
       hash_string_lst accr "apply" @@
           gaccr (self.expr self) e ::
             (List.map  (fun (_,x) -> gaccr (self.expr self) x) xs)
    | Pexp_match (e,cl) ->
       hash_string_lst accr "match"
         [ gaccr (self.expr self) e
         ; gaccr (self.cases self) cl
         ]
    | Pexp_try (e,cl) ->
       hash_string_lst accr "try"
         [ gaccr (self.expr self) e
         ; gaccr (self.cases self) cl
         ]
    | Pexp_tuple xs ->
       hash_lst accr self.expr self "tuple" xs
    | Pexp_ifthenelse (i,f,e) ->
       hash_string_lst accr "ifthenelse"
         [ gaccr (self.expr self) i
         ; gaccr (self.expr self) f
         ; hash_option (gaccr (self.expr self)) e
         ] 
    | _ ->
       accr := Digest.string "";
       raise Exit
  in
  let expr self e =
    try
    hash_string_lst accr "expr"
      [ gaccr (hash_expr_desc self) e.pexp_desc;
        gaccr (self.attributes self) e.pexp_attributes
      ]
    with Exit -> default_iterator.expr self e
  in
  let pat self p =
    hash_string_lst accr "pat"
      [ Digest.string "pattern_desc";
        gaccr (self.attributes self) p.ppat_attributes
      ]
  in
  let structure self =
    hash_lst accr self.structure_item self "structure" in
  let structure_item self x =
      match x.pstr_desc with
      | Pstr_eval (e,a) ->
         hash_string_lst accr "eval"
           [ gaccr (self.expr self) e
           ; gaccr (self.attributes self) a
           ]
      | Pstr_value (r,valbindl) ->
         hash_string_lst accr "value"
           [ hash_rec_flag r
           ; gaccr (hash_lst_anon accr self.value_binding self) valbindl
           ]
      | Pstr_attribute a -> self.attribute self a
      | Pstr_exception _ -> accr := Digest.string "exception" 
      | _ ->
         accr := "";
         default_iterator.structure_item self x
  in
  let value_binding self e =
    hash_string_lst accr "value_binding"
      [ gaccr (self.pat self) e.pvb_pat;
        gaccr (self.expr self) e.pvb_expr;
        gaccr (self.attributes self) e.pvb_attributes;
      ]
  in
  { default_iterator with
      attribute
    ; attributes
    ; case
    ; cases
    ; expr
    ; extension = attribute
    ; pat
    (* ; payload *)
    ; structure
    ; structure_item
    ; value_binding
  }

let hash_of_struct structure =
  let accr = ref "" in
  let hi = hash_iterator accr in
  hi.structure hi structure;
  !accr

let hash_of_bindings (r,v) =
  let accr = ref "" in
  let hi = hash_iterator accr in
  hi.structure_item hi {pstr_desc=(Pstr_value (r,v)); pstr_loc=Location.none};
  !accr
