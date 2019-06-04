open Parsetree
open Ast_iterator
open Asttypes

(* TODO hash node *)

let hash_string_lst accr xs=
  accr :=
    Digest.string @@
      String.concat "" xs

let hash_lst accr f self xs =
  let res =
    List.fold_left
      (fun acc x -> f self x; !accr :: acc)
      [] xs in
  hash_string_lst accr (List.sort compare res)

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
    self.payload self payload in
  let attributes self =
    hash_lst accr self.attribute self
  in
  let case self pc =
    hash_string_lst accr
      [ gaccr (self.pat self) pc.pc_lhs
      ; hash_option (gaccr (self.expr self)) pc.pc_guard
      ; gaccr (self.expr self) pc.pc_rhs
      ]
  in
  let cases self =
    hash_lst accr self.case self
  in
  let hash_expr_desc self = function
    | Pexp_ident _ -> accr := Digest.string "ident"
    | Pexp_constant _ -> accr := Digest.string "const"
    | Pexp_let (r,valbindl,e) ->
       hash_string_lst accr
         [ hash_rec_flag r
         ; gaccr (hash_lst accr self.value_binding self) valbindl
         ; gaccr (self.expr self) e
         ]
    | Pexp_function xs -> self.cases self xs
    | Pexp_fun (arg,oe,p,e) ->
       hash_string_lst accr
         [ hash_arg_label arg
         ; hash_option (gaccr (self.expr self)) oe
         ; gaccr (self.pat self) p
         ; gaccr (self.expr self) e
         ]
    | _ -> ()
  in
  let expr self e =
    hash_string_lst accr
      [ gaccr (hash_expr_desc self) e.pexp_desc;
        gaccr (self.attributes self) e.pexp_attributes
      ]
  in
  let pat self p =
    hash_string_lst accr
      [ Digest.string "pattern_desc";
        gaccr (self.attributes self) p.ppat_attributes
      ]
  in
  let signature self =
    hash_lst accr self.signature_item self in
  let signature_item _ _ = accr := Digest.string "psig_desc" in
  let structure self =
    hash_lst accr self.structure_item self in
  let structure_item self x =
      match x.pstr_desc with
      | Pstr_eval (e,a) ->
         hash_string_lst accr
           [ gaccr (self.expr self) e;
             gaccr (self.attributes self) a
           ]
      | Pstr_value (r,valbindl) ->
         hash_string_lst accr
           [ hash_rec_flag r
           ; gaccr (hash_lst accr self.value_binding self) valbindl
           ]
      | Pstr_attribute a -> self.attribute self a
      | Pstr_exception _ -> accr := Digest.string "exception"
      | _ -> accr := Digest.string "pstr_desc"
  in
  let value_binding self e =
    hash_string_lst accr
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
    ; signature
    ; signature_item
    ; structure
    ; structure_item
    ; value_binding
  }

let hash_of_struct structure =
  let accr = ref "" in
  let hi = hash_iterator accr in
  hi.structure hi structure;
  !accr

let hash_of_valbind valbindl =
  let accr = ref "" in
  let hi = hash_iterator accr in
  hash_lst accr hi.value_binding hi valbindl;
  !accr
