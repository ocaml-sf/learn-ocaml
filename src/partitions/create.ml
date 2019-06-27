open Learnocaml_data
open Learnocaml_data.Partition

open Learnocaml_report

open Lwt.Infix

open Monad_error

module IntMap = Map.Make(struct type t = int let compare = compare end)

(* Get the parsetree of an OCaml expression *)
let impl_of_string (s : string) : Parsetree.structure Err.t =
  let open Err in
  try ret (Parse.implementation (Lexing.from_string s))
  with
  | Lexer.Error _ | Syntaxerr.Error _ -> fail

(* Used to get the _last_ definition of a function *)
let take_until_last p =
  let rec aux = function
  | [] -> None
  | x::xs ->
     match aux xs with
     | None ->
        if p x
        then Some [x]
        else None
     | Some xs -> Some (x::xs)
  in aux

(* Type a structure with the initial environment *)
let type_with_init lst =
  try
    Compmisc.init_path true;
    let init_env = Compmisc.initial_env () in
    Err.ret (Typemod.type_structure init_env lst Location.none)
  with Typetexp.Error _ | Typecore.Error _ -> Err.fail

(* Convert a Parsetree to a Typedtree *)
let to_typed_tree (lst : Parsetree.structure) : Typedtree.structure Err.t =
  Err.map (fun (s,_,_) -> s) (type_with_init lst)

(* Get the environment after the typing of an OCaml fragment *)
let get_env str : Env.t =
  let open Err in
  let env =
      impl_of_string str
      >>= fun lst ->
      Err.map (fun (_,_,e) -> e) (type_with_init lst)
  in
  match Err.run env with
  | None -> failwith "Bad prelude"
  | Some e -> e

(* Search if a pattern has the right name *)
let has_name f x =
  let open Typedtree in
  match x.vb_pat.pat_desc with
  | Tpat_var (_,v) -> Asttypes.(v.txt) = f
  | _ -> false

(* Get the type of the last definition of f in the typedtree *)
let get_type_of_f_in_last f tree =
  let open Typedtree in
  let open Err in
  let aux acc x =
    match x.str_desc with
    | Tstr_value (_,lst) ->
       begin
         match List.find_opt (has_name f) lst with
         | None -> acc
         | Some x -> ret x.vb_expr.exp_type
       end
    | _ -> acc
  in
  List.fold_left aux fail tree.str_items

(* "Cut" a structure, with the last definition being the searched function *)
let find_func f xs : Parsetree.structure Err.t =
  let open Parsetree in
  let pred c =
    match c.pstr_desc with
    | Pstr_value (_,(x::_)) ->
       begin
         match x.pvb_pat.ppat_desc with
         | Ppat_var v -> Asttypes.(v.txt) = f
         | _ -> false
       end
    | _ -> false
  in
  Err.to_err (take_until_last pred xs)

(* Return a list of all saves with their definition of the function *)
let get_all_saves exo_name prelude fun_name =
  Learnocaml_store.Student.Index.get () >>=
    Lwt_list.fold_left_s (* filter_map_rev *)
      (fun acc t ->
        let t = t.Student.token in
        Learnocaml_store.Save.get t >|= fun save ->
        let open Err in
        maybe acc (fun x -> x :: acc) @@ run @@
          begin
          to_err save
          >>= fun x ->
          to_err (SMap.find_opt exo_name Save.(x.all_exercise_states))
          >>= fun x ->
          impl_of_string (prelude ^ "\n" ^ Answer.(x.solution))
          >>= find_func fun_name
          >>= fun r -> ret (t,x,r)
          end
      ) []

let rec last = function
  | [] -> failwith "last"
  | [x] -> x
  | _::xs -> last xs

(* Find the type of fun_name in the solution of the exercise *)
let find_sol_type prelude exo fun_name =
  let str = prelude ^ "\n"^Learnocaml_exercise.(decipher File.solution exo) in
  let open Err in
  let found_type =
    impl_of_string str
    >>= find_func fun_name
    >>= to_typed_tree
    >>= get_type_of_f_in_last fun_name in
  match run found_type with
  | None -> failwith "Required function not implemented in solution"
  | Some x -> x

(* Get the last element of a list of lambda expression *)
let rec get_last_of_seq = function
  | Lambda.Lsequence (_,u) -> get_last_of_seq u
  | x -> x

(* Convert a Typedtree.structure to a lambda expression *)
let to_lambda (lst : Typedtree.structure) =
  get_last_of_seq @@
    Lambda_utils.inline_all @@
      Simplif.simplify_lambda "" @@
        Translmod.transl_toplevel_definition lst

(* Return a tuple where
   - The first element contains answer without a grade
   - The second the others *)
let partition_WasGraded =
  let aux (nonlst,acc) ((a,x,b) as e) =
    match Answer.(x.report) with
    | None -> e::nonlst,acc
    | Some g -> nonlst,(a,g,b)::acc
  in
  List.fold_left aux ([], [])

(* Test if two types are "equal" *)
let eq_type env t1 t2 =
  try Ctype.unify env t1 t2; true with
  | Ctype.Unify _ -> false

(* Partition the codes between those who have the function with
   the right name and the right type, and the others *)
let partition_FunExist prelude_env sol_type fun_name =
  let open Err in
  let eq_type = eq_type prelude_env in
  let pred lst =
    let tree =
      to_typed_tree lst
      >>= fun t ->
      get_type_of_f_in_last fun_name t
      >>= fun x ->
      if eq_type sol_type x
      then ret (last lst, to_lambda t)
      else fail in
    run tree in
  let aux (bad,good) (n,u,x) =
    match pred x with
    | None -> (n::bad, good)
    | Some x -> (bad, (n,u,x)::good)
  in List.fold_left aux ([],[])

let partition_by_grade funname =
  let rec get_relative_section = function
    | [] -> []
    | (Message _)::xs -> get_relative_section xs
    | (Section (t,res))::xs ->
       match t with
       | Text func::Code  fn::_ ->
          if func = "Function:" && fn = funname
          then res
          else get_relative_section xs
       | _ -> get_relative_section xs
  in
  let rec get_grade xs =
    let aux acc =
      function
      | Section (_,s) -> get_grade s
      | Message (_,s) ->
         match s with
         | Success i -> acc + i
         | _ -> acc
    in
    List.fold_left aux 0 xs
  in
  let aux acc ((_,x,_) as e) =
    let sec = get_relative_section x in
    let g = get_grade sec in
    let lst =
      match IntMap.find_opt g acc with
      | None -> [e]
      | Some xs -> e::xs
    in IntMap.add g lst acc
  in
  List.fold_left aux IntMap.empty

let hm_part prof m =
  let hashtbl = Hashtbl.create 100 in
  List.iter
    (fun (t,_,(_,x)) ->
      let hash,lst = Lambda_utils.hash_lambda prof x in
      Hashtbl.add hashtbl t (hash::lst)
    ) m;
  Clustering.cluster hashtbl

exception Found of Parsetree.structure_item
let assoc_3 t lst =
  try
    List.iter (fun (t',_,(x,_)) -> if t = t' then raise (Found x) else ()) lst;
    failwith "assoc_3"
  with
  | Found x -> x

let refine_with_hm prof =
  let string_of_bindings x =
    Pprintast.string_of_structure [x] in
  IntMap.map @@
    fun x ->
    List.map
      (fold_tree
         (fun xs -> Leaf (List.map (fun u -> u,string_of_bindings (assoc_3 u x)) xs))
         (fun f a b -> Node (f,a,b)))
    (hm_part prof x)

let list_of_IntMap m =
  IntMap.fold (fun k a acc -> (k,a)::acc) m []

let partition exo_name fun_name prof =
  Learnocaml_store.Exercise.get exo_name
  >>= fun exo ->
  let prelude = Learnocaml_exercise.(access File.prelude exo) in
  let prepare = Learnocaml_exercise.(decipher File.prepare exo) in
  let prelude = prelude ^ "\n" ^ prepare in
  let prelude_env = get_env prelude in
  get_all_saves exo_name prelude fun_name
  >|= fun saves ->
  let sol_type = find_sol_type prelude exo fun_name in
  let not_graded,lst = partition_WasGraded saves in
  let not_graded = List.map (fun (x,_,_) -> x) not_graded in
  let bad_type,funexist = partition_FunExist prelude_env sol_type fun_name lst in
  let map = list_of_IntMap @@ refine_with_hm prof @@ partition_by_grade fun_name funexist in
  {not_graded; bad_type; patition_by_grade=map}
