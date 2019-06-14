open Learnocaml_data
open Learnocaml_data.Partition

open Learnocaml_report

open Lwt.Infix

open Utils

module IntMap = Map.Make(struct type t = int let compare = compare end)

(* Return all the token in sync *)
let get_all_token () =
  Learnocaml_store.Student.Index.get () >|= List.map (fun x -> x.Student.token)

let impl_of_string s = Parse.implementation (Lexing.from_string s)

let get_with_pred p =
  let rec aux =
  function
  | [] -> None
  | x::xs ->
     match p x with
     | None -> aux xs
     | res -> res
  in aux

type func_res = Asttypes.rec_flag * Parsetree.value_binding list

let find_func f : Parsetree.structure_item list -> func_res option =
  let open Parsetree in
  let pred c =
    match c.pstr_desc with
    | Pstr_value (r,(x::_ as body)) ->
       begin
         match x.pvb_pat.ppat_desc with
         | Ppat_var v ->
            if Asttypes.(v.txt) = f
            then Some (r,body)
            else None
         | _ -> None
       end
    | _ -> None
  in
  get_with_pred pred

(* Renvoie la liste des différents Answer.t associés à exo_name et fun_name *)
let get_exo_states exo_name fun_name lst : (Token.t * Answer.t * func_res) list Lwt.t =
  Lwt_list.filter_map_s
    (fun t ->
      Learnocaml_store.Save.get t >|=
        bindOption
          (fun x ->
            bindOption
              (fun x ->
                fmapOption
                  (fun r -> t,x,r)
                  (find_func fun_name (impl_of_string Answer.(x.solution)))
              )
              (SMap.find_opt exo_name Save.(x.all_exercise_states))
          )
    )
    lst

(* Renvoie un couple où:
   - Le premier membre contient les réponses sans notes
   - Le second contient les report des réponses notées
*)
let partition_WasGraded =
  let aux (nonlst,acc) ((a,x,b) as e) =
    match Answer.(x.report) with
    | None -> e::nonlst,acc
    | Some g -> nonlst,(a,g,b)::acc
  in
  List.fold_left aux ([], [])

let partition_FunExist fun_name =
  let pred (_,x,_) =
    let rec inner_pred =
      function
      | Message (x,_) ->
         begin
           match x with
           | Text found::Code fn::Text t::_ ->
              found = "Found" && fn = fun_name && t = "with compatible type."
           | _ -> false
         end
      | Section (_,x) -> List.exists inner_pred x
    in List.exists inner_pred x
  in List.partition pred

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

let hm_part m =
  let hashtbl = Hashtbl.create 100 in
  List.iter
    (fun (t,_,x) ->
      let hash,lst = Ast_utils.hash_of_bindings 50 x in
      Hashtbl.add hashtbl t (hash::lst)
    ) m;
  Clustering.cluster hashtbl

exception Found of func_res
let assoc_3 t lst =
  try
    List.iter (fun (t',_,x) -> if t = t' then raise (Found x) else ()) lst;
    failwith "assoc_3"
  with
  | Found x -> x

let string_of_bindings (r,xs)=
  let pstr_desc = Parsetree.Pstr_value (r,xs) in
  let pstr_loc = Location.none in
  Pprintast.string_of_structure [Parsetree.{pstr_desc;pstr_loc}]

let refine_with_hm =
  IntMap.map  @@
    fun x ->
    List.map
      (fold_tree
         (fun f a b -> Node (f,a,b))
         (fun xs -> Leaf (string_of_bindings (assoc_3 (List.hd xs) x), xs)))
    (hm_part x)

let list_of_IntMap m =
  IntMap.fold (fun k a acc -> (k,a)::acc) m []

let partition exo_name fun_name =
  get_all_token ()
  >>= get_exo_states exo_name fun_name
  >|= fun saves ->
  let not_graded,lst = partition_WasGraded saves in
  let not_graded = List.map (fun (x,_,_) -> x) not_graded in
  let funexist,bad_type = partition_FunExist fun_name lst in
  let bad_type = List.map (fun (x,_,_) -> x) bad_type in
  let map = list_of_IntMap @@ refine_with_hm @@ partition_by_grade fun_name funexist in
  {not_graded; bad_type; patition_by_grade=map}
