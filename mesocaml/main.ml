open Learnocaml_data

open Lwt.Infix

module IntMap = Map.Make(struct type t = int let compare = compare end)
   
(* Return all the token in sync *)
let get_all_token sync =
  let rec aux acc f =
    let dir = Sys.readdir f in
    let dir_full = Array.map (fun s -> f ^ "/" ^ s) dir in
    (* If there is a sub-directory which does not start by "." *)
    if Array.exists (fun s -> Sys.is_directory s && not (String.contains s '.')) dir_full
    then
      Array.fold_left
        (fun acc x ->
          if Sys.is_directory x
          then aux acc x
          else acc
        ) acc dir_full
    else
      let sync_len = String.length sync + 1 in
      let string_token =
        String.concat "-" @@
          String.split_on_char '/' @@
            String.sub f sync_len (String.length f - sync_len) in
      if Token.check string_token
      then Token.parse string_token :: acc
      else acc
  in
  aux [] sync

let maybe e f = function
  | None -> e
  | Some x -> f x

let fmapOption f = function
  | None -> None
  | Some x -> Some (f x)

let bindOption f = function
  | None -> None
  | Some x -> f x

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
            if v.txt = f
            then Some (r,body)
            else None
         | _ -> None
       end
    | _ -> None
  in
  get_with_pred pred

(* Renvoie la liste des différents Answer.t associés à exo_name et fun_name *)
let get_exo_states exo_name fun_name lst : (Token.t * Answer.t * func_res) list =
  Lwt_main.run @@
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
   - Le second contient une map (note -> answer list)
*)
let partition_by_grade =
  let aux (nonlst,acc) ((_,x,_) as e) =
    match Answer.(x.grade) with
    | None -> e::nonlst,acc
    | Some g ->
       let lst =
         match IntMap.find_opt g acc with
         | None -> [e]
         | Some xs -> e::xs
       in nonlst,IntMap.add g lst acc
  in
  List.fold_left aux ([], IntMap.empty)

(*
  Sépare les codes entre ceux étants marqués comme récursifs et les autres.
*)
let refine_part_with_rec =
  IntMap.map
    (List.partition
        (fun (_,_,(isrec,_)) ->
          match isrec with
          | Asttypes.Recursive -> true
          | _ -> false
        )
    )

let fst3 (a,_,_) = a

let print_part m =
  Printf.printf "%d classes were found:\n" (IntMap.cardinal m);
  IntMap.iter
    (fun k (lstr,lstnr) ->
      let reclength = List.length lstr in
      let nonreclength = List.length lstnr in
      let sumlength = reclength + nonreclength in
      Printf.printf " %d pc: %d answers\n" k sumlength;
      if reclength != 0
      then
        Printf.printf "  %d where recursive, repr: %s\n" reclength (Token.to_string @@ fst3 @@ List.hd lstr);
      if nonreclength != 0
      then
        Printf.printf "  %d where nonrecursive, repr: %s\n" nonreclength (Token.to_string @@ fst3 @@ List.hd lstnr);
    )
    m

let main sync exo_name fun_name =
  Learnocaml_store.sync_dir := Filename.concat (Sys.getcwd ()) sync;
  let lst = get_all_token sync in
  let saves = get_exo_states exo_name fun_name lst in
  Printf.printf "%d matching repositories found.\n" (List.length saves);
  let nonlst,map = partition_by_grade saves in
  let map = refine_part_with_rec map in
  Printf.printf "%d codes were not graded.\n" (List.length nonlst);
  print_part map;
  ()

let () =
  if Array.length Sys.argv - 1 < 3
  then
    begin
      print_endline "You must provide 3 arguments: sync repo, exo name and fun name";
      exit 1
    end
  else
    main Sys.argv.(1) Sys.argv.(2) Sys.argv.(3)
