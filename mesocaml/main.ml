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

(* Renvoie la liste des différents Answer.t associés à exo_name *)
let get_exo_states exo_name lst : (Token.t * Answer.t) list =
  Lwt_main.run @@
    Lwt_list.filter_map_s
      (fun t ->
        Learnocaml_store.Save.get t >|=
        bindOption
          (fun x ->
            fmapOption (fun x -> t,x) @@
              SMap.find_opt exo_name Save.(x.all_exercise_states))
      )
      lst

(* Renvoie un couple où:
   - Le premier membre contient les réponses sans notes
   - Le second contient une map (note -> answer list)
*)
let partition_by_grade =
  let aux (nonlst,acc) ((_,x) as e) =
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

let print_part m =
  Printf.printf "%d class where found:\n" (IntMap.cardinal m);
  IntMap.iter
    (fun k lst ->
      Printf.printf " %d pc: %d answers, repr: %s\n" k (List.length lst) (Token.to_string @@ fst @@ List.hd lst)
    )
    m

let main sync exo_name _fun_name =
  Learnocaml_store.sync_dir := Filename.concat (Sys.getcwd ()) sync;
  let lst = get_all_token sync in
  let saves = get_exo_states exo_name lst in
  Printf.printf "%d matching repositories found.\n" (List.length saves);
  let nonlst,map = partition_by_grade saves in
  Printf.printf "%d codes was not graded.\n" (List.length nonlst);
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
