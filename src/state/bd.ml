open Yojson
open Printf
open Lwt
open Learnocaml_data

(** TODO : Ajouter "creer_index" lors de l'initialisation de lot
 ajouter "ajouter_token" lors d'une création de token **)

let sync_dir = ref (Filename.concat (Sys.getcwd ()) "sync")

(* Unlocked *)
let mutex_json_token = Lwt_mutex.create ()

let cast_list list = (`List list)

let to_json_string (value:string) = (`String value : Yojson.Basic.t)

let token_to_string liste = List.map (fun t -> Token.to_string t) liste

let string_to_token liste = List.map (fun t -> Token.parse t) liste

let get () =
      let base = !sync_dir in
      let ( / ) dir f = if dir = "" then f else Filename.concat dir f in
      let rec scan f d acc =
        let rec aux s acc =
          Lwt.catch (fun () ->
              Lwt_stream.get s >>= function
              | Some ("." | "..") -> aux s acc
              | Some x -> scan f (d / x) acc >>= aux s
              | None -> Lwt.return acc)
          @@ function
          | Unix.Unix_error (Unix.ENOTDIR, _, _) -> f d acc
          | Unix.Unix_error _ -> Lwt.return acc
          | e -> Lwt.fail e
        in
        aux (Lwt_unix.files_of_directory (base / d)) acc
      in
      scan (fun d acc ->
          let d =
            if Filename.basename d = "save.json" then Filename.dirname d
            else d
          in
          let stok = String.map (function '/' | '\\' -> '-' | c -> c) d in
          if Token.check stok then
            Lwt.return (stok :: acc)
          else
            Lwt.return acc
        ) "" []


(* Récupère le fichier demandé, le créé s'il nexiste pas *)
let get_fichier nom () = if Sys.file_exists nom
                         then Yojson.Basic.from_file nom
                         else
                           (Lwt_mutex.lock mutex_json_token;
                            close_out @@ open_out nom;
                            Lwt_mutex.unlock mutex_json_token;
                            Yojson.Basic.from_file nom)
(* Token list *)
let get_token () =
  let json = get_fichier "token.json" () in
  List.map (fun e -> Yojson.Basic.Util.to_string e) @@ Yojson.Basic.Util.to_list json

(* string List -> (`String: Yojson.Basic.t) List *)
let rec transformation_liste liste =
  match liste with
  | x::l -> to_json_string x :: transformation_liste l
  | [] -> []

(* Create index *)
let ecrire_index liste =
  let data =  cast_list @@ transformation_liste liste in
  Lwt_mutex.lock mutex_json_token >>= fun u -> u;
  let oo = open_out "token.json" in
  Yojson.Basic.pretty_to_channel oo data;
  close_out oo;
  Lwt_mutex.unlock mutex_json_token;
  Lwt.return_unit

let ajouter_token token () =
  let json_list = Yojson.Basic.Util.to_list @@ get_fichier "token.json" () in
  let token = to_json_string token in
  Lwt_mutex.lock mutex_json_token >>= fun u -> u;
  let oo = open_out "token.json" in
  Yojson.Basic.pretty_to_channel oo @@ cast_list (token::json_list);
  close_out oo;
  Lwt_mutex.unlock mutex_json_token;
  Lwt.return_unit

let creer_index = (get () >>= fun l -> ecrire_index l;)

let test () =
  creer_index >|= fun u -> u;
  (* Mettre un Lwt.ignore_result marche pas *)
  string_to_token @@ get_token ();
