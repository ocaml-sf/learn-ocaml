open Yojson
open Lwt
open Learnocaml_data


let sync_dir = ref (Filename.concat (Sys.getcwd ()) "sync")

(* Unlocked *)
let mutex_json_token = Lwt_mutex.create ()

let cast_list list = (`List list)

let string_to_json (value:string) = (`String value : Yojson.Basic.t)

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


(* string List -> (`String: Yojson.Basic.t) List *)
let rec list_cast list =
  match list with
  | x::l -> string_to_json x :: list_cast l
  | [] -> []

(* Create and write index file *)
let write_index list =
  (let data =  cast_list @@ list_cast list in
  Lwt_mutex.lock mutex_json_token >|= fun () ->
  let oo = open_out "token.json" in
  Yojson.Basic.pretty_to_channel oo data;
  close_out oo;
  Lwt_mutex.unlock mutex_json_token;)

let create_index = (get () >>= write_index;)


(* if file doesn't exist, create it *)
let get_file nom () =
  if Sys.file_exists nom then(
    try
      Lwt.return @@ Yojson.Basic.from_file nom
    with
      Json_error _ ->  create_index >|= fun () -> Yojson.Basic.from_file nom)
  else
    create_index >|= fun () -> Yojson.Basic.from_file nom
 


(* Token list *)
let get_tokens () =
  let json = get_file "token.json" () in
  json >|= Yojson.Basic.Util.to_list >|= List.map (fun e -> Yojson.Basic.Util.to_string e) >|= string_to_token

let add_token token () =
  (let token = string_to_json @@ Token.to_string token in
  let json_list = get_file "token.json" () >|=  Yojson.Basic.Util.to_list >>= fun l -> Lwt.return @@ token::l in
  Lwt_mutex.lock mutex_json_token >>= fun () ->
  (let oo = open_out "token.json" in
  json_list >|= cast_list >|= Yojson.Basic.pretty_to_channel oo >|= fun () ->
  close_out oo;
  Lwt_mutex.unlock mutex_json_token;))
