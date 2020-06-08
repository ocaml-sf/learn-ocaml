open Yojson
open Lwt
open Learnocaml_data


let token_file = "sync/token.json"

(* Unlocked *)
let mutex_token = Lwt_mutex.create ()

let cast_list l = `List l

let string_to_json (value:string) = (`String value : Yojson.Basic.t)

let token_to_string l = List.map (fun t -> Token.to_string t) l

let string_to_token l = List.map (fun t -> Token.parse t) l

let get (sync_dir : string) () =
      let base = sync_dir in
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


let write_file file mutex data =
  (Lwt_mutex.lock mutex >|= fun () ->
  let oo = open_out file in
  Yojson.Basic.pretty_to_channel oo data;
  close_out oo;
  Lwt_mutex.unlock mutex)

let create_index (sync_dir : string) =
  let l = get sync_dir () in
  let data =  l >|= List.map string_to_json >|= cast_list in
  data >>= write_file token_file mutex_token


(* if file doesn't exist, create it *)
let get_file nom (sync_dir : string) =
  if Sys.file_exists nom then begin
    try
      Lwt.return @@ Yojson.Basic.from_file nom
    with
    (* Note: this error handling could be adapted later on, to be "more conservative"? (this does not matter now, as the "sync/token.json" file is not critical and can be regenerated) *)
      Json_error _ ->  create_index sync_dir >|= fun () -> Yojson.Basic.from_file nom end
  else
    create_index sync_dir >|= fun () -> Yojson.Basic.from_file nom


(* Token list *)
let get_tokens (sync_dir : string) () =
  let json = get_file token_file sync_dir in
  json >|= Yojson.Basic.Util.to_list >|= List.map Yojson.Basic.Util.to_string >|= string_to_token


let add_token token (sync_dir : string) =
  let token = string_to_json @@ Token.to_string token in
   let json_list = get_file token_file sync_dir >|=  Yojson.Basic.Util.to_list >>= fun l -> Lwt.return @@ token::l in
   json_list >|= cast_list >>= write_file token_file mutex_token
