open Yojson
open Lwt
open Learnocaml_data


let token_file = "token.json"

(* Unlocked *)
let mutex_token = Lwt_mutex.create ()

let cast_list l = `List l

let cast_string (value:string) = `String value

let string_to_token l = List.map Token.parse l

let ( / ) dir f = if dir = "" then f else Filename.concat dir f

let get sync_dir () =
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
        aux (Lwt_unix.files_of_directory (sync_dir / d)) acc
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
  Lwt_mutex.lock mutex >|= fun () ->
  let oo = open_out file in
  Yojson.Basic.pretty_to_channel oo data;
  close_out oo;
  Lwt_mutex.unlock mutex

let create_index sync_dir =
  (* Note: we may want to write some line in the standard output telling that
     the token index is being generated. *)
  let l = get sync_dir () in
  let data =  l >|= List.map cast_string >|= cast_list in
  data >>= write_file (sync_dir / token_file) mutex_token

let get_file name sync_dir =
  let create () =
    create_index sync_dir >|= fun () -> Yojson.Basic.from_file name
  in
  if Sys.file_exists name then begin
      try
        Lwt.return @@ Yojson.Basic.from_file name
      with
      (* Note: this error handler may be adapted later to be more conservative?
         it does not matter now as sync/token.json is not a critical file, and
         can be regenerated. *)
        Json_error _ -> create () end
  else
    create ()

let get_tokens sync_dir =
  let json = get_file (sync_dir / token_file) sync_dir in
  json >|= Yojson.Basic.Util.to_list >|=
    List.map Yojson.Basic.Util.to_string >|= string_to_token

let add_token token sync_dir =
  let token = cast_string @@ Token.to_string token in
  let json_list =
    get_file (sync_dir / token_file) sync_dir >|=
      Yojson.Basic.Util.to_list >>= fun l -> Lwt.return @@ token::l in
  json_list >|= cast_list >>= write_file (sync_dir / token_file) mutex_token
