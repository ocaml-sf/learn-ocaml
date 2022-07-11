open Js_of_ocaml
open Js_of_ocaml_tyxml
open Js_utils
open Lwt.Infix
open Learnocaml_common
open Learnocaml_data.Exercise.Meta

let init_tabs, select_tab =
  mk_tab_handlers "text" ["text"; "meta"]

type encoded_token =
  {
    arg_name: string;
    raw_arg: string;
    token: Learnocaml_data.Token.t
  }

(** [get_arg_token ()] read (and decode if need be) the user token.

    @return [Some encoded_token] if a token was successfully read.
            It returns [None] if no token was specified in the URL.
            An exception is raised if an incorrect token was specified. *)
let get_encoded_token () =
  match arg "token" with (* arg in plain text, deprecated in learn-ocaml 0.13 *)
  | raw_arg ->
     let token = Learnocaml_data.Token.parse raw_arg in
     Some { arg_name = "token"; raw_arg; token }
  | exception Not_found ->
     match arg "token1" with (* encoding algo 1: space-padded token |> base64 *)
     | raw_arg ->
        begin match Base64.decode ~pad:true raw_arg with
        (* ~pad:false would work also, but ~pad:true is stricter *)
        | Ok pad_token ->
           Some { arg_name = "token1"; raw_arg;
                  token = Learnocaml_data.Token.parse (String.trim pad_token) }
        | Error (`Msg msg) -> failwith msg
        end
     | exception Not_found -> None

module Exercise_link =
  struct
    let exercise_link ?(cl = []) id content =
      match get_encoded_token () with
      | Some { arg_name; raw_arg; _ } ->
         Tyxml_js.Html5.(a ~a:[ a_href
                                  (Printf.sprintf "/description/%s#%s=%s"
                                   id arg_name raw_arg);
                                a_class cl ]
                           content)
      | None ->
         Tyxml_js.Html5.(a ~a:[ a_href "#" ; a_class cl ] content)
  end

module Display = Display_exercise(Exercise_link)
open Display

let () =
  print_string ("Test Show exo desc : 0 \n");
  run_async_with_log @@ fun () ->
    let id = match Url.Current.path with
      | "" :: "description" :: p | "description" :: p ->
         String.concat "/" (List.map Url.urldecode (List.filter ((<>) "") p))
      | _ -> arg "id" in
     Learnocaml_local_storage.init () ;
     let title_container = find_component "learnocaml-exo-tab-text-title" in
     let text_container = find_component "learnocaml-exo-tab-text-descr" in
     match get_encoded_token () with
     | Some { arg_name = _; raw_arg = _; token } -> begin
         let exercise_fetch =
           retrieve (Learnocaml_api.Exercise (Some token, id))
         in
         init_tabs ();
         exercise_fetch >>= fun (ex_meta, ex, _deadline) ->
         let exo = match ex with
           | Learnocaml_exercise.Subexercise ([], _ )  -> raise Not_found
           | Learnocaml_exercise.Subexercise ((exo, subex) :: _, _ ) -> 
             if subex.Learnocaml_exercise.student_hidden = false then exo
             else raise Not_found
           | Learnocaml_exercise.Exercise exo -> exo
         in
    	 let sub_id = exo.Learnocaml_exercise.id
    	 in
         (* display exercise questions and prelude *)
         setup_tab_text_prelude_pane Learnocaml_exercise.(decipher ~subid:sub_id false File.prelude (Learnocaml_exercise.Exercise exo));
         let prelude_container = find_component "learnocaml-exo-tab-text-prelude" in
         let iframe_container = find_component "learnocaml-exo-tab-text-iframe" in
         let text_iframe = Dom_html.createIframe Dom_html.document in
         Manip.replaceChildren title_container
           Tyxml_js.Html5.[ h1 [ txt ex_meta.title] ];
         Manip.replaceChildren iframe_container 
           [Tyxml_js.Of_dom.of_iFrame text_iframe];
         Manip.replaceChildren text_container
           [title_container;
            prelude_container;
            iframe_container ];
         Js.Opt.case
           (text_iframe##.contentDocument)
           (fun () -> failwith "cannot edit iframe document")
           (fun d ->
             d##open_;
             d##write (Js.string (exercise_text ex_meta (Learnocaml_exercise.Exercise exo)));
             d##close) ;
         (* display meta *)
         display_meta (Some token) ex_meta id >>= fun () ->
         (* hide the initial/loading phase curtain *)
         Lwt.return @@ hide_loading ~id:"learnocaml-exo-loading" ()
       end
     | None ->
       let elt = find_div_or_append_to_body "learnocaml-exo-loading" in
       Manip.(addClass elt "loading-layer") ;
       Manip.(removeClass elt "loaded") ;
       Manip.(addClass elt "loading") ;
       Manip.replaceChildren elt
         Tyxml_js.Html5.[
           h1 [ txt "Error: missing token. \
                     Use a link from ";
                (* Note: could be put in a global constant *)
                a ~a:[ a_href "https://github.com/pfitaxel/learn-ocaml.el" ]
                  [ txt "learn-ocaml-mode" ];
                txt "?" ] ];
       Lwt.return_unit
