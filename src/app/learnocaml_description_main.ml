open Js_utils
open Lwt.Infix
open Learnocaml_common
open Learnocaml_data.Exercise.Meta

let init_tabs, select_tab =
  mk_tab_handlers "text" ["text"; "meta"]

module Exercise_link =
  struct
    let exercise_link ?(cl = []) id content =
      let token = Learnocaml_data.Token.(to_string (parse (arg "token"))) in
      Tyxml_js.Html5.(a ~a:[ a_href ("/description/"^id^"#token="^token);
                             a_class cl ]
                        content)
  end
  
module Display = Display_exercise(Exercise_link)    
open Display
       
let () =
  run_async_with_log @@ fun () ->
    let id = match Url.Current.path with
      | "" :: "description" :: p | "description" :: p ->
         String.concat "/" (List.map Url.urldecode (List.filter ((<>) "") p))
      | _ -> arg "id" in
     Learnocaml_local_storage.init () ;
     let text_container = find_component "learnocaml-exo-tab-text" in
     try begin
         let token = Learnocaml_data.Token.parse (arg "token") in
         let exercise_fetch =
           retrieve (Learnocaml_api.Exercise (Some token, id))
         in
         init_tabs ();
         exercise_fetch >>= fun (ex_meta, exo, _deadline) ->
         (* display exercise questions *)
         let text_iframe = Dom_html.createIframe Dom_html.document in
         Manip.replaceChildren text_container
           Tyxml_js.Html5.[ h1 [ pcdata ex_meta.title ] ;
                            Tyxml_js.Of_dom.of_iFrame text_iframe ] ;
         Js.Opt.case
           (text_iframe##.contentDocument)
           (fun () -> failwith "cannot edit iframe document")
           (fun d ->
             d##open_;
             d##write (Js.string (exercise_text ex_meta exo));
             d##close) ;
         (* display meta *)
         display_meta (Some token) ex_meta id
       end
     with Not_found ->
       Lwt.return @@
         Manip.replaceChildren text_container
           Tyxml_js.Html5.[ h1 [ pcdata "Error: Missing token" ] ]
