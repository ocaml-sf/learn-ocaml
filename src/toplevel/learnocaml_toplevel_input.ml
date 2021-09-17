(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml
open Js_of_ocaml_tyxml

let indent_caml s in_lines =
  let output = {
    IndentPrinter.debug = false;
    config = IndentConfig.default;
    in_lines;
    indent_empty = true;
    adaptive = true;
    kind = IndentPrinter.Print (fun s acc -> acc ^ s)
  }
  in
  let stream = Nstream.of_string s in
  IndentPrinter.proceed output stream IndentBlock.empty ""

let indent_ocaml_textarea textbox =
  let rec loop s acc (i,pos') =
    try
      let pos = String.index_from s pos' '\n' in
      loop s ((i,(pos',pos))::acc) (succ i,succ pos)
    with _ -> List.rev ((i,(pos',String.length s)) :: acc) in
  let rec find (l : (int * (int * int)) list ) c =
    match l with
    | [] -> assert false
    | (i,(lo,up))::_ when up >= c -> c,i,lo,up
    | _::rem -> find rem c in
  let v = textbox##.value in
  let pos =
    let c1 = (Obj.magic textbox)##.selectionStart
    and c2 = (Obj.magic textbox)##.selectionEnd in
    if Js.Opt.test (Obj.magic c1) && Js.Opt.test (Obj.magic c2)
    then begin
      let l = loop (Js.to_string v) [] (0,0) in
      Some (find l c1,find l c2)
    end
    else None in
  let f = match pos with
    | None -> (fun _ -> true)
    | Some ((_,line1,_,_),(_,line2,_,_)) -> (fun l -> l>=(line1+1) && l<=(line2+1)) in
  let v = indent_caml (Js.to_string v) f in
  textbox##.value:=Js.string v;
  begin match pos with
    | Some ((c1,line1,_,up1),(c2,line2,_,up2)) ->
      let l = loop v [] (0,0) in
      let (lo1'',up1'') = List.assoc line1 l in
      let (lo2'',up2'') = List.assoc line2 l in
      let n1 = max (c1 + up1'' - up1) lo1'' in
      let n2 = max (c2 + up2'' - up2) lo2'' in
      let () = (Obj.magic textbox)##(setSelectionRange n1 n2) in
      textbox##focus;
      ()
    | None -> () end

type sizing =
  { line_height : int ;
    min_lines : int ;
    max_lines : int }

type input =
  { textbox : Dom_html.textAreaElement Js.t ;
    sizing : sizing option ;
    container : [ `Div ] Tyxml_js.Html5.elt ;
    mutable focused : bool ;
    mutable disabled : bool ;
    history : Learnocaml_toplevel_history.history ;
    on_resize : unit -> unit ;
    execute : string -> unit }

let disable ({ textbox ; container } as input) =
  textbox##.disabled := Js._true ;
  Js_utils.Manip.addClass container "disabled" ;
  input.disabled <- true

let enable ({ textbox ; container } as input) =
  textbox##.disabled := Js._false ;
  Js_utils.Manip.removeClass container "disabled" ;
  if input.focused then textbox##focus ;
  input.disabled <- false

let set { textbox } text =
  textbox##.value := Js.string text

let get { textbox } =
  Js.to_string textbox##.value

let resize { textbox ; sizing ; on_resize } =
  match sizing with
  | None -> ()
  | Some { line_height ; min_lines ; max_lines } ->
      on_resize () ;
      let lines =
        let text = textbox##.value in
        let res = ref 1 in
        for i = 0 to text##.length - 1 do
          if text##(charAt i) = Js.string "\n" then incr res
        done ;
        !res |> min max_lines |> max min_lines in
      textbox##.style##.fontSize := (Js.string (string_of_int line_height ^ "px")) ;
      textbox##.style##.height := Js.string (Printf.sprintf "%dpx" (line_height * lines))

let execute ({ history ; textbox ; execute } as input) =
  let code = Js.to_string textbox##.value in
  Learnocaml_toplevel_history.update history code ;
  Learnocaml_toplevel_history.push history ;
  textbox##.value := Js.string (Learnocaml_toplevel_history.current history) ;
  resize input ;
  execute code

let go_backward ({ history ; textbox } as input) =
  Learnocaml_toplevel_history.update history (Js.to_string textbox##.value) ;
  Learnocaml_toplevel_history.go_backward history ;
  textbox##.value := Js.string (Learnocaml_toplevel_history.current history) ;
  resize input

let go_forward ({ history ; textbox } as input) =
    Learnocaml_toplevel_history.update history (Js.to_string textbox##.value) ;
    Learnocaml_toplevel_history.go_forward history ;
    textbox##.value := Js.string (Learnocaml_toplevel_history.current history) ;
    resize input

let focus input = input.textbox##focus

let setup
    ?sizing ?history
    ?(on_resize = (fun () -> ()))
    ~execute:execute_callback ~container () =
  let textbox =
    Dom_html.createTextarea Dom_html.document in
  Js_utils.Manip.addClass container "toplevel-input" ;
  let history = match history with
    | None ->
        let gettimeofday () = 0. (* unsaved history *) in
        Learnocaml_toplevel_history.create ~gettimeofday ~max_size: 99 ()
    | Some history -> history in
  let input =
    { textbox ; sizing ; container ; history ; on_resize ;
      focused = false ; disabled = false ; execute = execute_callback } in
  textbox##.onkeydown := Dom_html.handler (fun e ->
      let ctrl = Js.to_bool e##.ctrlKey || Js.to_bool e##.metaKey in
      let shift = Js.to_bool e##.shiftKey in
      match e##.keyCode with
      (* Enter *)
      | 13 when not (shift || ctrl) ->
          execute input ;
          Js._false
      (* Tab *)
      | 09 when not shift ->
          indent_ocaml_textarea textbox ;
          Js._false
      (* Up arrow *)
      | 38 when ctrl ->
          go_backward input ;
          Js._false
      (* Down arrow *)
      | 40 when ctrl ->
          go_forward input ;
          Js._false
      (* Defaults *)
      | 13 ->
          resize input ;
          Js._true
      | _ -> Js._true
    );
  textbox##.onfocus :=
    Dom_html.handler (fun _ ->
        if not (input.disabled) then input.focused <- true ;
        Js._true);
  textbox##.onblur :=
    Dom_html.handler (fun _ ->
        if not (input.disabled) then input.focused <- false ;
        Js._true);
  textbox##.onkeyup :=
    Dom_html.handler (fun _ -> resize input ; Js._true);
  textbox##.onchange :=
    Dom_html.handler (fun _ -> resize input ; Js._true);
  Js_utils.Manip.replaceChildren container
    [ Tyxml_js.Of_dom.of_textArea textbox ] ;
  resize input ;
  input
