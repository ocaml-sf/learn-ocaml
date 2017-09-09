(* $Id: printer.ml,v 1.6 2004/09/24 00:51:16 garrigue Exp $ *)

open Format;;
open Misc;;
open Common;;
open Typs;;
open Unify;;
open Define;;
open Predef;;

let vars = ref ([] : (type_var * string) list)
and var_counter = ref 0 ;;
let reset_vars () = vars := []; var_counter := 0 ;;

let name_of_var tv =
  try
    List.assq tv !vars
  with Not_found ->
    let name =
      let n = !var_counter / 26 and c = !var_counter mod 26 in
      if n > 0 then
	String.make 1 (char_of_int (c + 97)) ^ string_of_int n
      else
	String.make 1 (char_of_int (c + 97))
    in incr var_counter;
    vars := (tv, name) :: !vars;
    name
;;

let print_list prt sep = function
    [] -> ()
  | a :: l -> prt a; List.iter (fun a -> sep (); prt a) l
;;

let print_comma () = print_string ","; print_space () ;;

let rec type_expr prio sch ty =
  match repr ty with
    Tvar tv ->
      if tv.level <> generic_level & sch then
	print_string "'_"
      else
	print_string "'";
      print_string (name_of_var tv)
  | Tarrow (ty1, ty2) ->
      if prio > 0 then print_string "(";
      open_hvbox 0;
      type_expr 1 sch ty1;
      print_string " ->";
      print_space ();
      type_expr 0 sch ty2;
      if prio > 0 then print_string ")";
      close_box ()
  | Ttuple [] ->
      print_string "unit"
  | Ttuple l ->
      if prio > 1 then print_string "(";
      open_hovbox 0;
      print_list (type_expr 2 sch)
        (fun () -> print_string" *"; print_space())
        l;
      if prio > 1 then print_string ")";
      close_box ()
  | Tconstr (id, []) ->
      print_string id.name
  | Tconstr (id1, [ty])
    when same_id id1 id_array & repr ty = Tconstr(id_char,[]) ->
      print_string "string"
  | Tconstr (id, [ty]) ->
      open_hvbox 0;
      type_expr 2 sch ty;
      print_space ();
      print_string id.name;
      close_box ()
  | Tconstr (id, l) ->
      open_hvbox 0; open_hovbox 1;
      print_string "(";
      print_list (type_expr 0 sch) print_comma l;
      print_string ")";
      close_box ();
      print_space ();
      print_string id.name;
      close_box ()
;;

let print_scheme ty =
  reset_vars ();
  type_expr 0 true ty

and print_type ty =
  type_expr 0 false ty
;;

let max_printer_depth = ref 50;;
let max_printer_steps = ref 300;;
let printer_steps = ref !max_printer_steps;;

exception Ellipsis;;

let cautious f arg = try f arg with Ellipsis -> print_string "...";;

let rec print_val prio depth ty obj =
  decr printer_steps;
  if !printer_steps <= 0 or depth <= 0 then raise Ellipsis;
  match repr ty with
    Tvar _ -> print_string "<poly>"
  | Tarrow _ -> print_string "<fun>"
  | Ttuple [] -> print_string "()"
  | Ttuple l ->
      open_hovbox 1;
      print_string "(";
      print_list
        (cautious (fun (ty,obj) -> print_val 0 (depth-1) ty obj))
        print_comma
        (List.combine l (Array.to_list (Obj.obj obj)));
      print_string ")";
      close_box ()
  | Tconstr (id, []) when same_id id id_int ->
      print_int (Obj.obj obj)
  | Tconstr (id, []) when same_id id id_char ->
      print_char '\'';
      begin match Obj.obj obj with
	  96 -> print_char '\''
	| 39 -> print_string "\\'"
	| c when c > 0xff -> print_string (string_of_array [|c|])
	| c -> print_string (Char.escaped (char_of_int c))
      end;
      print_char '\''
  | Tconstr (id, []) when same_id id id_float ->
      print_float (Obj.obj obj)
  | Tconstr (id, [ty]) when same_id id id_array ->
      begin match repr ty with
	Tconstr (id, []) when same_id id id_char ->
	  print_char '"';
	  Array.iter
	    (function
		96 -> print_char '\''
	      |	34 -> print_string "\\\""
	      | c when c > 0xff -> print_string (string_of_array [|c|])
	      |	c -> print_string (Char.escaped (char_of_int c)))
	    (Obj.obj obj : wchar array);
	  print_char '"'
      |	_ ->
	  open_hovbox 2;
	  print_string "[|";
	  cautious
	    (print_list (print_val 0 (depth-1) ty) print_comma)
	    (Array.to_list (Obj.obj obj));
	  print_string "|]";
	  close_box ()
      end
  | Tconstr (id, [ty]) when same_id id id_list ->
      open_hovbox 1;
      print_string "[";
      cautious
        (print_list (print_val 0 (depth-1) ty) print_comma)
        (Obj.obj obj);
      print_string "]";
      close_box ()
  | Tconstr (id, tyl) ->
      begin try
	let info = IdMap.find id !types_map in
	let s = List.combine info.ti_params tyl in
	match info.ti_kind with
	  Kbasic -> print_string "<abstract>"
	| Kabbrev ty ->
	    decr printer_steps;
	    print_val prio depth (subst s ty) obj
	| Kvariant l ->
	    begin try
              let (c, nc) = List.partition (fun (_,tl) -> tl = []) l in
              if Obj.is_int obj then
                let (name, _) = List.nth c (Obj.obj obj) in
                print_string name
              else
	        let (name, tyl') = List.nth nc (Obj.tag obj) in
		if prio > 0 then print_string "(";
		open_hvbox 0;
		print_string name;
		print_space ();
		begin match tyl' with
		  [ty'] ->
		    cautious (print_val 1 (depth-1) (subst s ty'))
		      (Obj.field obj 0)
		| _ ->
		    cautious (print_val 1 (depth-1) (subst s (Ttuple tyl')))
		      obj
		end;
		if prio > 0 then print_string ")";
		close_box ()
	    with Not_found ->
	      print_string "<unknown constructor>"
	    end
	| Krecord l ->
	    open_hovbox 1;
	    print_string "{";
	    print_list
	      (cautious
	       (fun ((name,ty,_),obj) ->
		 printf "%s=@;<0 0>" name;
		 print_val 0 (depth-1) (subst s ty) obj))
	      print_comma
	      (List.combine l (Array.to_list (Obj.obj obj)));
	    print_string "}";
	    close_box ()
      with Not_found ->
	print_string "<unknown type>"
      end
;;

let print_value ty obj =
    printer_steps := !max_printer_steps;
    try print_val 0 !max_printer_depth ty obj
    with x -> print_newline(); flush stderr; raise x
;;
