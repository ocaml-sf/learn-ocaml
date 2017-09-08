(* $Id: graph.ml,v 1.7 2004/09/24 03:40:11 garrigue Exp $ *)

open Misc;;
open Common;;
open Typs;;
open Define;;
open Builtins;;
open Graphics;;

let id_color = new_id "color";;

add_type "color" (make_ti id_color [] (Kabbrev(Tconstr(Predef.id_int,[])))) ;;

let color = Tconstr(id_color,[]) ;;

let read_coords () =
  let status = wait_next_event [Button_down] in
  (status.mouse_x, status.mouse_y)
;;

let repr = Obj.repr ;;

builtins := !builtins @
  [ "open_graph", repr (fun () -> open_graph ""), arr unit unit;
    "close_graph", repr close_graph, arr unit unit;
    "clear_graph", repr clear_graph, arr unit unit;
    "size_x", repr size_x, arr unit int;
    "size_y", repr size_y, arr unit int;
    "set_color", repr set_color, arr color unit;
    "rgb", repr rgb, arr2 int int (arr int color);
    "black", repr black, color;
    "white", repr white, color;
    "red", repr red, color;
    "green", repr green, color;
    "blue", repr blue, color;
    "yellow", repr yellow, color;
    "cyan", repr cyan, color;
    "magenta", repr magenta, color;
    "point_color", repr point_color, arr2 int int color;
    "plot", repr plot, arr2 int int unit;
    "moveto", repr moveto, arr2 int int unit;
    "current_point", repr current_point, arr unit (Ttuple[int;int]) ;
    "lineto", repr lineto, arr2 int int unit;
    "set_line_width", repr set_line_width, arr int unit;
    "draw_string", repr (fun s -> draw_string (string_of_array s)),
    		   arr string unit;
    "set_font", repr (fun s -> set_font (string_of_array s)), arr string unit;
    "set_font_size", repr set_text_size, arr int unit;
    "fill_rect", repr fill_rect, arr2 int int (arr2 int int unit);
    "read_point", repr read_coords, arr unit (Ttuple[int;int]);
    "key_pressed", repr key_pressed, arr unit bool;
    "read_key", repr read_key, arr unit char;
    "sound", repr sound, arr2 int int unit ]
;;

handlers :=
  (function
      Graphic_failure s ->
	Format.printf "> Graphics error : %s.@." s;
	true
    | _ -> false)
  :: !handlers
;;
