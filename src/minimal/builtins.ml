(* $Id: builtins.ml,v 1.23 2004/09/24 03:40:11 garrigue Exp $ *)

open Misc;;
open Common;;
open Typs;;
open Define;;
open Predef;;

(* exception handlers *)

let handlers = ref [] ;;

let handle exn =
  List.exists (fun handler -> handler exn) !handlers
;;

(* user exception *)

exception User of string;;

let minimal_fail s = raise (User (string_of_array s)) ;;

handlers :=
  [fun exn ->
    let msg =
      match exn with
	User s -> "Exception : " ^ s
      | Compile.Match_error s ->
	  if s <> "" then
	    "Match failure in " ^ s
	  else
	    "Match failure in toplevel input"
      | Not_found ->
	  "Internal : Not_found"
      | Invalid_argument "vect_item" ->
	  "Error : array access out of bounds"
      | Invalid_argument "vect_assign" ->
	  "Error : array assignment out of bounds"
      | Invalid_argument s ->
	  "Invalid argument for " ^ s
      | Division_by_zero ->
	  "Division by zero"
      | Failure s ->
	  "Failure : " ^ s
      | Sys_error s ->
          "System error : " ^ s
      | exn -> ""
    in
    if msg = "" then false else begin
      Format.printf "> %s.@." msg;
      true
    end]
;;

(* custom primitives *)

(* careful functions *)

let maxint = (-1) lsr 1 ;;
let minint = maxint + 1 ;;

let float_maxint = float_of_int maxint
and float_minint = float_of_int minint ;;

let trunc x =
  if x > float_maxint then maxint else
  if x < float_minint then minint else
  truncate x

let round x = if x < 0. then trunc (x -. 0.5) else trunc (x +. 0.5)

and sqrt x = if x < 0. then raise (Invalid_argument "sqrt") else sqrt x

and log x = if x < 0. then raise (Invalid_argument "log") else log x

and asin x =
  if x < -1. or x > 1. then raise (Invalid_argument "asin") else asin x

and acos x =
  if x < -1. or x > 1. then raise (Invalid_argument "acos") else acos x

and div_float x y =
  if y = 0. then raise Division_by_zero else x /. y
;;

(* tracing *)

module Trace = struct
open Format;;

let rec trace_obj s ty =
  match Unify.repr ty with
    Tarrow (ty1, ty2) ->
      let tracing = trace_obj (s ^ " *") ty2 in
      (fun obj -> Obj.repr (fun arg ->
	open_hovbox 2;
	print_string (s ^ " <=");
	print_space ();
	Printer.print_value ty1 arg;
	close_box ();
	print_newline ();
	let res = Obj.obj obj arg in
	open_hovbox 2;
	print_string (s ^ " * =");
	print_space ();
	Printer.print_value ty2 res;
	close_box ();
	print_newline ();
	tracing res))
  | _ -> (fun obj -> obj)
;;

let tracing_obj s ty_args ty_arg ty_res =
  let tracing = trace_obj (s ^ " ..") ty_res in
  (fun args obj -> Obj.repr(fun arg ->
    open_hovbox 2;
    print_string (s ^ " <=");
    List.iter2
      (fun arg ty -> print_space(); Printer.print_value ty arg)
      (List.rev (arg::args)) (List.rev (ty_arg::ty_args));
    close_box ();
    print_newline ();
    let res = Obj.obj obj arg in
    open_hovbox 2;
    print_string (s ^ " =>");
    print_space ();
    Printer.print_value ty_res res;
    close_box ();
    print_newline ();
    tracing res))
;;

let rec tracen_obj s n ty_args ty_res =
  match Unify.repr ty_res with
    Tarrow (ty_arg, ty_res) ->
      if n = 1 or
	(match Unify.repr ty_res with Tarrow _ -> false | _ -> true)
      then tracing_obj s ty_args ty_arg ty_res
      else begin
	let tracing = tracen_obj s (n-1) (ty_arg::ty_args) ty_res in
	(fun args obj -> Obj.repr (fun arg ->
	  let partial = Obj.obj obj arg in
	  tracing (arg::args) partial))
      end
  | _ -> failwith "builtins__tracen_obj"
;;
  
(* #close"format";; *)
end
open Trace ;;

let trace_table = ref (IdMap.empty : Obj.t IdMap.t) ;;

let trace n s =
  let s = string_of_array s in
  try
    let info = StrMap.find s !values
    and id = List.assoc s !global_idents in
    let obj_ref = IdMap.find id !global_env in
    let old_obj =
      try IdMap.find id !trace_table
      with Not_found -> !obj_ref in
    let is_function =
      match Unify.repr info.vi_type with Tarrow _ -> true | _ -> false in
    if not is_function then
      Format.printf "%s is not a function, you cannot trace it.@." s
    else
      let traced_obj =
	if n <= 1 then trace_obj s info.vi_type old_obj
	else tracen_obj s n [] info.vi_type [] old_obj in
      trace_table := IdMap.add id old_obj !trace_table;
      obj_ref := traced_obj;
      Format.printf "Function %s is now traced.@." s
  with
    Not_found ->
      Format.printf "Unknown identifier %s.@." s
;;

let untrace s =
  let s = string_of_array s in
  try
    let id = List.assoc s !global_idents in
    let obj_ref = IdMap.find id !global_env
    and old_obj = IdMap.find id !trace_table in
    obj_ref := old_obj;
    trace_table := IdMap.remove id !trace_table;
    Format.printf "Function %s will not be traced anymore.@." s
  with Not_found ->
    Format.printf "Function %s is not traced.@." s
;;

let untrace_all () =
  IdMap.iter
    (fun id old_obj ->
      let obj_ref = IdMap.find id !global_env in
      obj_ref := old_obj)
    !trace_table;
  trace_table := IdMap.empty;
  Format.printf "All functions are no longer traced.@."
;;

(* Special builtins *)

let read_file name =
  let data = ref [] in
  try
    let ic = open_in (string_of_array name) in
    begin try while true do
      data := input_line ic :: !data
    done with End_of_file ->
      close_in ic; raise Exit
    end;
    []
  with _ ->
    List.rev_map array_of_string !data

let write_file name data =
  let oc = open_out (string_of_array name) in
  List.iter
    (fun x -> output_string oc (string_of_array x); output_char oc '\n') data;
  close_out oc

let format_float pre sci (x : float) =
  let p = if pre >= 0 then "." ^ string_of_int pre else "" in
  let f = if sci then "e" else "f" in
  let fmt = Obj.magic ("%" ^ p ^ f) in
  array_of_string (Printf.sprintf fmt x)
  
(* definitions *)

let int = Tconstr(id_int,[])
and char = Tconstr(id_char,[])
and float = Tconstr(id_float,[])
and bool = Tconstr(id_bool,[])
and unit = Ttuple[]
and a = Tvar{link=None;level= -1}
and b = Tvar{link=None;level= -1}
and arr t t' = Tarrow(t,t')
and arr2 t1 t2 t = Tarrow(t1,Tarrow(t2,t))
and list a = Tconstr(id_list,[a])
and array a = Tconstr(id_array,[a])
;;

let string = array char ;;

let repr = Obj.repr ;;

let builtins = ref
  [ "+", repr (+), arr2 int int int ;
    "-", repr (-), arr2 int int int ;
    "*", repr ( * ), arr2 int int int ;
    "/", repr (/), arr2 int int int ;
    "mod", repr (mod), arr2 int int int ;
    "~", repr (~-), arr int int ;
    "lor", repr (lor), arr2 int int int ;
    "lxor", repr (lxor), arr2 int int int ;
    "land", repr (land), arr2 int int int ;
    "lsr", repr (lsr), arr2 int int int ;
    "lsl", repr (lsl), arr2 int int int ;
    "+.", repr (+.), arr2 float float float ;
    "-.", repr (-.), arr2 float float float ;
    "*.", repr ( *.), arr2 float float float ;
    "/.", repr div_float , arr2 float float float ;
    "**", repr ( ** ), arr2 float float float ;
    "~.", repr (~-.) , arr float float ;
    "=", repr (=), arr2 a a bool ;
    "<>", repr (<>), arr2 a a bool ;
    "==", repr (==), arr2 a a bool ;
    "!=", repr (!=), arr2 a a bool ;
    "<", repr (<), arr2 a a bool ;
    ">", repr (>), arr2 a a bool ;
    "<=", repr (<=), arr2 a a bool ;
    ">=", repr (>=), arr2 a a bool ;
    "&", repr (fun a b -> a & b) , arr2 bool bool bool ;
    "or", repr (fun a b -> a or b) , arr2 bool bool bool ;
    "not", repr not , arr bool bool ;
    "@", repr (@), arr2 (list a) (list a) (list a) ;
    "^", repr Array.append , arr2 (array a) (array a) (array a) ;
    ".", repr Array.get , arr2 (array a) int a ;
    ".<-", repr Array.set , arr2 (array a) int (arr a unit);
    "$.", repr Obj.field , unit ;
    "$.<-", repr Obj.set_field , unit ;
(*    "get_field", repr vect_item ;
    "set_field", repr vect_assign ; *)
    "length", repr Array.length , arr (array a) int ;
    "array", repr Array.create , arr2 int a (array a) ;
    "copy", repr Array.copy , arr (array a) (array a) ;
    "sub", repr Array.sub , arr2 (array a) int (arr int (array a));
    "map_array", repr Array.map , arr2 (arr a b) (array a) (array b);
    "do_array", repr Array.iter , arr2 (arr a b) (array a) unit ;
    "array_of_list", repr Array.of_list , arr (list a) (array a);
    "list_of_array", repr Array.to_list , arr (array a) (list a);
    "map_list", repr List.map , arr2 (arr a b) (list a) (list b);
    "do_list", repr List.iter , arr2 (arr a b) (list a) unit ;
    "raise", repr minimal_fail , arr string a ;
    "print_string", repr (Array.iter print_wchar), arr string unit;
    "newline", repr print_newline, arr unit unit;
    "read_string", repr (fun () -> array_of_string (read_line ())),
    arr unit string;
    "string_of_int", repr (fun n -> array_of_string (string_of_int n)),
    arr int string;
    "string_of_float", repr(fun x -> array_of_string (string_of_float x)),
    arr float string;
    "format_float", repr format_float, arr2 int bool (arr float string);
    "int_of_string", repr (fun n -> int_of_string (string_of_array n)),
    arr string int;
    "float_of_string", repr(fun x -> float_of_string (string_of_array x)),
    arr string float;
    "print_int", repr print_int , arr int unit ;
    "print_float", repr print_float , arr float unit ;
    "read_int", repr read_int , arr unit int ;
    "read_float", repr read_float , arr unit float;
    "code", repr repr, arr char int;
    "char", repr repr, arr int char;
    "system", repr (fun s -> Sys.command (string_of_array s)), arr string int;
    "read_file", repr read_file, arr string (list string);
    "write_file", repr write_file, arr string (arr (list string) unit);
    "time", repr Sys.time, arr unit float;
    "random_init", repr Random.init, arr int unit;
    "random_int", repr Random.int, arr int int;
    "random_float", repr Random.float, arr float float;
    "maxint", repr maxint, int;
    "minint", repr minint, int;
    "float", repr float_of_int, arr int float;
    "trunc", repr trunc, arr float int;
    "round", repr round, arr float int;
    "exp", repr exp, arr float float;
    "log", repr log, arr float float;
    "sqrt", repr sqrt, arr float float;
    "sin", repr sin, arr float float;
    "cos", repr cos, arr float float;
    "tan", repr tan, arr float float;
    "asin", repr asin, arr float float;
    "acos", repr acos, arr float float;
    "atan", repr atan, arr float float;
    "abs", repr abs_float, arr float float;
    "trace", repr (trace maxint), arr string unit;
    "tracen", repr trace, arr2 int string unit;
    "untrace", repr untrace, arr string unit;
    "untrace_all", repr untrace_all, arr unit unit;
    "quit", repr (fun () -> exit 0), arr unit unit;
    "cd", repr (fun s -> Sys.chdir (string_of_array s)), arr string unit
  ]
;;

let add_builtins () =
  List.iter
    (fun (name, obj, ty) ->
      add_value (new_id name) obj
          {vi_type = ty; vi_access = Immutable})
    !builtins ;;

