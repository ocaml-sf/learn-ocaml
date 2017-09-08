(* $Id: compile.ml,v 1.14 2004/09/24 00:51:15 garrigue Exp $ *)

open Misc;;
open Common;;
open Define;;
open Untyped;;

exception Match_error of string ;;

module IdSet = Set.Make(struct type t = ident let compare = compare_id end)

type var_alloc =
    { mutable size: int ;
      mutable table: int IdMap.t ;
      mutable defined: IdSet.t ;
      mutable needed: IdSet.t }
;;

let empty_alloc () =
  { size = 0 ; table = IdMap.empty ;
    defined = IdSet.empty ; needed = IdSet.empty } ;;
let new_alloc alloc =
  { size = 0 ; table = IdMap.empty ;
    defined = alloc.defined ; needed = IdSet.empty } ;;

let add_alloc alloc id =
  let index = alloc.size in
  alloc.defined <- IdSet.add id alloc.defined;
  alloc.table <- IdMap.add id index alloc.table;
  alloc.size <- index + 1;
  index
;;

let find_alloc alloc id =
  if IdSet.mem id alloc.defined then
    try IdMap.find id alloc.table
    with Not_found ->
      alloc.needed <- IdSet.add id alloc.needed;
      add_alloc alloc id
  else
    raise Not_found
;;
  
let repr_constant = function
    Cint n -> Obj.repr n
  | Cchar c -> Obj.repr c
  | Cfloat x -> Obj.repr x
;;

type pattern =
    CPid of int
  | CPint of int
  | CPconst of Obj.t
  | CPblock of int * pattern array
  | CPany
;;

let rec pattern alloc = function
    UPid id ->
      let n = add_alloc alloc id in
      CPid n
  | UPconst (Cfloat x) ->
      CPconst (Obj.repr x)
  | UPconst c ->
      CPint (Obj.obj (repr_constant c))
  | UPblock (tag, []) ->
      CPint (Obj.obj (Obj.new_block tag 0))
  | UPblock (tag, l) ->
      let cv = Array.map (pattern alloc) (Array.of_list l) in
      begin try
	let obj = Obj.new_block tag (Array.length cv) in
	for i = 0 to Array.length cv - 1 do
	  let field =
	    match cv.(i) with
		CPint x -> Obj.repr x
	      | CPconst obj -> obj
	      | _ -> raise Not_found
	  in Obj.set_field obj i field
	done;
	CPconst obj
      with
	Not_found -> CPblock (tag, cv)
      end
  | UPany ->
      CPany
;;

(* from now on we need fast vector accesses *)
module Array = struct
  include Array
  external get : 'a array -> int -> 'a = "%array_unsafe_get"
  external set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
end

let matching alloc exn pat =
  let cpat = pattern alloc pat in
  let rec match_rec = function
      CPany ->
	(fun _ _ -> ())
    | CPid n ->
	(fun env obj -> env.(n) <- obj)
    | CPint x ->
	(fun _ obj -> if x <> (Obj.obj obj) then raise exn)
    | CPconst x ->
	(fun _ obj -> if x <> obj then raise exn)
    | CPblock (tag, arr) ->
	(fun env obj ->
	  let len = Array.length arr in
	  if Obj.is_int obj or Obj.size obj <> len or Obj.tag obj <> tag
          then raise exn;
	  for i = 0 to len - 1 do
	    match_rec arr.(i) env (Obj.obj obj).(i)
	  done)
  in
  match_rec cpat
;;

exception Match_failure;;

let return obj env = obj ;;

let rec command alloc = function
    UEexpr e ->
      let code = expression alloc e in
      (fun env -> code env; ())
  | UEval (UPid id, e, err) ->
      let code = expression alloc e in
      let i = add_alloc alloc id in
      (fun env -> env.(i) <- code env)
  | UEval (pat, e, err) ->
      let mtc = matching alloc (Match_error err) pat
      and code = expression alloc e in
      (fun env -> mtc env (code env))
  | UEfun l ->
      let offset = alloc.size in
      let v = Array.of_list l in
      (* Allocate locally only if not defined globally *)
      begin try
	let orefs = Array.map (fun (id,e) -> IdMap.find id !global_env) v in
	let codes = Array.map (fun (id, e) -> expression alloc e) v
	and last = Array.length v -  1 in
	(fun env ->
	  for i = 0 to last do
	    orefs.(i) := codes.(i) env
	  done)
      with
	Not_found ->
	  Array.iter (fun (id, e) -> let _ = add_alloc alloc id in ()) v;
	  let codes = Array.map (fun (id, e) -> expression alloc e) v
	  and last = Array.length v -  1 in
	  (fun env ->
	    for i = 0 to last do
	      env.(i+offset) <- codes.(i) env
	    done)
      end
  | UEvar (id, e) ->
      let code = expression alloc e in
      let i = add_alloc alloc id in
      (fun env -> env.(i) <- code env)

and expression alloc = function
    UEid id ->
      begin try
	let i = find_alloc alloc id in
	(fun env -> env.(i))
      with
	Not_found ->
	  try
	    let oref = IdMap.find id !global_env in
	    (fun env -> !oref)
	  with
	    Not_found ->
	      failwith (id.name ^ " not found")
      end
  | UEconst c -> return (repr_constant c)
  | UEblock (tag, []) -> return (Obj.new_block tag 0)
  | UEblock (tag, l) ->
      let codes = Array.map (expression alloc) (Array.of_list l) in
      let size = Array.length codes in
      (fun env ->
	let obj = Obj.new_block tag size in
	for i = 0 to pred(size) do
	  Obj.set_field obj i (codes.(i) env)
	done;
	obj)
  | UEapply (UEid{name="&"|"or" as op}, [e1;e2]) ->
      let code1 = expression alloc e1
      and code2 = expression alloc e2 in
      if op = "&" then
	(fun env -> Obj.repr(Obj.obj(code1 env) & Obj.obj(code2 env)))
      else
	(fun env -> Obj.repr(Obj.obj(code1 env) or Obj.obj(code2 env)))
  | UEapply (e, [e1]) ->
      let code = expression alloc e
      and code1 = expression alloc e1 in
      (fun env -> (Obj.obj (code env)) (code1 env))
  | UEapply (e, [e1; e2]) ->
      let code = expression alloc e
      and code1 = expression alloc e1
      and code2 = expression alloc e2 in
      (fun env -> (Obj.obj (code env)) (code1 env) (code2 env))
  | UEapply (e, l) ->
      let codes = List.map (expression alloc) l
      and code = expression alloc e in
      let rec apply env obj = function
	  [] -> obj
	| code :: l -> apply env ((Obj.obj obj) (code env)) l
      in
      (fun env -> apply env (code env) codes)
  | UEfunct (ids, e) ->
      let alloc' = new_alloc alloc in
      List.iter (fun id -> let _ = add_alloc alloc' id in ()) ids;
      let code = expression alloc' e in
      let needed = Array.of_list (IdSet.elements alloc'.needed) in
      let source =  Array.map (find_alloc alloc) needed
      and dest = Array.map (find_alloc alloc') needed
      and last_copy = Array.length needed - 1
      and last_id = List.length ids - 1
      and size = alloc'.size in
      if last_id = 0 then
	if last_copy = -1 then
	  (Obj.magic
	   (fun env x ->
	     let new_env = (Obj.obj (Obj.new_block 0 size) : Obj.t array) in
             (* build a cell to force allocation, hence signals  *)
	     ignore (Some(new_env.(0) <- x));
	     code new_env)
	   : Obj.t array -> Obj.t)
	else
	  (Obj.magic
	   (fun env x ->
	     let new_env = (Obj.obj (Obj.new_block 0 size) : Obj.t array) in
	     ignore (Some(new_env.(0) <- x));
	     for i = 0 to last_copy do
	       new_env.(dest.(i)) <- env.(source.(i))
	     done;
	     code new_env)
	   : Obj.t array -> Obj.t)
      else
      let pred_id = pred (last_id) in
      let rec copy_arg n l env =
	if n = 0 then
	  ignore (Some(env.(0) <- l))
	else begin
	  env.(n) <- Obj.field l 0;
	  copy_arg (pred n) (Obj.field l 1) env
	end in
      let code =
	if last_copy = -1 then
	  (Obj.magic
	   (fun env l x ->
	     let new_env = (Obj.obj (Obj.new_block 0 size) : Obj.t array) in
	     new_env.(last_id) <- x; copy_arg pred_id l new_env;
	     code new_env)
	   : Obj.t array -> Obj.t -> Obj.t)
	else
	  (Obj.magic
	   (fun env l x ->
	     let new_env = (Obj.obj (Obj.new_block 0 size) : Obj.t array) in
	     new_env.(last_id) <- x; copy_arg pred_id l new_env;
	     for i = 0 to last_copy do
	       new_env.(dest.(i)) <- env.(source.(i))
	     done;
	     code new_env)
	   : Obj.t array -> Obj.t -> Obj.t)
      in
      let rec make_code n code =
	if n = 0 then
	  (Obj.magic code : Obj.t array -> Obj.t)
	else
	  make_code (pred n)
	    (Obj.magic (fun env l x -> code env (Obj.repr (x::l)))
	     : Obj.t array -> Obj.t -> Obj.t)
      in
      make_code pred_id code
  | UEseq ([], e) ->
      expression alloc e
  | UEseq (cmds1, UEseq (cmds2, e)) ->
      expression alloc (UEseq (cmds1 @ cmds2, e))
  | UEseq (cmds, e) ->
      let ccmds = Array.map (command alloc) (Array.of_list cmds) in
      let code = expression alloc e in
      let last = Array.length ccmds - 1 in
      if last = 0 then
	let ccmd = ccmds.(0) in (fun env -> ccmd env; code env)
      else
	(fun env ->
	  for i = 0 to last do ccmds.(i) env done;
	  code env)
  | UEcase (e, cases, err) ->
      let code = expression alloc e in
      let i, need_eval =
	try match e with
	  UEid id -> find_alloc alloc id, false
	| _ -> raise Not_found
	with Not_found -> add_alloc alloc (new_id "case"), true
      in
      let alloc' =
	{ defined = alloc.defined ; needed = alloc.needed ;
	  size = alloc.size ; table = alloc.table } in
      List.iter
        (fun (pat,e) ->
	  let _ = matching alloc' Match_failure pat
	  and _ = expression alloc' e in ())
        cases;
      let new_needed = IdSet.diff alloc'.needed alloc.needed in
      IdSet.iter (fun id -> let _ = find_alloc alloc id in ()) new_needed;
      let needed = alloc.needed
      and size = alloc.size in
      let max_size = ref size in
      let make_code exn (pat, e) =
	alloc.size <- size;
	let mtc = matching alloc exn pat
	and code = expression alloc e in
	max_size := max alloc.size !max_size;
	(fun env -> mtc env env.(i); code env)
      in
      let cases, last_case = split_last cases in
      let codes = List.map (make_code Match_failure) cases
      and last_code = make_code (Match_error err) last_case in
      alloc.size <- !max_size;
      if not (IdSet.equal needed alloc.needed) then failwith "compile__UEcase";
      let rec try_matching env = function
	  [] -> last_code env
	| coden :: l ->
	    try coden env with Match_failure -> try_matching env l
      in if need_eval then
	(fun env -> env.(i) <- code env; try_matching env codes)
      else
	(fun env -> try_matching env codes)
  | UEifthenelse (e, e1, e2) ->
      let code = expression alloc e
      and code1 = expression alloc e1
      and code2 = expression alloc e2 in
      (fun env ->
	if Obj.obj (code env) then code1 env else code2 env)
  | UEset (id, e) ->
      let code = expression alloc e in
      begin try
	let i = find_alloc alloc id in
	(fun env -> Obj.repr (env.(i) <- code env))
      with Not_found ->
	try
	  let oref = IdMap.find id !global_env in
	  (fun env -> Obj.repr(oref := code env))
	with
	  Not_found ->
	    failwith (id.name ^ " not found")
      end
  | UEfor (id, e1, dir, e2, e) ->
      let i = add_alloc alloc id in
      let code1 = expression alloc e1
      and code2 = expression alloc e2
      and code = expression alloc e in
      if dir = Upto then
      	(fun env -> Obj.repr(
	  for j = Obj.obj (code1 env) to Obj.obj (code2 env)
	  do
	    env.(i) <- Obj.repr j;
            (* build a cell to force allocation, hence signals  *)
	    ignore (Some(code env))
	  done))
      else
      	(fun env -> Obj.repr(
	  for j = Obj.obj (code1 env) downto Obj.obj (code2 env)
	  do
	    env.(i) <- Obj.repr j;
	    ignore (Some(code env))
	  done))
  | UEwhile (e1, e2) ->
      let code1 = expression alloc e1
      and code2 = expression alloc e2 in
      (fun env ->
	Obj.repr(while Obj.obj (code1 env) do ignore (Some(code2 env)) done))
;;

let compile_expression e =
  let alloc = empty_alloc () in
  let code = expression alloc e in
  (fun () ->
    let env = Array.make alloc.size (Obj.repr ()) in
    code env)
;;

let compile_commands ucmds idents =
  let alloc = empty_alloc () in
  let codes = List.map (command alloc) ucmds in
  let env = Array.make alloc.size (Obj.repr ()) in
  List.iter (fun code -> code env) codes;
  List.map 
    (fun id ->
      try env.(IdMap.find id alloc.table)
      with Not_found ->
	try !(IdMap.find id !global_env)
	with Not_found -> failwith "Compile.compile_commands")
    idents
;;

let prealloc_idents cmds =
  List.flatten
    (List.map
       (function UEfun l -> List.map fst l | _ -> [])
       cmds)
;;
