(*
	##########################
	#        SETTINGS        #
	##########################
*)
let p = [|[|1;1;1;2;2|]|]
let a = [|[|1;1;2;2;2|]; [|1;1;2;2;3|]|]
let e = [|[|2;2;3;3;3|]; [|2;2;2;3;3|]|]



(*
	#################################
	#    DATA TYPES & EXCEPTIONS    #
	#################################
*)
type difficulty = | Basic | Intermediate | Consolidated


type exercise = {
	title         : string;
	stars         : difficulty;
	id            : float;
	requirements  : float list
}


type student = {
	token : Learnocaml_data.Token.Map.key;
	level : int
}


exception Invalid_argument of string
exception No_more_exercises 



(*
	##########################
	#     AUX FUNCTIONS      #
	##########################
*)
let diff_to_int = function
  | Basic         -> 1
  | Intermediate  -> 2
  | _             -> 3


let float_to_diff = function
	| x when x < 2. -> Basic
	| x when x < 3. -> Intermediate
	| _             -> Consolidated


let htbl_to_array htbl = 
	Array.of_list (Hashtbl.fold (fun k _ acc -> k::acc) htbl [])

	
let create_student t lvl =
	{
		token = t;
		level = 
			match lvl with
			| "1st" -> 1
			| "2nd" -> 2
			| _     -> 3
	}


let create_htbls availabel_exercises = 
	let basic         = Hashtbl.create 10 in
	let intermediate  = Hashtbl.create 10 in
	let consolidated  = Hashtbl.create 10 in
	let add_exe_to_htbl exe = 
		let htbl = (
			match exe.stars with
			| Basic         -> basic
			| Intermediate  -> intermediate 
			| Consolidated  -> consolidated
		) in
		Hashtbl.add htbl (exe.id) exe
	in
	let rec sort_exercises = function
		| []    -> ()
		| exe::t  -> add_exe_to_htbl exe; sort_exercises t
	in
	sort_exercises availabel_exercises;
	basic, intermediate, consolidated
	

let gen_assignment (basic, intermediate, consolidated) level =
	let basic_a         = htbl_to_array basic in
	let intermediate_a  = htbl_to_array intermediate in
	let consolidated_a  = htbl_to_array consolidated in
	let assignment  		= 
		match level with
		| 1 -> p.(Random.int (Array.length p))
		| 2 -> a.(Random.int (Array.length a))
		| _ -> e.(Random.int (Array.length e))
	in
	let init_assignment () = 
		let rec init_aux b i c idx =
			if (idx < (Array.length assignment)) then
				match assignment.(idx) with
				| 1 -> if b > 0 then init_aux (b-1) i c (idx+1)
					else if i > 0 then (assignment.(idx) <- 2; init_aux b (i-1) c (idx+1))
					else (assignment.(idx) <- 3; init_aux b i (c-1) (idx+1))
				| 2 -> if i > 0 then init_aux b (i-1) c (idx+1)
					else if b > 0 then (assignment.(idx) <- 1; init_aux (b-1) i c (idx+1))
					else (assignment.(idx) <- 3; init_aux b i (c-1) (idx+1))
				| _ -> if c > 0 then init_aux b i (c-1) (idx+1)
					else if i > 0 then (assignment.(idx) <- 2; init_aux b (i-1) c (idx+1))
					else (assignment.(idx) <- 1; init_aux (b-1) i c (idx+1))
		in
		init_aux (Array.length basic_a) (Array.length intermediate_a) (Array.length consolidated_a) 0 
	in
	let diff_to_exercise a = 
		let r = Array.make 6 "" in
		let shuffle_a v = 
			let n = Array.length v in
			let v = Array.copy v in
			for i = n - 1 downto 1 do
				let k = Random.int (i+1) in
				let x = v.(k) in
				v.(k) <- v.(i);
				v.(i) <- x
			done;
			v
		in
		let b_a = shuffle_a basic_a in
		let i_a = shuffle_a intermediate_a in
		let c_a = shuffle_a consolidated_a in
    let get_requirement (k : float) =
      match Hashtbl.find_opt basic k with
      | Some x  -> x.requirements
      | None    ->
        match Hashtbl.find_opt intermediate k with
        | Some x  -> x.requirements
        | None    -> (Hashtbl.find consolidated k).requirements
    in
		let rec translate_aux b i c req idx = 
			if (idx >= 0) then
				match a.(idx) with
				| 1 -> (
          let new_req = get_requirement b_a.(b) in 
          r.(idx) <- (Hashtbl.find basic b_a.(b)).title; 
          translate_aux (b+1) i c (if new_req <> [] then ((idx,List.hd new_req)::req) else req) (idx - 1))
				| 2 -> (
          let new_req = get_requirement i_a.(i) in 
          r.(idx) <- (Hashtbl.find intermediate i_a.(i)).title; 
          translate_aux b (i+1) c (if new_req <> [] then ((idx,List.hd new_req)::req) else req) (idx - 1))
				| _ -> (
          let new_req = get_requirement c_a.(c) in 
          r.(idx) <- (Hashtbl.find consolidated c_a.(c)).title; 
          translate_aux b i (c+1) (if new_req <> [] then ((idx,List.hd new_req)::req) else req) (idx - 1))
      else b,i,c,req
		in
    let b,i,c,req = translate_aux 0 0 0 [] ((Array.length a)-1) in
    let changed   = Array.make 5 0 in 
    let find_place d = 
      let d = diff_to_int d in
      let rec aux id =
        if (id >= 0) then (
          if (changed.(id) = 0) then
            match d with
            | 1 -> id
            | 2 -> id
            | _ -> id
          else aux (id-1)
        )
        else -1 
      in
      aux 4 
    in
    let change_exercise idx =
      let d = a.(idx) in
      match d with
      | 1 -> if b < ((Array.length b_a)-1) then b+1 else raise No_more_exercises
      | 2 -> if i < ((Array.length i_a)-1) then i+1 else raise No_more_exercises
      | _ -> if c < ((Array.length c_a)-1) then c+1 else raise No_more_exercises
    in
    let rec fill_requirements = function 
      | []                  -> ()
      | (idx, exe_id)::tl ->
        let {title;stars;id;requirements} = Hashtbl.find (match a.(idx) with | 1 -> basic | 2 -> intermediate | _ ->
          consolidated) exe_id in
        if Array.mem title r then fill_requirements tl 
        else ( 
          let new_id = find_place stars in
          changed.(idx) <- 1;
          if new_id <> -1 then (
            let new_req = get_requirement id in 
            r.(new_id) <- title; 
            changed.(new_id) <- 1;
            fill_requirements (if new_req <> [] then ((new_id, List.hd new_req)::tl) else tl)
          )
          else (
            if r.(5) = "" then (
              let new_req = get_requirement id in 
              r.(5) <- title; 
              fill_requirements (if new_req <> [] then ((5, List.hd new_req)::tl) else tl)
            )
            else
              try
                let new_id = change_exercise idx in
                let new_req = get_requirement (match a.(idx) with | 1 -> b_a.(new_id) | 2 -> i_a.(new_id) | _ -> c_a.(new_id)) in 
                r.(idx) <-  (match a.(idx) with | 1 -> Hashtbl.find basic b_a.(new_id) | 2 -> Hashtbl.find intermediate i_a.(new_id) | _ -> Hashtbl.find consolidated c_a.(new_id)).title;
                fill_requirements (if new_req <> [] then ((idx, List.hd new_req)::tl) else tl)
              with _ -> ()
          )
        )
    in
    fill_requirements req;
    r
	in
	init_assignment (); 
	Array.fast_sort compare assignment;
	let assignment = diff_to_exercise assignment in
  let assignment = if assignment.(5) = "" then Array.sub assignment 0 5 else assignment in
	Array.to_list assignment
	


(*
	##########################
	#          MAIN          #
	##########################
*)
let iter_tokens tokens availabel_exercises = 
	let tbl = create_htbls availabel_exercises in
	let assignments = Hashtbl.create 25 in
	let rec assignment_to_string_list acc = function
		| []      -> acc
		| exe::t  -> assignment_to_string_list (exe.title::acc) t 
	in
	let rec aux_tokens = function
		| []    -> assignments
		| {token; level}::t  -> 
        if ((List.length availabel_exercises) < 6) then 
          (Hashtbl.add assignments (assignment_to_string_list [] availabel_exercises) token; aux_tokens t)
        else 
          (Hashtbl.add assignments (gen_assignment tbl level) token; aux_tokens t)
	in
	aux_tokens tokens
