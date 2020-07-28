
module Serialize = struct 
	open Core_kernel


	let (>>) f g = (fun x -> g (f x)) (* Function compostion left to right*)

	let to_string = Sexp.to_string;;
	let of_string = Sexp.of_string;;

	let log s =
		let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 "a.txt" in
		output_string oc (s ^ "\n");
		close_out oc;
		s

	type ('a,'b) bridged_function = {
					sexp_of_a : ('a -> Sexp.t) 
					;b_of_sexp : ( Sexp.t -> 'b)
					;sexp_of_b : ('b -> Sexp.t) 
					;a_of_sexp : (Sexp.t -> 'a)
					}

	type uri = string



	let (>=>) : ('a -> 'b option) -> ('b -> 'c option) -> ('a -> 'c option) =
	(fun f g -> (fun x -> match f x with 
				| None -> None
				| Some x -> g x))


	let (>>=) : 'a option -> ('a -> 'b option) -> 'b option =
	(fun x f -> match x with 
						| None -> None
						| Some y -> f y)

	
	type 'c sexp_helper = {sexp_of_c : ('c -> Sexp.t) 
				;c_of_sexp : (Sexp.t -> 'c)}
	
	let sig_gen : ('a sexp_helper) -> ('b sexp_helper) -> ('a,'b) bridged_function = 
	(fun a b -> {	sexp_of_a = a.sexp_of_c
			;b_of_sexp = b.c_of_sexp
			;sexp_of_b = b.sexp_of_c
			;a_of_sexp = a.c_of_sexp;})

	(*Converters for all base types can be found in Sexplib.Std*)
    	let unit_helper = {
        	sexp_of_c = Sexplib.Std.sexp_of_unit;
		c_of_sexp = Sexplib.Std.unit_of_sexp
    	}

	let int_helper = {
			sexp_of_c = Sexplib.Std.sexp_of_int;
			c_of_sexp = Sexplib.Std.int_of_sexp}

	let float_helper = {
			sexp_of_c = Sexplib.Std.sexp_of_float;
			c_of_sexp = Sexplib.Std.float_of_sexp}
	let string_helper = {
			sexp_of_c = Sexplib.Std.sexp_of_string;
			c_of_sexp = Sexplib.Std.string_of_sexp}

	let bool_helper = {
			sexp_of_c = Sexplib.Std.sexp_of_bool;
			c_of_sexp = Sexplib.Std.bool_of_sexp}

	let option_helper helper = {	
			sexp_of_c =  Sexplib.Std.sexp_of_option helper.sexp_of_c 
			; c_of_sexp = Sexplib.Std.option_of_sexp helper.c_of_sexp}

	
	let list_helper helper = {	
			sexp_of_c =  Sexplib.Std.sexp_of_list helper.sexp_of_c 
			; c_of_sexp = Sexplib.Std.list_of_sexp helper.c_of_sexp}





	let int_to_int = (sig_gen int_helper int_helper);;
	let int_to_string = (sig_gen int_helper string_helper);;
	let int_to_bool = (sig_gen int_helper bool_helper);;

	let int_to_int_list = (sig_gen int_helper (list_helper int_helper));;

	let int_list_to_int = (sig_gen (list_helper int_helper) int_helper);;

	let temp = (list_helper int_helper)

	
	let coerce = (fun (Some x) -> x)

	let join x = match x with
				| None -> None
				| Some x -> x







  end 


open Serialize


let report_of_string s = try 
			s |> of_string |> Learnocaml_report.t_of_sexp |> (fun x -> Some x)
			with 
			| _ -> None 


let coerce = Serialize.coerce



type 'a tree = Leaf | Node of 'a * ('a tree) * ('a tree) [@@deriving sexp]





let rec gen_tree n = if n = 0 then Leaf 
		else let child = (gen_tree (n-1)) in 
			Node (n,child,child)

let sample_tree = (gen_tree 2)

let show_tree t = t |> (sexp_of_tree int_helper.sexp_of_c) |> to_string

let package s = s |> (string_helper.sexp_of_c) |> to_string


