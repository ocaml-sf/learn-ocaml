
module Serialize = struct 
	open Core_kernel
	(*
	Duplicate learnocaml_report type definition
	 as the required ppx, core_kernel dependencies broke learn-ocaml when used directly
	 *)

(*
	type t = item list

	and item =
	   | Section of text * t
	   | SectionMin of text * t * int
	   | Message of text * status

	 and status =
	   | Success of int | Penalty of int | Failure
	   | Warning | Informative | Important

	 and text = inline list

	 and inline =
	   | Text of string
	   | Break
	   | Code of string
	   | Output of string [@@deriving sexp]

	let rec map f xs = match xs with 	
				| [] -> []
				| x :: xs -> (f x) :: (map f xs);;


	let rec convert_t : t -> Learnocaml_report.t = (fun s -> map convert_item s)
	and convert_item : item -> Learnocaml_report.item = 
		(fun s -> 	match s with 
				| Section (text,t) -> Learnocaml_report.Section (convert_text text,convert_t t)
				| SectionMin (text,t,n) -> Learnocaml_report.SectionMin (convert_text text,convert_t t,n)
				| Message (text,status) -> Learnocaml_report.Message (convert_text text,convert_status status))
	and convert_status : status -> Learnocaml_report.status = 
		(fun s -> 	match s with 
				| Success n -> Learnocaml_report.Success n 
				| Penalty n -> Learnocaml_report.Penalty n 
				| Failure -> Learnocaml_report.Failure 
				| Warning -> Learnocaml_report.Warning 
				| Informative -> Learnocaml_report.Informative 
				| Important -> Learnocaml_report.Important)
	and convert_text : text -> Learnocaml_report.text = map convert_inline 
	and convert_inline : inline -> Learnocaml_report.inline = 
		(fun s -> 	match s with 
				| Text s -> Learnocaml_report.Text s 
				| Break -> Learnocaml_report.Break 
				| Code s -> Learnocaml_report.Code s
				| Output s -> Learnocaml_report.Output s)
			
	

  	*)

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




