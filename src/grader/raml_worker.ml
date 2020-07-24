open Js_of_ocaml;;

let func_of_name s = Js.Unsafe.js_expr s;;
let apply = Js.Unsafe.fun_call;;
let eval_js_expr = Js.Unsafe.js_expr;;
let cast = Js.Unsafe.inject



let _ = Worker.set_onmessage @@ fun json -> 
		let str = "Eaudepanache" in
		let code:string = json##.data in 
		let xhttp = eval_js_expr "new XMLHttpRequest()" in 
		let this = eval_js_expr "this" in
		let make_message str = let empty = eval_js_expr "{}" in 
					let _ = empty##.data := str in 
					empty in
		let response_handler ()  = if this##.readyState = 4 && this##.status = 200
						then let _ = apply (func_of_name "postMessage") [|make_message (xhttp##.response)|] in ()
						else let _ = apply (func_of_name "postMessage") [|make_message "(fail1234"|] in ()
		 in
		()
	(*
		begin
			let code:string = json##.data in 
			let xhttp = eval_js_expr "new XMLHttpRequest()" in(*
		let _ = xhttp##.onreadystatechange := (eval_js_expr "function(){if (this.readyState == 4 && this.status == 200) {postMessage({'data' : xhttp.responseText});}else{postMessage({'data' :'(failure'});}}") in *)
			
			let _ = apply (xhttp##.overrideMimeType) [|cast "text/html"|] in 
			let _ = apply (eval_js_expr "raml_http_request.open") [|cast "POST"; cast "http://192.168.203.208:5000/"; eval_js_expr "false" |] in 
			let _ = apply (xhttp##.setRequestHeader) [|cast "Content-Type"; cast "text/plain"|] in 
			try 
				let _ = apply (xhttp##.overrideMimeType) [|cast code|] in ()
			with
			| _ -> let _ = eval_js_expr "postMessage({'data' : '(failure'});" in ()
		end
	*)
