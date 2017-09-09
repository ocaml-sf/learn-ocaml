(* $Id: misc.ml,v 1.6 2004/09/24 00:51:15 garrigue Exp $ *)

type wchar = int

type code = EUC | SJIS | UTF8 | NOCONV

let rec get_var = function
    [] -> ""
  | v :: rem -> try Sys.getenv v with Not_found -> get_var rem
;;

let suffix s1 s2 =
  let l1 = String.length s1 and l2 = String.length s2 in
  l2 <= l1 && String.sub s1 (l1-l2) l2 = s2

let code =
  let lang = get_var ["MINIMAL_LANG"; "LC_ALL"; "LC_CTYPE"; "LANG"] in
  let lang = String.lowercase_ascii lang in
  if lang = "ja" || suffix lang "euc" || suffix lang "eucjp" then EUC else
  if suffix lang "sjis" || suffix lang "pck" then SJIS else
  if suffix lang "utf8" || suffix lang "utf-8" then UTF8 else
  if suffix lang "iso8859-1" || suffix lang "iso8859-15" ||
     suffix lang "noconv"
  then NOCONV
  else SJIS
;;

let charset =
  match code with
    EUC -> "EUC"
  | SJIS -> "SJIS"
  | UTF8 -> "UTF-8"
  | NOCONV -> "NOCONV"
;;

let rec hi_bits n =
  if n land 0x80 = 0 then 0 else
  1 + hi_bits (n lsl 1)

  let to_unichar s ~pos : wchar =
    let c = Char.code s.[!pos] in
    incr pos;
    let n = hi_bits c in
    if n < 2 || n > 6 then c else
    let u = ref (c land (1 lsl (7-n) - 1)) in
    let len = String.length s in
    for i = 1 to n-1 do
      u := !u lsl 6;
      if !pos < len then 
        let c = Char.code s.[!pos] in
        if c land 0xc0 = 0x80 then begin
          u := !u + c land 0x3f;
          incr pos
        end
    done;
    !u

let to_ucstring s : wchar array =
  let pos = ref 0 in
  let len = String.length s in
  let r = Array.make len 0 in
  let i = ref 0 in
  while !pos < len do r.(!i) <- to_unichar s ~pos; incr i done;
  Array.sub r 0 !i
;;

let rec log64 n =
  if n = 0 then 0 else
  1 + log64 (n lsr 6)
  
let write_unichar s ~pos (c : wchar) =
  if c < 0x80 then begin
    Bytes.set s !pos (Char.chr c); incr pos
  end else begin
    let len = log64 c and p = !pos in
    pos := !pos + len;
    Bytes.set s p (Char.chr (((1 lsl len - 1) lsl (8-len)) lor (c lsr (len*6-6))));
    for i = 1 to len-1 do
      Bytes.set s(p+i) (Char.chr (((c lsr ((len-i-1)*6)) land 0x3f) lor 0x80))
    done
  end
    
let of_ucstring (s : wchar array) =
  let len = Array.length s in
  let r = Bytes.create (len*6) in
  let pos = ref 0 in
  for i = 0 to len-1 do write_unichar r ~pos s.(i) done;
  Bytes.sub_string r 0 !pos
;;

let print_wchar c =
  if c <= 0xff then print_char (char_of_int c) else
  if code = UTF8 then print_string (of_ucstring [|c|]) else
  begin
    print_char (char_of_int (c lsr 8));
    print_char (char_of_int (c land 0xff))
  end
;;

let string_of_array arr =
  let len = Array.length arr in
  if code = NOCONV then
    let s = Bytes.create len in
    for i = 0 to len-1 do Bytes.set s i (char_of_int arr.(i)) done;
    Bytes.to_string s
  else if code = UTF8 then of_ucstring arr else
  let len' = ref 0 in
  for i = 0 to len - 1 do
    incr len';
    if arr.(i) > 0xff then incr len'
  done;
  let s = Bytes.create !len' and j = ref 0 in
  for i = 0 to len - 1 do
    let c = arr.(i) in
    if c <= 0xff then Bytes.set s !j (char_of_int c) else begin
      Bytes.set s !j (char_of_int (c lsr 8));
      incr j;
      Bytes.set s !j (char_of_int (c land 0xff))
    end;
    incr j
  done;
  Bytes.to_string s
;;

let array_of_string s =
  let len = String.length s in
  if code = NOCONV then
    let arr = Array.make len 32 in
    for i = 0 to len-1 do arr.(i) <- int_of_char s.[i] done;
    arr
  else if code = UTF8 then to_ucstring s else
  let ws = Array.make len 0 in
  let len' = ref 0 and i = ref 0 in
  while !i < len do
    let c = Char.code s.[!i] in
    incr i;
    let wide = !i < len &&
      (code = SJIS && (c>127 && c<160 || c>223) && s.[!i] >= '\064' ||
       code = EUC && (c>159 && s.[!i] >= '\160'))
    in
    if wide then begin
      ws.(!len') <- c lsl 8 + Char.code s.[!i];
      incr i;
    end else
      ws.(!len') <- c;
    incr len'
  done;
  Array.sub ws 0 !len'
;;

let split_last l =
  match List.rev l with
    last :: rl -> List.rev rl, last
  | [] -> raise (Invalid_argument "Misc.split_last")
;;

let rec do_list3 f l1 l2 l3 =
  match l1,l2,l3 with
    [],[],[] -> ()
  | a::l1, b::l2, c::l3 -> f a b c; do_list3 f l1 l2 l3
  | _ -> raise (Invalid_argument "do_list3")
;;
