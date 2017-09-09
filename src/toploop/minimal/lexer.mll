(* The lexer definition *)

{
(**) open Parser;;

(* For nested comments *)

let comment_depth = ref 0;;

(* The table of keywords *)

let keyword_table = (Hashtbl.create 57 : (string, token) Hashtbl.t)
;;

List.iter (fun (str,tok) -> Hashtbl.add keyword_table str tok)
  [ "and", AND;
    "begin", BEGIN;
    "case", CASE;
    "do", DO;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
(*    "exception", EXCEPTION; *)
    "fn", FN;
    "for", FOR;
    "fun", FUN;
(*    "handle", HANDLE; *)
    "hide", HIDE;
    "if", IF;
    "mutable", MUTABLE;
    "of", OF;
    "or", OR;
    "then", THEN;
    "to", TO;
    "type", TYPE;
    "val", VAL;
    "var", VAR;
    "while", WHILE;
  ];;

let add_infix s =
  Hashtbl.add keyword_table s (INFIX3 s)
;;

List.iter add_infix ["mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"] ;;

(*
let remove_infix s =
  Hashtbl.remove keyword_table s
;;
*)

(* To buffer string literals *)

let initial_string_buffer = Bytes.create 256;;
let string_buff = ref initial_string_buffer;;
let string_index = ref 0;;

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0;
  ()
;;

let store_string_char c =
  if !string_index >= Bytes.length (!string_buff) then begin
    let new_buff = Bytes.create (Bytes.length (!string_buff) * 2) in
      Bytes.blit (!string_buff) 0 new_buff 0 (Bytes.length (!string_buff));
      string_buff := new_buff
  end;
  Bytes.set (!string_buff) (!string_index) c;
  incr string_index
;;

let get_stored_string () =
  let s = Bytes.sub_string (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s
;;

(* To translate escape sequences *)

let char_for_backslash = function

    'n' -> '\010'
  | 'r' -> '\013'

  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c
;;

open Lexing

let char_for_decimal_code lexbuf i =
  let c = 
    100 * (int_of_char(lexeme_char lexbuf i) - 48) +
     10 * (int_of_char(lexeme_char lexbuf (i+1)) - 48) +
          (int_of_char(lexeme_char lexbuf (i+2)) - 48) in
  char_of_int(c land 0xFF)
;;

type error =
    Unterminated_string
  | Unterminated_comment
  | Illegal_character
;;
exception Lexical_error of error * int * int ;;

}

rule skip_sb = parse
    "#!" [^ '\010' '\013'] * { () }
  | ""                       { () }

and main = parse
    [' ' '\010' '\013' '\009' '\012'] +
      { main lexbuf }
  | ['A'-'Z' 'a'-'z' '\160'-'\255' ]
    ( '_' ? ['A'-'Z' 'a'-'z' '\160'-'\255' ''' (*'*) '0'-'9' ] ) *
      { let s = lexeme lexbuf in
          try
            Hashtbl.find keyword_table s
          with Not_found ->
            IDENT s }
  | ['0'-'9']+
    | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
    | '0' ['o' 'O'] ['0'-'7']+
    | '0' ['b' 'B'] ['0'-'1']+
      { INT (int_of_string(lexeme lexbuf)) }
  | ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
      { FLOAT (float_of_string(lexeme lexbuf)) }
  | "\""
      { reset_string_buffer();
        let string_start = lexbuf.lex_start_pos + lexbuf.lex_abs_pos in
        begin try
          string lexbuf
        with Lexical_error(Unterminated_string, _, string_end) ->
          raise(Lexical_error(Unterminated_string, string_start, string_end))
        end;
        lexbuf.lex_start_pos <- string_start - lexbuf.lex_abs_pos;
        STRING (get_stored_string()) }
  | "'"
      { let char_start = lexbuf.lex_start_pos + lexbuf.lex_abs_pos in
        let token = char_or_var lexbuf in
        lexbuf.lex_start_pos <- char_start - lexbuf.lex_abs_pos;
        token }
  | "(*"
      { let comment_start = lexbuf.lex_start_pos + lexbuf.lex_abs_pos in
        comment_depth := 1;
        begin try
          comment lexbuf
        with Lexical_error(Unterminated_comment, _, comment_end) ->
          raise(Lexical_error(Unterminated_comment,
                              comment_start, comment_end))
        end;
        main lexbuf }
  | "&" { AMPERSAND }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "*" { STAR }
  | "," { COMMA }
  | "->" { MINUSGREATER }
  | "=>" { EQUALGREATER }
  | "." { DOT }
  | ":" { COLON }
  | "::" { COLONCOLON }
  | ";" { SEMI }
  | "<-" { LESSMINUS }
  | "=" { EQUAL }
  | "==" { EQUALEQUAL }
  | "[" { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "]" { RBRACKET }
  | "_" { UNDERSCORE }
  | "{" { LBRACE }
  | "|" { BAR }
  | "|]" { BARRBRACKET }
  | "}" { RBRACE }

  | "-"     { SUBTRACTIVE "-" }
  | "-."    { SUBTRACTIVE "-." }

  | "!="
  | "<>"
  | ['<' '>'] '=' ?
            { INFIX0(lexeme lexbuf) }
  | ['@' '^']
            { INFIX1(lexeme lexbuf) }
  | '+' '.' ?
            { INFIX2(lexeme lexbuf) }
  | "**"
            { INFIX4(lexeme lexbuf) }
  | [ '*' '/' ] '.' ?
            { INFIX3(lexeme lexbuf) }
  | eof { EOF }
  | _
      { raise (Lexical_error(Illegal_character,
                            lexeme_start lexbuf, lexeme_end lexbuf)) }

and comment = parse
    "(*"
      { comment_depth := succ !comment_depth; comment lexbuf }
  | "*)"
      { comment_depth := pred !comment_depth;
        if !comment_depth > 0 then comment lexbuf }
  | "\""
      { reset_string_buffer();
        let string_start = lexbuf.lex_start_pos + lexbuf.lex_abs_pos in
        begin try
          string lexbuf
        with Lexical_error(Unterminated_string, _, string_end) ->
          raise(Lexical_error(Unterminated_string, string_start, string_end))
        end;
        comment lexbuf }
  | "'\"'"
      { comment lexbuf }
  | eof
      { raise(Lexical_error
                (Unterminated_comment, 0, lexeme_start lexbuf)) }
  | _
      { comment lexbuf }

and char_or_var = parse
    [^ '\\' '''] "'"
      { CHAR (int_of_char (lexeme_char lexbuf 0)) }
  | ['\160'-'\255'] ['\160'-'\255'] "'"
      { CHAR (int_of_char (lexeme_char lexbuf 0) lsl 8
	      +	int_of_char (lexeme_char lexbuf 1)) }
  | '\\' ['\\' ''' 'n' 't' 'b' 'r'] "'"
      { CHAR (int_of_char (char_for_backslash (lexeme_char lexbuf 1))) }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR (int_of_char (char_for_decimal_code lexbuf 1)) }
  | ['A'-'Z' 'a'-'z' '\160'-'\255' ]
    ( '_' ? ['A'-'Z' 'a'-'z' '\160'-'\255' ''' (*'*) '0'-'9' ] ) *
      { QUOTED (lexeme lexbuf) }

and string = parse
    '"' (*"'"'*)
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { raise (Lexical_error
                (Unterminated_string, 0, lexeme_start lexbuf)) }
  | _
      { store_string_char(lexeme_char lexbuf 0);
        string lexbuf }

