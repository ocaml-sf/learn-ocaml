/* The parser definition */

%{
open Lexing;;
open Misc;;
open Common;;
open Syntax;;

let get_location () = { first = symbol_start () ; last = symbol_end () } ;;

let make_typ desc =
  { st_desc = desc ; st_loc = get_location () }
and make_pat desc =
  { sp_desc = desc ; sp_loc = get_location () }
and make_cmd desc =
  { sc_desc = desc ; sc_loc = get_location () }
and make_expr desc =
  { se_desc = desc ; se_loc = get_location () }
and make_typedef name params kind =
  { sd_name = name ; sd_params = params ; sd_kind = kind ;
    sd_loc = get_location () }
and make_ident s =
  { se_desc = SEid s ; se_loc = get_location () }
;;

let make_string s =
  make_expr (SEarray (List.map
		      (fun c -> { se_desc = SEconst (Cchar c) ;
				  se_loc = get_location () })
		      (Array.to_list (array_of_string s))))
and make_unop s e =
  make_expr (SEapply (make_ident s, [e]))
and make_binop s e1 e2 =
  make_expr (SEapply (make_ident s, [e1; e2]))
and make_ternop s e1 e2 e3 =
  make_expr (SEapply (make_ident s, [e1; e2; e3]))
;;

let make_pat_string s =
  make_pat (SParray (List.map
		     (fun c -> { sp_desc = SPconst (Cchar c) ;
				 sp_loc = get_location () })
		     (Array.to_list (array_of_string s))))
and make_cons_pat a l =
  make_pat (SPconstr ("::", make_pat (SPtuple [a; l])))
;;

%}

/* Tokens */

/* Identifiers, prefixes, infixes */
%token <string> IDENT
%token <string> QUOTED
%token <string> PREFIX
%token <string> INFIX0
%token <string> INFIX1
%token <string> INFIX2
%token <string> SUBTRACTIVE
%token <string> INFIX3
%token <string> INFIX4
/* Literals */
%token <int> INT
%token <Misc.wchar> CHAR
%token <float> FLOAT
%token <string> STRING
/* The end-of-file marker */
%token EOF
/* Special symbols */
%token EQUAL          /* "=" */
%token EQUALEQUAL     /* "==" */
%token AMPERSAND      /* "&" */
%token LPAREN         /* "(" */
%token RPAREN         /* ")" */
%token STAR           /* "*" */
%token COMMA          /* "," */
%token MINUSGREATER   /* "->" */
%token EQUALGREATER   /* "=>" */
%token DOT            /* "." */
%token COLON          /* ":" */
%token COLONCOLON     /* "::" */
%token SEMI           /* ";" */
%token LBRACKET       /* "[" */
%token LBRACKETBAR    /* "[|" */
%token LESSMINUS      /* "<-" */
%token RBRACKET       /* "]" */
%token UNDERSCORE     /* "_" */
%token LBRACE         /* "{" */
%token BAR            /* "|" */
%token BARRBRACKET    /* "|]" */
%token RBRACE         /* "}" */
%token UNDERSCORE     /* "_" */
/* Keywords */
%token AND            /* "and" */
%token BEGIN          /* "begin" */
%token CASE	      /* "case" */
%token DO             /* "do" */
%token DOWNTO         /* "downto" */
%token ELSE           /* "else" */
%token END            /* "end" */
/* %token EXCEPTION      /* "exception" */
%token FOR            /* "for" */
%token FN	      /* "fn" */
%token FUN            /* "fun" */
%token HIDE	      /* "hide" */
%token IF             /* "if" */
%token IN             /* "in" */
%token MUTABLE        /* "mutable" */
%token OF             /* "of" */
%token OR             /* "or" */
%token THEN           /* "then" */
%token TO             /* "to" */
%token TYPE           /* "type" */
%token VAL	      /* "val" */
%token VAR	      /* "val" */
%token WHILE          /* "while" */

/* Precedences and associativities. Lower precedences first. */

%right MINUSGREATER EQUALGREATER
%right SEMI
%right AND
%right prec_list
%right prec_if
%right LESSMINUS
%left  BAR
%left  COMMA
%left  OR
%left  AMPERSAND
%left  INFIX0 EQUAL EQUALEQUAL	        /* comparisons */
%right INFIX1                           /* concatenations */
%right COLONCOLON                       /* cons */
%left  INFIX2 SUBTRACTIVE               /* additives, subtractives */
%left  STAR INFIX3                      /* multiplicatives */
%right INFIX4                           /* exponentiations */
%right prec_uminus
%right prec_app
%left  DOT

/* Entry points */

%start phrase
%type <Syntax.command list> phrase

/*
%start Type_expr
%type <Syntax.type_expr> Type_expr
%start Pattern
%type <Syntax.pattern> Pattern
%start Expression
%type <Syntax.expression> Expression
%start Command
%type <Syntax.command> Command
%start Typedef
%type <Syntax.typedef> Typedef
*/

%%

/* One toplevel phrase */

phrase :
        Top_command_list SEMI
	  { List.rev $1 }
      | Top_command_list EOF
	  { List.rev $1 }
      |	EOF
	  { raise End_of_file }
;

Command :
	Expression
	  { make_cmd (SEexpr $1) }
      |	Definition
	  { $1 }
;

Definition :
        VAL Value_def
	  { make_cmd (SEval $2) }
      |	FUN Function_def
	  { make_cmd (SEfun $2) }
      |	VAR Var_def
	  { make_cmd (SEvar $2) }
      |	TYPE Typedef_list
	  { make_cmd (STtype $2) }
      |	HIDE IDENT
	  { make_cmd (SThide $2) }
;

Command_list :
	Command_list SEMI Command
	  { $3 :: $1 }
      |	Command_list Definition
	  { $2 :: $1 }
      |	Command
	  { [$1] }
;
Top_command_list :
      	Top_command_list Definition
	  { $2 :: $1 }
      |	Command
	  { [$1] }
;

Value_def :
	Pattern Expression_def AND Value_def
	  { ($1,$2) :: $4 }
      |	Pattern Expression_def
	  { [$1,$2] }
;

Function_def :
	IDENT Equation AND Function_def
	  { ($1,$2) :: $4 }
      |	IDENT Equation
	  { [$1,$2] }
;

Var_def :
	IDENT Expression_def AND Var_def
	  { ($1,$2) :: $4 }
      |	IDENT Expression_def
	  { [$1,$2] }
;

Equation :
	Simple_pattern_list Expression_def
	  { make_expr (SEfunct ($1, $2)) }
;

Expression_def :
	COLON Type_expr EQUAL Expression
	  { make_expr (SEtype ($4,$2)) }
      |	EQUAL Expression
	  { $2 }
;

Expression :
	Simple_expr
	  { $1 }
      | SUBTRACTIVE Expression  %prec prec_uminus
          { make_unop (if $1 = "-" then "~" else "~.") $2 }
      | Expression INFIX4 Expression
          { make_binop $2 $1 $3 }
      | Expression INFIX3 Expression
          { make_binop $2 $1 $3 }
      | Expression INFIX2 Expression
          { make_binop $2 $1 $3 }
      | Expression SUBTRACTIVE Expression
          { make_binop $2 $1 $3 }
      | Expression INFIX1 Expression
          { make_binop $2 $1 $3 }
      | Expression INFIX0 Expression
          { make_binop $2 $1 $3 }
      | Expression EQUAL Expression
          { make_binop "=" $1 $3 }
      | Expression EQUALEQUAL Expression
          { make_binop "==" $1 $3 }
      | Expression STAR Expression
          { make_binop "*" $1 $3 }
      |	Expression AMPERSAND Expression
	  { make_binop "&" $1 $3 }
      |	Expression OR Expression
	  { make_binop "or" $1 $3 }
      |	Expression COLONCOLON Expression
	  { make_unop "::" (make_expr (SEtuple [$1;$3])) }
      | Simple_expr Simple_expr_list   %prec prec_app
          { make_expr (SEapply ($1, $2)) }
      |	FN Simple_pattern_list EQUALGREATER Expression
	  { make_expr (SEfunct ($2, $4)) }
      |	BEGIN Expr_cmd_list END
	  { $2 }
      |	CASE Expression OF Opt_bar Matching END
	  { make_expr (SEcase ($2, $5)) }
      |	IF Expression THEN Expression ELSE Expression
	  { make_expr (SEifthenelse ($2, $4, $6)) }
      |	IF Expression THEN Expression
	  { make_expr (SEifthenelse ($2, $4, make_expr (SEtuple []))) }
      |	IDENT LESSMINUS Expression
	  { make_expr (SEset ($1, $3)) }
      |	Simple_expr DOT LBRACKET Expression RBRACKET LESSMINUS Expression
	  { make_ternop ".<-" $1 $4 $7 }
      |	Simple_expr DOT IDENT LESSMINUS Expression
	  { make_expr (SEsetfield ($1, $3, $5)) }
      |	FOR IDENT EQUAL Expression Dir_flag Expression DO Expression
	  { make_expr (SEfor ($2, $4, $5, $6, $8)) }
      |	WHILE Expression DO Expression
	  { make_expr (SEwhile ($2, $4)) }
;

Expression_comma_list :
	Expr_may_type COMMA Expression_comma_list
	  { $1 :: $3 }
      |	Expr_may_type
	  { [$1] }
      |	/* empty */
	  { [] }
;

Expr_may_type :
	Expression COLON Type_expr
	  { make_expr (SEtype ($1, $3)) }
      |	Expression
	  { $1 }
;

Label_expr_list :
	IDENT EQUAL Expression COMMA Label_expr_list
	  { ($1,$3) :: $5 }
      |	IDENT EQUAL Expression
	  { [$1,$3] }
;

Dir_flag :
	TO	{ Upto }
      |	DOWNTO	{ Downto }
;

Simple_expr :
	IDENT
	  { make_expr (SEid $1) }
      |	STRING
	  { make_string $1 }
      |	Constant
	  { make_expr (SEconst $1) }
      |	LPAREN Expression_comma_list RPAREN
	  { match $2 with [e] -> e
	    | l -> make_expr (SEtuple l) }
      |	LPAREN Infx RPAREN
	  { make_ident $2 }
      |	LBRACKETBAR Expression_comma_list BARRBRACKET
	  { make_expr (SEarray $2) }
      |	LBRACKET Expression_comma_list RBRACKET
	  { List.fold_right
              (fun a l -> make_unop "::" (make_expr (SEtuple [a;l])))
		    $2 (make_ident "[]") }
      |	LBRACE Label_expr_list RBRACE
	  { make_expr (SErecord $2) }
      |	Simple_expr DOT IDENT
	  { make_expr (SEgetfield ($1, $3)) }
      |	Simple_expr DOT LBRACKET Expression RBRACKET
	  { make_binop "." $1 $4 }
;

Simple_expr_list :
        Simple_expr Simple_expr_list
          { $1 :: $2 }
      | Simple_expr
          { [$1] }
;

Expr_cmd_list :
	Command_list
	  { make_expr (SEseq (List.rev $1)) }
      |	Command_list SEMI
	  { make_expr (SEseq (List.rev $1)) }
;

Opt_bar :
	/* Empty */			{ () }
      |	BAR				{ () }
;
Matching :
	Pattern EQUALGREATER Expr_cmd_list BAR Matching
	  { ($1, $3) :: $5 }
      |	Pattern EQUALGREATER Expr_cmd_list
	  { [$1, $3] }
;

Pattern :
	Simple_pattern
	  { $1 }
      |	IDENT Simple_pattern		%prec prec_app
	  { make_pat (SPconstr ($1, $2)) }
      |	Pattern COLONCOLON Pattern
	  { make_cons_pat $1 $3 }
;

Label_pattern_list :
	IDENT EQUAL Pattern COMMA Label_pattern_list
	  { ($1,$3) :: $5 }
      |	IDENT EQUAL Pattern
	  { [$1,$3] }
;

Pattern_comma_list :
	Pattern_may_type COMMA Pattern_comma_list
	  { $1 :: $3 }
      |	Pattern_may_type
	  { [$1] }
      |	/* empty */
	  { [] }
;

Pattern_may_type :
	Pattern COLON Type_expr
	  { make_pat (SPtype ($1, $3)) }
      |	Pattern
	  { $1 }
;

Simple_pattern :
	IDENT
	  { make_pat (SPid $1) }
      |	UNDERSCORE
	  { make_pat SPany }
      |	STRING
	  { make_pat_string $1 }
      |	Constant
	  { make_pat (SPconst $1) }
      |	SUBTRACTIVE INT
	  { make_pat (SPconst(Cint(- $2))) }
      |	SUBTRACTIVE FLOAT
	  { make_pat (SPconst(Cfloat(-. $2))) }
      |	LPAREN Pattern_comma_list RPAREN
	  { match $2 with [pat] -> pat
	    | l -> make_pat (SPtuple l) }
      |	LBRACKET Pattern_comma_list RBRACKET
	  { List.fold_right make_cons_pat
	      $2 { sp_desc = SPid "[]" ; sp_loc = get_location () } }
      |	LBRACKETBAR Pattern_comma_list BARRBRACKET
	  { make_pat (SParray $2) }
      |	LBRACE Label_pattern_list RBRACE
	  { make_pat (SPrecord $2) }
;

Simple_pattern_list :
        Simple_pattern Simple_pattern_list
          { $1 :: $2 }
      | Simple_pattern
          { [$1] }
;

Constant :
	INT	{ Cint $1 }
      |	CHAR	{ Cchar $1 }
      |	FLOAT	{ Cfloat $1 }
;

Infx :
        INFIX0          { $1 }
      | INFIX1          { $1 }    | INFIX2        { $1 }
      | INFIX3          { $1 }    | INFIX4        { $1 }
      | STAR            { "*" }   | COLONCOLON    { "::" }
      | EQUAL           { "=" }   | SUBTRACTIVE     { $1 }
      | AMPERSAND       { "&" }   | OR              { "or" }
      |	EQUALEQUAL	{ "==" }
;

/* Type expressions */

Type_expr :
	Type_star_list
          { match List.rev $1 with [ty] -> ty
	    | l -> make_typ(STtuple l) }
      | Type_expr MINUSGREATER Type_expr
          { make_typ(STarrow($1, $3)) }
;

Simple_type :
        QUOTED
          { make_typ(STvar $1) }
      | IDENT
          { make_typ(STconstr($1, [])) }
      | Simple_type IDENT
          { make_typ(STconstr($2, [$1])) }
      | LPAREN Type_expr COMMA Type_comma_list RPAREN IDENT
          { make_typ(STconstr($6, $2 :: $4)) }
      | LPAREN Type_expr RPAREN
          { $2 }
;

Type_star_list :
        Type_star_list STAR Simple_type
          { $3 :: $1 }
      | Simple_type
          { [$1] }
;

Type_comma_list :
        Type_expr COMMA Type_comma_list
          { $1 :: $3 }
      | Type_expr
          { [$1] }
;

/* Declarations */

Typedef_list :
        Typedef AND Typedef_list
          { $1 :: $3 }
      | Typedef
          { [$1] }
;

Constr_decl :
        Constr1_decl BAR Constr_decl
          { $1 :: $3 }
      | Constr1_decl
          { [$1] }
;

Label_decl :
        Label1_decl COMMA Label_decl
          { $1 :: $3 }
      | Label1_decl
          { [$1] }
;

Typedef :
        Type_params IDENT Type1_def
          { make_typedef $2 $1 $3 }
;

Type1_def :
	EQUAL Opt_bar Constr_decl
          { SKvariant $3 }
      | EQUAL LBRACE Label_decl RBRACE
          { SKrecord $3 }
      | EQUALEQUAL Type_expr
          { SKabbrev $2 }
;

Constr1_decl :
        IDENT OF Type_star_list
          { ($1, List.rev $3) }
      | IDENT
          { ($1, []) }
;

Label1_decl :
        Mutable_option IDENT COLON Type_expr
          { ($2, $4, $1) }
;

Mutable_option :
        MUTABLE
          { Mutable }
      | /* epsilon */
          { Immutable }
;

Type_params :
        LPAREN Type_var_list RPAREN
          { $2 }
      | QUOTED
          { [$1] }
      |
          { [] }
;

Type_var_list :
        QUOTED COMMA Type_var_list
          { $1 :: $3 }
      | QUOTED
          { [$1] }
;

%%
