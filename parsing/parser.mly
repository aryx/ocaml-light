/*(*s: ./parsing/parser.mly *)*/
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* The parser definition */

%{
/*(*s: Parser header *)*/
open Location
open Asttypes
open Longident
open Parsetree

let mktyp d =
  { ptyp_desc = d; ptyp_loc = symbol_loc() }
let mkpat d =
  { ppat_desc = d; ppat_loc = symbol_loc() }
let mkexp d =
  { pexp_desc = d; pexp_loc = symbol_loc() }
let mkmty d =
  { pmty_desc = d; pmty_loc = symbol_loc() }
let mksig d =
  { psig_desc = d; psig_loc = symbol_loc() }
let mkmod d =
  { pmod_desc = d; pmod_loc = symbol_loc() }
let mkstr d =
  { pstr_desc = d; pstr_loc = symbol_loc() }

let mkoperator name pos =
  { pexp_desc = Pexp_ident(Lident name); pexp_loc = rhs_loc pos }

let mkassert e =
  let {loc_start = st; loc_end = en} = symbol_loc () in
  let triple = mkexp (Pexp_tuple
                       [mkexp (Pexp_constant (Const_string !input_name));
                        mkexp (Pexp_constant (Const_int st));
                        mkexp (Pexp_constant (Const_int en))]) in
  let ex = Ldot (Lident "Pervasives", "Assert_failure") in
  let bucket = mkexp (Pexp_construct (ex, Some triple)) in
  let ra = Ldot (Lident "Pervasives", "raise") in
  let raiser = mkexp (Pexp_apply (mkexp (Pexp_ident ra), [bucket])) in
  let un = mkexp (Pexp_construct (Lident "()", None)) in
  match e with
  | {pexp_desc = Pexp_construct (Lident "false", None) } -> raiser
  | _ -> if !Clflags.noassert
         then un
         else mkexp (Pexp_ifthenelse (e, un, Some raiser))
;;

let mklazy e =
  let void_pat = mkpat (Ppat_construct (Lident "()", None)) in
  let f = mkexp (Pexp_function ([void_pat, e])) in
  let delayed = Ldot (Lident "Lazy", "Delayed") in
  let df = mkexp (Pexp_construct (delayed, Some f)) in
  let r = mkexp (Pexp_ident (Ldot (Lident "Pervasives", "ref"))) in
  mkexp (Pexp_apply (r, [df]))
;;

let mkinfix arg1 name arg2 =
  mkexp(Pexp_apply(mkoperator name 2, [arg1; arg2]))

let mkuminus name arg =
  match arg.pexp_desc with
    Pexp_constant(Const_int n) ->
      mkexp(Pexp_constant(Const_int(-n)))
  | Pexp_constant(Const_float f) ->
      mkexp(Pexp_constant(Const_float("-" ^ f)))
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, [arg]))

let rec mklistexp = function
    [] ->
      mkexp(Pexp_construct(Lident "[]", None))
  | e1 :: el ->
      mkexp(Pexp_construct(Lident "::",
                           Some(mkexp(Pexp_tuple[e1; mklistexp el]))
                           ))
let rec mklistpat = function
    [] ->
      mkpat(Ppat_construct(Lident "[]", None))
  | p1 :: pl ->
      mkpat(Ppat_construct(Lident "::",
                           Some(mkpat(Ppat_tuple[p1; mklistpat pl]))
                           ))

let mkstrexp e =
  { pstr_desc = Pstr_eval e; pstr_loc = e.pexp_loc }

let array_function str name =
  Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name))

let rec mkrangepat c1 c2 =
  if c1 > c2 then mkrangepat c2 c1 else
  if c1 = c2 then mkpat(Ppat_constant(Const_char c1)) else
  mkpat(Ppat_or(mkpat(Ppat_constant(Const_char c1)),
                mkrangepat (Char.chr(Char.code c1 + 1)) c2))

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_num closing_name closing_num =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(rhs_loc opening_num, opening_name,
                                           rhs_loc closing_num, closing_name)))

/*(*e: Parser header *)*/
%}

/* Tokens */

/*(*s: Parser tokens *)*/
%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COMMA
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token GREATER
%token IF
%token IN
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token <int> INT
%token LAZY
%token LBRACE
%token LBRACKET
%token LBRACKETBAR
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token OF
%token OPEN
%token OR
%token PARSER
%token <string> PREFIXOP
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token SIG
%token STAR
%token <string> STRING
%token STRUCT
%token <string> SUBTRACTIVE
%token THEN
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token WHEN
%token WHILE
%token WITH
/*(*e: Parser tokens *)*/

/* Precedences and associativities. Lower precedences come first. */

/*(*s: Parser precedences and associativities *)*/
%right prec_let                         /* let ... in ... */
%right prec_type_def                    /* = in type definitions */
%right SEMI                             /* e1; e2 (sequence) */
%right prec_fun prec_match prec_try     /* match ... with ... */
%right prec_list                        /* e1; e2 (list, array, record) */
%right prec_if                          /* if ... then ... else ... */
%right COLONEQUAL LESSMINUS             /* assignments */
%left  AS                               /* as in patterns */
%left  BAR                              /* | in patterns */
%left  COMMA                            /* , in expressions, patterns, types */
%right prec_type_arrow                  /* -> in type expressions */
%right OR BARBAR                        /* || */
%right AMPERSAND AMPERAMPER             /* && */
%left  INFIXOP0 EQUAL LESS GREATER      /* = < > etc */
%right INFIXOP1                         /* @ ^ etc */
%right COLONCOLON                       /* :: */
%left  INFIXOP2 SUBTRACTIVE             /* + - */
%left  INFIXOP3 STAR                    /* * / */
%right INFIXOP4                         /* ** */
%right prec_unary_minus                 /* - unary */
%left  prec_appl                        /* function application */
%right prec_constr_appl                 /* constructor application */
%left  DOT                              /* record access, array access */
%right PREFIXOP                         /* ! */
/*(*e: Parser precedences and associativities *)*/

/* Entry points */

/*(*s: Parser entry points types *)*/
%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
/*(*x: Parser entry points types *)*/
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file
/*(*e: Parser entry points types *)*/

%%

/*(*s: grammar *)*/

/* Entry points */

/*(*s: entry points rules *)*/
implementation:
    structure EOF                        { $1 }
;
interface:
    signature EOF                        { List.rev $1 }
;
/*(*x: entry points rules *)*/

toplevel_phrase:
    top_structure SEMISEMI               { Ptop_def $1 }
  | seq_expr SEMISEMI                    { Ptop_def[mkstrexp $1] }
  | toplevel_directive SEMISEMI          { $1 }
  | EOF                                  { raise End_of_file }
;
top_structure:
    structure_item                       { [$1] }
  | structure_item top_structure         { $1 :: $2 }
;
use_file:
    use_file_tail                        { $1 }
  | seq_expr use_file_tail               { Ptop_def[mkstrexp $1] :: $2 }
;
use_file_tail:
    EOF                                         { [] }
  | SEMISEMI EOF                                { [] }
  | SEMISEMI seq_expr use_file_tail             { Ptop_def[mkstrexp $2] :: $3 }
  | SEMISEMI structure_item use_file_tail       { Ptop_def[$2] :: $3 }
  | SEMISEMI toplevel_directive use_file_tail   { $2 :: $3 }
  | structure_item use_file_tail                { Ptop_def[$1] :: $2 }
  | toplevel_directive use_file_tail            { $1 :: $2 }
;

/*(*e: entry points rules *)*/

/* Module expressions */

/*(*s: structure rules *)*/

module_expr:
    mod_longident
      { mkmod(Pmod_ident $1) }
  | STRUCT structure END
      { mkmod(Pmod_structure($2)) }
  | LPAREN module_expr COLON module_type RPAREN
      { mkmod(Pmod_constraint($2, $4)) }
  | LPAREN module_expr RPAREN
      { $2 }

  | STRUCT structure error
      { unclosed "struct" 1 "end" 3 }
  | LPAREN module_expr COLON module_type error
      { unclosed "(" 1 ")" 5 }
  | LPAREN module_expr error
      { unclosed "(" 1 ")" 3 }
;
structure:
    structure_tail                              { $1 }
  | seq_expr structure_tail                     { mkstrexp $1 :: $2 }
;
structure_tail:
    /* empty */                                 { [] }
  | SEMISEMI                                    { [] }
  | SEMISEMI seq_expr structure_tail            { mkstrexp $2 :: $3 }
  | SEMISEMI structure_item structure_tail      { $2 :: $3 }
  | structure_item structure_tail               { $1 :: $2 }
;
structure_item:
    LET rec_flag let_bindings
      { match $3 with
          [{ppat_desc = Ppat_any}, exp] -> mkstr(Pstr_eval exp)
        | _ -> mkstr(Pstr_value($2, List.rev $3)) }
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
      { mkstr(Pstr_primitive($2, {pval_type = $4; pval_prim = $6})) }
  | TYPE type_declarations
      { mkstr(Pstr_type(List.rev $2)) }
  | EXCEPTION UIDENT constructor_arguments
      { mkstr(Pstr_exception($2, $3)) }
  | MODULE UIDENT module_binding
      { mkstr(Pstr_module($2, $3)) }
  | OPEN mod_longident
      { mkstr(Pstr_open $2) }
;
module_binding:
    EQUAL module_expr
      { $2 }
  | COLON module_type EQUAL module_expr
      { mkmod(Pmod_constraint($4, $2)) }
;
/*(*e: structure rules *)*/

/* Module types */

/*(*s: signature rules *)*/

module_type:
    mty_longident
      { mkmty(Pmty_ident $1) }
  | SIG signature END
      { mkmty(Pmty_signature(List.rev $2)) }
  | LPAREN module_type RPAREN
      { $2 }

  | SIG signature error
      { unclosed "sig" 1 "end" 3 }
  | LPAREN module_type error
      { unclosed "(" 1 ")" 3 }
;
signature:
    /* empty */                                 { [] }
  | signature signature_item                    { $2 :: $1 }
  | signature signature_item SEMISEMI           { $2 :: $1 }
;
signature_item:
    VAL val_ident COLON core_type
      { mksig(Psig_value($2, {pval_type = $4; pval_prim = []})) }
  | EXTERNAL val_ident COLON core_type EQUAL primitive_declaration
      { mksig(Psig_value($2, {pval_type = $4; pval_prim = $6})) }
  | TYPE type_declarations
      { mksig(Psig_type(List.rev $2)) }
  | EXCEPTION UIDENT constructor_arguments
      { mksig(Psig_exception($2, $3)) }
  | MODULE UIDENT module_declaration
      { mksig(Psig_module($2, $3)) }
  | OPEN mod_longident
      { mksig(Psig_open $2) }
;

module_declaration:
    COLON module_type
      { $2 }
;
/*(*e: signature rules *)*/

/* Core expressions */

/*(*s: expression rules *)*/

seq_expr:
  | expr                          { $1 }
  | expr SEMI                     { $1 }
  | expr SEMI seq_expr            { mkexp(Pexp_sequence($1, $3)) }
;
expr:
    simple_expr
      { $1 }
  | simple_expr simple_expr_list %prec prec_appl
      { mkexp(Pexp_apply($1, List.rev $2)) }
  | LET rec_flag let_bindings IN seq_expr %prec prec_let
      { mkexp(Pexp_let($2, List.rev $3, $5)) }
  | FUNCTION opt_bar match_cases %prec prec_fun
      { mkexp(Pexp_function(List.rev $3)) }
  | FUN simple_pattern fun_def %prec prec_fun
      { mkexp(Pexp_function([$2, $3])) }
  | MATCH seq_expr WITH opt_bar match_cases %prec prec_match
      { mkexp(Pexp_match($2, List.rev $5)) }
  | TRY seq_expr WITH opt_bar match_cases %prec prec_try
      { mkexp(Pexp_try($2, List.rev $5)) }
  | TRY seq_expr WITH error %prec prec_try
      { syntax_error() }
  | expr_comma_list
      { mkexp(Pexp_tuple(List.rev $1)) }
  | constr_longident simple_expr %prec prec_constr_appl
      { mkexp(Pexp_construct($1, Some $2)) }
  | IF seq_expr THEN expr ELSE expr %prec prec_if
      { mkexp(Pexp_ifthenelse($2, $4, Some $6)) }
  | IF seq_expr THEN expr %prec prec_if
      { mkexp(Pexp_ifthenelse($2, $4, None)) }
  | WHILE seq_expr DO seq_expr DONE
      { mkexp(Pexp_while($2, $4)) }
  | FOR val_ident EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
      { mkexp(Pexp_for($2, $4, $6, $5, $8)) }
  | expr COLONCOLON expr
      { mkexp(Pexp_construct(Lident "::", Some(mkexp(Pexp_tuple[$1;$3])))) }
  | expr INFIXOP0 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP1 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP2 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP3 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP4 expr
      { mkinfix $1 $2 $3 }
  | expr SUBTRACTIVE expr
      { mkinfix $1 $2 $3 } 
  | expr STAR expr
      { mkinfix $1 "*" $3 } 
  | expr EQUAL expr
      { mkinfix $1 "=" $3 } 
  | expr LESS expr
      { mkinfix $1 "<" $3 } 
  | expr GREATER expr
      { mkinfix $1 ">" $3 } 
  | expr OR expr
      { mkinfix $1 "or" $3 }
  | expr BARBAR expr
      { mkinfix $1 "||" $3 }
  | expr AMPERSAND expr
      { mkinfix $1 "&" $3 }
  | expr AMPERAMPER expr
      { mkinfix $1 "&&" $3 }
  | expr COLONEQUAL expr
      { mkinfix $1 ":=" $3 }
  | SUBTRACTIVE expr %prec prec_unary_minus
      { mkuminus $1 $2 }
  | simple_expr DOT label_longident LESSMINUS expr
      { mkexp(Pexp_setfield($1, $3, $5)) }
  | simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
      { mkexp(Pexp_apply(mkexp(Pexp_ident(array_function "Array" "set")),
                         [$1; $4; $7])) }
  | simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS expr
      { mkexp(Pexp_apply(mkexp(Pexp_ident(array_function "String" "set")),
                         [$1; $4; $7])) }
  | ASSERT simple_expr %prec prec_appl
      { mkassert $2 }
  | LAZY simple_expr %prec prec_appl
      { mklazy $2 }
;
simple_expr:
    val_longident
      { mkexp(Pexp_ident $1) }
  | constant
      { mkexp(Pexp_constant $1) }
  | constr_longident
      { mkexp(Pexp_construct($1, None)) }
  | LPAREN seq_expr RPAREN
      { $2 }
  | LPAREN seq_expr error
      { unclosed "(" 1 ")" 3 }
  | BEGIN seq_expr END
      { $2 }
  | BEGIN seq_expr error
      { unclosed "begin" 1 "end" 3 }
  | LPAREN seq_expr type_constraint RPAREN
      { mkexp(Pexp_constraint($2, $3)) }
  | simple_expr DOT label_longident
      { mkexp(Pexp_field($1, $3)) }
  | simple_expr DOT LPAREN seq_expr RPAREN
      { mkexp(Pexp_apply(mkexp(Pexp_ident(array_function "Array" "get")),
                         [$1; $4])) }
  | simple_expr DOT LPAREN seq_expr error
      { unclosed "(" 3 ")" 5 }
  | simple_expr DOT LBRACKET seq_expr RBRACKET
      { mkexp(Pexp_apply(mkexp(Pexp_ident(array_function "String" "get")),
                         [$1; $4])) }
  | simple_expr DOT LBRACKET seq_expr error
      { unclosed "[" 3 "]" 5 }
  | LBRACE lbl_expr_list opt_semi RBRACE
      { mkexp(Pexp_record(List.rev $2)) }
  | LBRACE lbl_expr_list opt_semi error
      { unclosed "{" 1 "}" 4 }
  | LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { mkexp(Pexp_array(List.rev $2)) }
  | LBRACKETBAR expr_semi_list opt_semi error
      { unclosed "[|" 1 "|]" 4 }
  | LBRACKETBAR BARRBRACKET
      { mkexp(Pexp_array []) }
  | LBRACKET expr_semi_list opt_semi RBRACKET
      { mklistexp(List.rev $2) }
  | LBRACKET expr_semi_list opt_semi error
      { unclosed "[" 1 "]" 4 }
  | PREFIXOP simple_expr
      { mkexp(Pexp_apply(mkoperator $1 1, [$2])) }
;
simple_expr_list:
    simple_expr
      { [$1] }
  | simple_expr_list simple_expr
      { $2 :: $1 }
;
let_bindings:
    let_binding                                 { [$1] }
  | let_bindings AND let_binding                { $3 :: $1 }
;
let_binding:
    val_ident fun_binding
      { ({ppat_desc = Ppat_var $1; ppat_loc = rhs_loc 1}, $2) }
  | pattern EQUAL seq_expr %prec prec_let
      { ($1, $3) }
;
fun_binding:
    EQUAL seq_expr %prec prec_let
      { $2 }
  | type_constraint EQUAL seq_expr %prec prec_let
      { mkexp(Pexp_constraint($3, $1)) }
  | simple_pattern fun_binding
      { mkexp(Pexp_function[$1,$2]) }
;
match_cases:
    pattern match_action                        { [$1, $2] }
  | match_cases BAR pattern match_action        { ($3, $4) :: $1 }
;
fun_def:
    match_action                                { $1 }
  | simple_pattern fun_def                      { mkexp(Pexp_function[$1,$2]) }
;
match_action:
    MINUSGREATER seq_expr                       { $2 }
  | WHEN seq_expr MINUSGREATER seq_expr         { mkexp(Pexp_when($2, $4)) }
;
expr_comma_list:
    expr_comma_list COMMA expr                  { $3 :: $1 }
  | expr COMMA expr                             { [$3; $1] }
;
lbl_expr_list:
    label_longident EQUAL expr %prec prec_list
      { [$1,$3] }
  | lbl_expr_list SEMI label_longident EQUAL expr %prec prec_list
      { ($3, $5) :: $1 }
;
expr_semi_list:
    expr %prec prec_list                        { [$1] }
  | expr_semi_list SEMI expr %prec prec_list    { $3 :: $1 }
;
type_constraint:
    COLON core_type                             { ($2) }
  | COLON error                                 { syntax_error() }
;
/*(*e: expression rules *)*/

/* Patterns */

/*(*s: pattern rules *)*/
pattern:
    simple_pattern
      { $1 }
  | pattern AS val_ident
      { mkpat(Ppat_alias($1, $3)) }
  | pattern_comma_list
      { mkpat(Ppat_tuple(List.rev $1)) }
  | constr_longident pattern %prec prec_constr_appl
      { mkpat(Ppat_construct($1, Some $2)) }
  | pattern COLONCOLON pattern
      { mkpat(Ppat_construct(Lident "::", Some(mkpat(Ppat_tuple[$1;$3]))
                             )) }
  | pattern BAR pattern
      { mkpat(Ppat_or($1, $3)) }
;
simple_pattern:
    val_ident
      { mkpat(Ppat_var $1) }
  | UNDERSCORE
      { mkpat(Ppat_any) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
  | CHAR DOTDOT CHAR
      { mkrangepat $1 $3 }
  | constr_longident
      { mkpat(Ppat_construct($1, None)) }
  | LBRACE lbl_pattern_list opt_semi RBRACE
      { mkpat(Ppat_record(List.rev $2)) }
  | LBRACE lbl_pattern_list opt_semi error
      { unclosed "{" 1 "}" 4 }
  | LBRACKET pattern_semi_list opt_semi RBRACKET
      { mklistpat(List.rev $2) }
  | LBRACKET pattern_semi_list opt_semi error
      { unclosed "{" 1 "}" 4 }
  | LPAREN pattern RPAREN
      { $2 }
  | LPAREN pattern error
      { unclosed "(" 1 ")" 3 }
  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" 1 ")" 5 }
;

pattern_comma_list:
    pattern_comma_list COMMA pattern            { $3 :: $1 }
  | pattern COMMA pattern                       { [$3; $1] }
;
pattern_semi_list:
    pattern                                     { [$1] }
  | pattern_semi_list SEMI pattern              { $3 :: $1 }
;
lbl_pattern_list:
    label_longident EQUAL pattern               { [($1, $3)] }
  | lbl_pattern_list SEMI label_longident EQUAL pattern { ($3, $5) :: $1 }
;
/*(*e: pattern rules *)*/

/* Type declarations */

/*(*s: type declaration rules *)*/
type_declarations:
    type_declaration                            { [$1] }
  | type_declarations AND type_declaration      { $3 :: $1 }
;
type_declaration:
    type_parameters LIDENT type_kind
      { let (kind, manifest) = $3 in
        ($2, {ptype_params = $1;
              ptype_kind = kind;
              ptype_manifest = manifest;
              ptype_loc = symbol_loc()}) }
;
type_kind:
  | EQUAL constructor_declarations
      { (Ptype_variant(List.rev $2), None) }
  | EQUAL BAR constructor_declarations
      { (Ptype_variant(List.rev $3), None) }
  | EQUAL LBRACE label_declarations opt_semi RBRACE
      { (Ptype_record(List.rev $3), None) }
  /*(*s: [[rule type_kind]] cases *)*/
  |  /*empty*/
      { (Ptype_abstract, None) }
  /*(*x: [[rule type_kind]] cases *)*/
  | EQUAL core_type %prec prec_type_def
      { (Ptype_abstract, Some $2) }
  /*(*x: [[rule type_kind]] cases *)*/
  | EQUAL core_type EQUAL opt_bar constructor_declarations %prec prec_type_def
      { (Ptype_variant(List.rev $5), Some $2) }
  | EQUAL core_type EQUAL LBRACE label_declarations opt_semi RBRACE
    %prec prec_type_def
      { (Ptype_record(List.rev $5), Some $2) }
  /*(*e: [[rule type_kind]] cases *)*/
;
type_parameters:
    /*empty*/                                   { [] }
  | type_parameter                              { [$1] }
  | LPAREN type_parameter_list RPAREN           { List.rev $2 }
;
type_parameter:
    QUOTE ident                                 { $2 }
;
type_parameter_list:
    type_parameter                              { [$1] }
  | type_parameter_list COMMA type_parameter    { $3 :: $1 }
;
constructor_declarations:
    constructor_declaration                     { [$1] }
  | constructor_declarations BAR constructor_declaration { $3 :: $1 }
;
constructor_declaration:
    constr_ident constructor_arguments          { ($1, $2) }
;
constructor_arguments:
    /*empty*/                                   { [] }
  | OF core_type_list                           { List.rev $2 }
;
label_declarations:
    label_declaration                           { [$1] }
  | label_declarations SEMI label_declaration   { $3 :: $1 }
;
label_declaration:
    mutable_flag LIDENT COLON core_type         { ($2, $1, $4) }
;
/*(*e: type declaration rules *)*/

/* Core types */

/*(*s: type expression rules *)*/

core_type:
    simple_core_type
      { $1 }
  | core_type MINUSGREATER core_type %prec prec_type_arrow
      { mktyp(Ptyp_arrow($1, $3)) }
  | core_type_tuple
      { mktyp(Ptyp_tuple(List.rev $1)) }
  /*(*s: [[rule core_type]] cases *)*/
  | core_type AS type_parameter
      { mktyp(Ptyp_alias($1, $3)) }
  /*(*e: [[rule core_type]] cases *)*/
;

simple_core_type:
    QUOTE ident
      { mktyp(Ptyp_var $2) }
  | type_longident
      { mktyp(Ptyp_constr($1, [])) }
  | simple_core_type type_longident %prec prec_constr_appl
      { mktyp(Ptyp_constr($2, [$1])) }
  | LPAREN core_type_comma_list RPAREN type_longident
      %prec prec_constr_appl
      { mktyp(Ptyp_constr($4, List.rev $2)) }
  | LPAREN core_type RPAREN
      { $2 }
  /*(*s: [[rule simple_core_type]] cases *)*/
  | UNDERSCORE
      { mktyp(Ptyp_any) }
  /*(*e: [[rule simple_core_type]] cases *)*/
;
core_type_tuple:
    simple_core_type STAR simple_core_type      { [$3; $1] }
  | core_type_tuple STAR simple_core_type       { $3 :: $1 }
;
core_type_comma_list:
    core_type COMMA core_type                   { [$3; $1] }
  | core_type_comma_list COMMA core_type        { $3 :: $1 }
;
core_type_list:
    simple_core_type                            { [$1] }
  | core_type_list STAR simple_core_type        { $3 :: $1 }
;
/*(*e: type expression rules *)*/

/* Identifiers and long identifiers */

/*(*s: name rules *)*/
ident:
    UIDENT                                      { $1 }
  | LIDENT                                      { $1 }
;
val_ident:
    LIDENT                                      { $1 }
  | LPAREN operator RPAREN                      { $2 }
;
operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | SUBTRACTIVE                                 { $1 }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
;
constr_ident:
    UIDENT                                      { $1 }
  | LBRACKET RBRACKET                           { "[]" }
  | LPAREN RPAREN                               { "()" }
  | COLONCOLON                                  { "::" }
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;
    
val_longident:
    val_ident                                   { Lident $1 }
  | mod_longident DOT val_ident                 { Ldot($1, $3) }
;
constr_longident:
    mod_longident                               { $1 }
  | LBRACKET RBRACKET                           { Lident "[]" }
  | LPAREN RPAREN                               { Lident "()" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
;
label_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;
type_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
mod_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;
mod_ext_longident:
    UIDENT                                      { Lident $1 }
  | mod_ext_longident DOT UIDENT                { Ldot($1, $3) }
;
mty_longident:
    ident                                       { Lident $1 }
  | mod_ext_longident DOT ident                 { Ldot($1, $3) }
;
/*(*e: name rules *)*/

/* Toplevel directives */

/*(*s: toplevel rules *)*/
toplevel_directive:
    SHARP ident                 { Ptop_dir($2, Pdir_none) }
  | SHARP ident STRING          { Ptop_dir($2, Pdir_string $3) }
  | SHARP ident INT             { Ptop_dir($2, Pdir_int $3) }
  | SHARP ident val_longident   { Ptop_dir($2, Pdir_ident $3) }
;
/*(*e: toplevel rules *)*/

/*(*s: extra rules *)*/
primitive_declaration:
    STRING                                      { [$1] }
  | STRING primitive_declaration                { $1 :: $2 }
;
/*(*x: extra rules *)*/
constant:
    INT                                         { Const_int $1 }
  | CHAR                                        { Const_char $1 }
  | STRING                                      { Const_string $1 }
  | FLOAT                                       { Const_float $1 }
;
signed_constant:
    constant                                    { $1 }
  | SUBTRACTIVE INT                             { Const_int(- $2) }
  | SUBTRACTIVE FLOAT                           { Const_float("-" ^ $2) }
;
/*(*e: extra rules *)*/

/* Miscellaneous */

/*(*s: misc rules *)*/
rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;
/*(*x: misc rules *)*/
opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;
/*(*e: misc rules *)*/
/*(*s: ebnf rules *)*/
/*(*e: ebnf rules *)*/
/*(*e: grammar *)*/
%%

/*(*e: ./parsing/parser.mly *)*/
