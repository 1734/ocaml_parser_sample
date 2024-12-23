/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> INT
%token <Support.Error.info> BOOL
%token <Support.Error.info> WHILE
%token <Support.Error.info> IF
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> RETURN

%token <int Support.Error.withinfo> INTV
%token <string Support.Error.withinfo> IDENTIFIER

/* Symbolic tokens */
%token <Support.Error.info> EOF
%token <Support.Error.info> ASSIGN
%token <Support.Error.info> NE
%token <Support.Error.info> EQEQ
%token <Support.Error.info> NOT
%token <Support.Error.info> GT
%token <Support.Error.info> GE
%token <Support.Error.info> LBRACE
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LT
%token <Support.Error.info> LE
%token <Support.Error.info> RBRACE
%token <Support.Error.info> RPAREN
%token <Support.Error.info> SEMI
%token <Support.Error.info> COMMA
%token <Support.Error.info> PLUS
%token <Support.Error.info> SUB
%token <Support.Error.info> MUL
%token <Support.Error.info> DIV
%token <Support.Error.info> OR
%token <Support.Error.info> AND

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start start
%type < Syntax.context -> (Syntax.a_function_def list * Syntax.context) > start

%left OR
%left AND
%right NOT
%left EQEQ NE LT LE GT GE
%left PLUS SUB
%left MUL DIV

%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */

start:
  | program EOF { fun ctx -> $1 (push_scope ctx) }

program:
  | /* empty */ { fun ctx -> [], ctx}
  | function_def program { fun ctx -> let (f,ctx') = $1 ctx in let (p,ctx'') = $2 ctx' in (f::p, ctx'') }

basic_type:
  | INT { A_int_type $1 }
  | BOOL { A_bool_type $1 }

function_def:
  | basic_type IDENTIFIER LPAREN parameter_list RPAREN function_body_block {
    fun ctx : (a_function_def * _) ->
      let sym : a_symbol_decl = { info = getBasicTypeInfo $1; name = $2.v; ptype = A_function_type($2.i) } in
      let ctx' = add_symbol ctx sym in
      let (ps, ctx'') = $4 (add_symbol (push_scope ctx') sym) in
      { info = getBasicTypeInfo $1; name = $2.v; returntype = $1; params = ps; body = $6 ctx'' }, ctx'
    }

parameter_list:
  | /* empty */ { fun ctx -> [], ctx }
  | basic_type IDENTIFIER { fun ctx -> 
    let sym : a_symbol_decl = { info = getBasicTypeInfo $1; name = $2.v; ptype = A_basic_type($1) } in
    let p : a_parameter = { info = sym.info; declared_symbol = sym } in
    (p :: [], add_symbol ctx sym) }
  | basic_type IDENTIFIER COMMA parameter_list {
    fun ctx -> 
      let sym : a_symbol_decl = { info = getBasicTypeInfo $1; name = $2.v; ptype = A_basic_type($1) } in
      let p : a_parameter = { info = sym.info; declared_symbol = sym } in
      let ctx' = add_symbol ctx sym in
      let (ps, ctx'') = $4 ctx' in
    (p :: ps, ctx'')
    }

block:
  | LBRACE statement_list RBRACE { fun ctx : a_block -> match ($2 (push_scope ctx)) with (ss, _) -> { info = $1; statements = ss } }

function_body_block:
  | LBRACE statement_list RBRACE { fun ctx : a_block -> match ($2 ctx) with (ss, _) -> { info = $1; statements = ss } }

statement_list:
  | /* empty */ { fun ctx -> [], ctx }
  | statement statement_list { fun ctx -> let (s, ctx') = $1 ctx in let (ss, ctx'') = $2 ctx' in (s::ss, ctx'') }

statement:
  | variable_declaration_statement { fun ctx -> $1 ctx }
  | assignment_statement { fun ctx -> $1 ctx, ctx }
  | expression_statement { fun ctx -> $1 ctx, ctx }
  | while_loop_statement { fun ctx -> $1 ctx, ctx }
  | if_statement { fun ctx -> $1 ctx, ctx }
  | block { fun ctx -> A_block_statement($1 ctx), ctx }
  | return_statement { fun ctx -> $1 ctx, ctx }

variable_declaration_statement:
  | basic_type IDENTIFIER opt_init SEMI {
    fun ctx : (a_statement * _) ->
      let sym : a_symbol_decl = { info = getBasicTypeInfo $1; name = $2.v; ptype = A_basic_type($1) } in
      let ctx' = add_symbol ctx sym in
      A_variable_declaration_statement { info = getBasicTypeInfo $1; declared_symbol = sym; init = $3 ctx }, ctx'
    }

opt_init:
  | ASSIGN expression { fun ctx -> Some($2 ctx) }
  | /* empty */ { fun ctx -> None }

variable_reference:
  | IDENTIFIER { fun ctx : a_variable_reference ->
    let sym : a_symbol_decl option = find_symbol ctx $1.v in
    match sym with
    | Some(s) -> ({ info = $1.i; refered_symbol = s } : a_variable_reference)
    | None -> failwith (infoToString $1.i ^ " Variable " ^ $1.v ^ " not declared")
  }

assignment_statement:
  | variable_reference ASSIGN expression SEMI { fun ctx ->
    let vr : a_variable_reference = $1 ctx in
    A_assignment_statement { info = vr.info; assigned_symbol = vr; value = $3 ctx } }

expression_statement:
  | expression SEMI { fun ctx -> A_expression_statement($1 ctx) }

expression:
  | boolean_expression { fun ctx -> A_boolean_expression($1 ctx) }
  | arithmetic_expression { fun ctx -> A_arithmetic_expression($1 ctx) }

boolean_expression:
  | boolean_expression OR boolean_expression { fun ctx -> A_or_boolean_expression { info = $2; left = $1 ctx; right = $3 ctx } }
  | boolean_expression AND boolean_expression { fun ctx -> A_and_boolean_expression { info = $2; left = $1 ctx; right = $3 ctx } }
  | NOT boolean_expression { fun ctx -> A_not_boolean_expression { info = $1; expr = $2 ctx } }
  | arithmetic_expression rel_op arithmetic_expression { fun ctx -> A_relation_expression { info = getRelOpInfo $2; op = $2; left = $1 ctx; right = $3 ctx } }
  | LPAREN boolean_expression RPAREN { fun ctx -> $2 ctx }
  | TRUE { fun ctx -> A_bool_constant { info = $1; value = true } }
  | FALSE { fun ctx -> A_bool_constant { info = $1; value = false } }

rel_op:
  | EQEQ { A_eqeq $1 }
  | NE { A_neq $1 }
  | LT { A_lt $1 }
  | GT { A_gt $1 }
  | LE { A_le $1 }
  | GE { A_ge $1 }

arithmetic_expression:
  | arithmetic_expression PLUS arithmetic_expression { fun ctx -> A_binary_arithmetic_expression { info = $2; op = A_plus $2; left = $1 ctx; right = $3 ctx } }
  | arithmetic_expression SUB arithmetic_expression { fun ctx -> A_binary_arithmetic_expression { info = $2; op = A_sub $2; left = $1 ctx; right = $3 ctx } }
  | arithmetic_expression MUL arithmetic_expression { fun ctx -> A_binary_arithmetic_expression { info = $2; op = A_mul $2; left = $1 ctx; right = $3 ctx } }
  | arithmetic_expression DIV arithmetic_expression { fun ctx -> A_binary_arithmetic_expression { info = $2; op = A_div $2; left = $1 ctx; right = $3 ctx } }
  | LPAREN arithmetic_expression RPAREN { fun ctx -> $2 ctx }
  | INTV { fun ctx -> A_interger_constant { info = $1.i; value = $1.v } }
  | variable_reference { fun ctx -> A_variable_reference($1 ctx) }

while_loop_statement:
  | WHILE LPAREN boolean_expression RPAREN block { fun ctx ->
      A_while_loop_statement { info = $1; condition = $3 ctx; body = $5 ctx } }

if_statement:
  | IF LPAREN boolean_expression RPAREN block else_part {
      fun ctx -> A_if_statement { info = $1; condition = $3 ctx; then_body = $5 ctx; else_body = $6 ctx } }

else_part:
  | /* empty */ { fun ctx -> None }
  | ELSE block { fun ctx -> Some($2 ctx) }

return_statement:
  | RETURN return_expression SEMI { fun ctx -> A_return_statement { info = $1; value = $2 ctx } }

return_expression:
  | /* empty */ { fun ctx -> None }
  | expression { fun ctx -> Some($1 ctx) }

/*   */
