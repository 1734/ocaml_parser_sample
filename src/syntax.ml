open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type a_basic_type = 
  | A_int_type of info
  | A_bool_type of info

type a_symbol_type =
  | A_basic_type of a_basic_type
  | A_function_type of info

let getBasicTypeInfo t = match t with
  | A_int_type(info) -> info
  | A_bool_type(info) -> info

let getSymbolTypeInfo t = match t with
  | A_basic_type(basic_type) -> getBasicTypeInfo basic_type
  | A_function_type(info) -> info

type a_symbol_decl = { info : info; name : string; ptype : a_symbol_type }

type a_variable_reference = { info : info; refered_symbol : a_symbol_decl }

type a_binary_arithmetic_op = 
  | A_plus of info
  | A_sub of info
  | A_mul of info
  | A_div of info

let getBinOpInfo t = match t with
  | A_plus(info) -> info
  | A_sub(info) -> info
  | A_mul(info) -> info
  | A_div(info) -> info

type a_arithmetic_expression =
  | A_binary_arithmetic_expression of { info : info; op : a_binary_arithmetic_op; left : a_arithmetic_expression; right : a_arithmetic_expression }
  | A_interger_constant of { info : info; value : int }
  | A_variable_reference of a_variable_reference

type a_relation_op = 
  | A_eqeq of info
  | A_neq of info
  | A_lt of info
  | A_gt of info
  | A_le of info
  | A_ge of info

let getRelOpInfo t = match t with
  | A_eqeq(info) -> info
  | A_neq(info) -> info
  | A_lt(info) -> info
  | A_gt(info) -> info
  | A_le(info) -> info
  | A_ge(info) -> info

type a_boolean_expression =
  | A_or_boolean_expression of { info : info; left : a_boolean_expression; right : a_boolean_expression }
  | A_and_boolean_expression of { info : info; left : a_boolean_expression; right : a_boolean_expression }
  | A_not_boolean_expression of { info : info; expr : a_boolean_expression }
  | A_relation_expression of { info : info; op: a_relation_op; left : a_arithmetic_expression; right : a_arithmetic_expression }
  | A_bool_constant of { info : info; value : bool }

type a_expression =
  | A_boolean_expression of a_boolean_expression
  | A_arithmetic_expression of a_arithmetic_expression

type a_statement =
  | A_variable_declaration_statement of { info : info; declared_symbol : a_symbol_decl; init : a_expression option }
  | A_assignment_statement of { info : info; assigned_symbol : a_variable_reference; value : a_expression }
  | A_expression_statement of a_expression
  | A_while_loop_statement of { info : info; condition : a_boolean_expression; body : a_block }
  | A_if_statement of { info : info; condition : a_boolean_expression; then_body : a_block; else_body : a_block option }
  | A_block_statement of a_block
  | A_return_statement of { info : info; value : a_expression option }

and a_block = { info : info; statements: a_statement list }

type a_parameter = { info : info; declared_symbol : a_symbol_decl }

type a_function_def = { info : info; name : string; returntype : a_basic_type; params : a_parameter list; body : a_block }

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

(* ---------------------------------------------------------------------- *)
(* Context management *)

type scope = a_symbol_decl list

type context = scope list

let push_scope (s : context) : context = [] :: s

let pop_scope (s : context) : context = match s with
  | [] -> failwith "Error: Cannot pop from an empty stack"
  | _ :: rest -> rest

let add_symbol (ctx : context) (sym : a_symbol_decl) : context = match ctx with
  | [] -> failwith "Error: Cannot add symbol to an empty context"
  | current_scope :: rest -> 
    let result = 
      try Some(List.find (fun (sym1 : a_symbol_decl) -> sym1.name = sym.name) current_scope) 
      with Not_found -> None 
    in match result with
    | Some(existing_sym) ->
      failwith ("Error: Duplicated symbol name: " ^ (infoToString sym.info) ^ sym.name ^
            ", already declared in" ^ (infoToString existing_sym.info))
    | None -> (sym :: current_scope) :: rest

let rec find_symbol (ctx : context) (name : string) : a_symbol_decl option = match ctx with
  | [] -> None
  | current_scope :: rest -> 
    try Some(List.find (fun (sym : a_symbol_decl) -> sym.name = name) current_scope)
    with Not_found -> find_symbol rest name

let emptycontext = []

let ctxlength ctx = List.length ctx

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_vbox 2
let obox() = open_vbox 2
let cbox() = close_box()
let break() = print_break 0 0

let print_a_basic_type (t : a_basic_type) = match t with
  | A_int_type(_) -> print_string "int"
  | A_bool_type(_) -> print_string "bool"

let print_a_symbol_type (t : a_symbol_type) = match t with
  | A_basic_type(basic_type) -> print_a_basic_type basic_type
  | A_function_type(_) -> print_string "function"

let print_a_variable_reference (v : a_variable_reference) = 
  print_string v.refered_symbol.name

let print_a_binary_arithmetic_op (op : a_binary_arithmetic_op) = match op with
  | A_plus(_) -> print_string "+"
  | A_sub(_) -> print_string "-"
  | A_mul(_) -> print_string "*"
  | A_div(_) -> print_string "/"

let rec print_a_arithmetic_expression (e : a_arithmetic_expression) = match e with
  | A_binary_arithmetic_expression({info; op; left; right}) -> 
    print_space();
    obox0();
    print_a_binary_arithmetic_op op;
    print_a_arithmetic_expression left;
    print_a_arithmetic_expression right;
    cbox()
  | A_interger_constant({info;value}) -> print_space(); print_int value
  | A_variable_reference(v) -> print_space(); print_a_variable_reference v

let print_a_relation_op (op : a_relation_op) = match op with
  | A_eqeq(_) -> print_string "=="
  | A_neq(_) -> print_string "!="
  | A_lt(_) -> print_string "<"
  | A_gt(_) -> print_string ">"
  | A_le(_) -> print_string "<="
  | A_ge(_) -> print_string ">="

let rec print_a_boolean_expression (e : a_boolean_expression) = match e with
  | A_or_boolean_expression({info; left; right}) -> 
    print_space();
    obox0();
    print_string "||";
    print_a_boolean_expression left;
    print_a_boolean_expression right;
    cbox();
  | A_and_boolean_expression({info; left; right}) -> 
    print_space();
    obox0();
    print_string "&&";
    print_a_boolean_expression left;
    print_a_boolean_expression right;
    cbox();
  | A_not_boolean_expression({info; expr}) -> 
    print_space();
    obox0();
    print_string "!";
    print_space();
    print_a_boolean_expression expr;
    cbox();
  | A_relation_expression({info; op; left; right}) -> 
    print_space();
    obox0();
    print_a_relation_op op;
    print_a_arithmetic_expression left;
    print_a_arithmetic_expression right;
    cbox();
  | A_bool_constant({info; value}) -> print_space(); print_string (if value then "true" else "false")

let print_a_expression (e : a_expression) = match e with
  | A_boolean_expression(expr) -> print_a_boolean_expression expr
  | A_arithmetic_expression(expr) -> print_a_arithmetic_expression expr

let rec print_a_statement (s : a_statement) = match s with
  | A_variable_declaration_statement({info; declared_symbol; init}) -> 
    print_space();
    obox0();
    print_string "variable declaration statement:";
    print_space();
    print_string declared_symbol.name;
    print_string " of type ";
    print_a_symbol_type declared_symbol.ptype;
    (match init with
    | Some(expr) -> 
      print_space();
      obox0();
      print_string "initiation:";
      print_a_expression expr;
      cbox();
    | None -> ());
    cbox();
  | A_assignment_statement({info; assigned_symbol; value}) -> 
    print_space();
    obox0();
    print_string "assignment statement:";
    print_space();
    obox0();
    print_string "=";
    print_space();
    print_a_variable_reference assigned_symbol;
    print_a_expression value;
    cbox();
    cbox();
  | A_expression_statement(expr) -> 
    print_space();
    obox0();
    print_string "expression statement:";
    print_a_expression expr;
    cbox();
  | A_while_loop_statement({info; condition; body}) -> 
    print_space();
    obox0();
    print_string "while loop statement:";
    print_space();
    obox0();
    print_string "condition:";
    print_a_boolean_expression condition;
    cbox();
    print_space();
    obox0();
    print_string "body:";
    print_a_block body;
    cbox();
    cbox()
  | A_if_statement({info; condition; then_body; else_body}) -> 
    print_space();
    obox0();
    print_string "if statement:";
    print_space();
    obox0();
    print_string "condition:";
    print_a_boolean_expression condition;
    cbox();
    print_space();
    obox0();
    print_string "then:";
    print_a_block then_body;
    cbox();
    (match else_body with
    | Some(else_body) ->
      print_space();
      obox0();
      print_string "else";
      print_a_block else_body;
      cbox()
    | None -> ());
    cbox()
  | A_block_statement(block) -> print_a_block block
  | A_return_statement({info; value}) -> 
    print_space();
    obox0();
    print_string "return statement:";
    (match value with
    | Some(expr) -> 
      print_a_expression expr
    | None -> ());
    cbox()

and print_a_block (b : a_block) =
  print_space();
  obox0();
  print_string "block:";
  List.iter print_a_statement b.statements;
  cbox()

let print_a_parameter (p : a_parameter) =
  print_space();
  obox0();
  print_string "parameter:";
  print_space();
  print_string p.declared_symbol.name;
  print_string " of type ";
  print_a_symbol_type p.declared_symbol.ptype;
  cbox()

let print_a_function_def (f : a_function_def) =
  print_space();
  obox0();
  print_string ("function definition: " ^ (infoToString f.info));
  print_space();
  print_string ("function name: " ^ f.name);
  print_space();
  print_string "return type: ";
  print_a_basic_type f.returntype;
  print_space();
  obox0();
  print_string "parameter list:";
  if List.length f.params = 0 then
    print_string " (empty)"
  else
  List.iter print_a_parameter f.params;
  cbox();
  print_space();
  obox0();
  print_string "body:";
  print_a_block f.body;
  cbox();
  cbox()
