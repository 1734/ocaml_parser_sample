(* module Syntax: syntax trees and associated support functions *)

(* open Support.Pervasive *)
open Support.Error

(* Data type definitions *)
type a_basic_type =
    A_int_type of Support.Error.info
  | A_bool_type of Support.Error.info
type a_symbol_type =
    A_basic_type of a_basic_type
  | A_function_type of Support.Error.info
val getBasicTypeInfo : a_basic_type -> Support.Error.info
val getSymbolTypeInfo : a_symbol_type -> Support.Error.info
type a_symbol_decl = {
  info : Support.Error.info;
  name : string;
  ptype : a_symbol_type;
}
type a_variable_reference = {
  info : Support.Error.info;
  refered_symbol : a_symbol_decl;
}
type a_binary_arithmetic_op =
    A_plus of Support.Error.info
  | A_sub of Support.Error.info
  | A_mul of Support.Error.info
  | A_div of Support.Error.info
val getBinOpInfo : a_binary_arithmetic_op -> Support.Error.info
type a_arithmetic_expression =
    A_binary_arithmetic_expression of { info : Support.Error.info;
      op : a_binary_arithmetic_op; left : a_arithmetic_expression;
      right : a_arithmetic_expression;
    }
  | A_interger_constant of { info : Support.Error.info; value : int; }
  | A_variable_reference of a_variable_reference
type a_relation_op =
    A_eqeq of Support.Error.info
  | A_neq of Support.Error.info
  | A_lt of Support.Error.info
  | A_gt of Support.Error.info
  | A_le of Support.Error.info
  | A_ge of Support.Error.info
val getRelOpInfo : a_relation_op -> Support.Error.info
type a_boolean_expression =
    A_or_boolean_expression of { info : Support.Error.info;
      left : a_boolean_expression; right : a_boolean_expression;
    }
  | A_and_boolean_expression of { info : Support.Error.info;
      left : a_boolean_expression; right : a_boolean_expression;
    }
  | A_not_boolean_expression of { info : Support.Error.info;
      expr : a_boolean_expression;
    }
  | A_relation_expression of { info : Support.Error.info; op : a_relation_op;
      left : a_arithmetic_expression; right : a_arithmetic_expression;
    }
  | A_bool_constant of { info : Support.Error.info; value : bool; }
type a_expression =
    A_boolean_expression of a_boolean_expression
  | A_arithmetic_expression of a_arithmetic_expression
type a_statement =
    A_variable_declaration_statement of { info : Support.Error.info;
      declared_symbol : a_symbol_decl; init : a_expression option;
    }
  | A_assignment_statement of { info : Support.Error.info;
      assigned_symbol : a_variable_reference; value : a_expression;
    }
  | A_expression_statement of a_expression
  | A_while_loop_statement of { info : Support.Error.info;
      condition : a_boolean_expression; body : a_block;
    }
  | A_if_statement of { info : Support.Error.info;
      condition : a_boolean_expression; then_body : a_block;
      else_body : a_block option;
    }
  | A_block_statement of a_block
  | A_return_statement of { info : Support.Error.info;
      value : a_expression option;
    }
and a_block = { info : Support.Error.info; statements : a_statement list; }
type a_parameter = {
  info : Support.Error.info;
  declared_symbol : a_symbol_decl;
}
type a_function_def = {
  info : Support.Error.info;
  name : string;
  returntype : a_basic_type;
  params : a_parameter list;
  body : a_block;
}
type scope = a_symbol_decl list
type context = scope list
val push_scope : context -> context
val pop_scope : context -> context
val add_symbol : context -> a_symbol_decl -> context
val find_symbol : context -> string -> a_symbol_decl option
val emptycontext : 'a list
val ctxlength : 'a list -> int
val obox0 : unit -> unit
val obox : unit -> unit
val cbox : unit -> unit
val break : unit -> unit
val print_a_basic_type : a_basic_type -> unit
val print_a_symbol_type : a_symbol_type -> unit
val print_a_variable_reference : a_variable_reference -> unit
val print_a_binary_arithmetic_op : a_binary_arithmetic_op -> unit
val print_a_arithmetic_expression : a_arithmetic_expression -> unit
val print_a_relation_op : a_relation_op -> unit
val print_a_boolean_expression : a_boolean_expression -> unit
val print_a_expression : a_expression -> unit
val print_a_statement : a_statement -> unit
val print_a_block : a_block -> unit
val print_a_parameter : a_parameter -> unit
val print_a_function_def : a_function_def -> unit
