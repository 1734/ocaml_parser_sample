## 简介
本项目实现了一个类C语言的简单编译器前端，支持变量定义、赋值、运算、条件判断、循环等基本语法。可以打印抽象语法树。
## 清理
dune clean
## 构建
dune build
## 测试
dune runtest
## 语法BNF形式
```
<program> ::= /* empty */
           | <function_def> <program>

<basic_type> ::= "int"
              | "bool"

<function_def> ::= <basic_type> IDENTIFIER "(" <parameter_list> ")" <function_body_block>

<parameter_list> ::= /* empty */
                  | <basic_type> IDENTIFIER
                  | <basic_type> IDENTIFIER "," <parameter_list>

<function_body_block> ::= "{" <statement_list> "}"

<block> ::= "{" <statement_list> "}"

<statement_list> ::= /* empty */
                  | <statement> <statement_list>

<statement> ::= <variable_declaration_statement>
             | <assignment_statement>
             | <expression_statement>
             | <while_loop_statement>
             | <if_statement>
             | <block>
             | <return_statement>

<variable_declaration_statement> ::= <basic_type> IDENTIFIER <opt_init> ";"

<opt_init> ::= "=" <expression>
            | /* empty */

<variable_reference> ::= IDENTIFIER

<assignment_statement> ::= <variable_reference> "=" <expression> ";"

<expression_statement> ::= <expression> ";"

<expression> ::= <boolean_expression>
              | <arithmetic_expression>

<boolean_expression> ::= <boolean_expression> "||" <boolean_expression>
                     | <boolean_expression> "&&" <boolean_expression>
                     | "!" <boolean_expression>
                     | <arithmetic_expression> <rel_op> <arithmetic_expression>
                     | "(" <boolean_expression> ")"
                     | "true"
                     | "false"

<rel_op> ::= "=="
         | "!="
         | "<"
         | ">"
         | "<="
         | ">="

<arithmetic_expression> ::= <arithmetic_expression> "+" <arithmetic_expression>
                         | <arithmetic_expression> "-" <arithmetic_expression>
                         | <arithmetic_expression> "*" <arithmetic_expression>
                         | <arithmetic_expression> "/" <arithmetic_expression>
                         | "(" <arithmetic_expression> ")"
                         | INTV
                         | <variable_reference>

<while_loop_statement> ::= "while" "(" <boolean_expression> ")" <block>

<if_statement> ::= "if" "(" <boolean_expression> ")" <block> <else_part>

<else_part> ::= /* empty */
             | "else" <block>

<return_statement> ::= "return" <return_expression> ";"

<return_expression> ::= /* empty */
                    | <expression>

```
