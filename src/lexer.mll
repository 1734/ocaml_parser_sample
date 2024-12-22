(* 
   The lexical analyzer: lexer.ml is generated automatically
   from lexer.mll.
   
   The only modification commonly needed here is adding new keywords to the 
   list of reserved words at the top.  
*)

{
open Support.Error

let reservedWords = [
  (* Keywords *)
  ("int", fun i -> Parser.INT i);
  ("bool", fun i -> Parser.BOOL i);
  ("while", fun i -> Parser.WHILE i);
  ("if", fun i -> Parser.IF i);
  ("else", fun i -> Parser.ELSE i);
  ("true", fun i -> Parser.TRUE i);
  ("false", fun i -> Parser.FALSE i);
  ("return", fun i -> Parser.RETURN i);
  
  (* Symbols *)
  ("==", fun i -> Parser.EQEQ i);
  ("=", fun i -> Parser.ASSIGN i);
  ("!=", fun i -> Parser.NE i);
  ("!", fun i -> Parser.NOT i);
  (">=", fun i -> Parser.GE i);
  (">", fun i -> Parser.GT i);
  ("{", fun i -> Parser.LBRACE i); 
  ("(", fun i -> Parser.LPAREN i); 
  ("<=", fun i -> Parser.LE i); 
  ("<", fun i -> Parser.LT i); 
  ("}", fun i -> Parser.RBRACE i);
  (")", fun i -> Parser.RPAREN i);
  (";", fun i -> Parser.SEMI i);
  (",", fun i -> Parser.COMMA i);
  ("+", fun i -> Parser.PLUS i);
  ("-", fun i -> Parser.SUB i);
  ("*", fun i -> Parser.MUL i);
  ("/", fun i -> Parser.DIV i);
  ("||", fun i -> Parser.OR i);
  ("&&", fun i -> Parser.AND i);

  (* Special compound symbols: *)
]

(* Support functions *)

type buildfun = info -> Parser.token
let (symbolTable : (string,buildfun) Hashtbl.t) = Hashtbl.create 1024
let _ =
  List.iter (fun (str,f) -> Hashtbl.add symbolTable str f) reservedWords

let createID i str =
  try (Hashtbl.find symbolTable str) i
  with _ ->
       Parser.IDENTIFIER {i=i;v=str}

let lineno   = ref 1
and depth    = ref 0
and start    = ref 0

and filename = ref ""
and startLex = ref dummyinfo

let create inFile stream =
  if not (Filename.is_implicit inFile) then filename := inFile
  else filename := Filename.concat (Sys.getcwd()) inFile;
  lineno := 1; start := 0; Lexing.from_channel stream

let newline lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)

let info lexbuf =
  createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)

let text = Lexing.lexeme

let stringBuffer = ref (Bytes.create 2048)
let stringEnd = ref 0

let resetStr () = stringEnd := 0

let addStr ch =
  let x = !stringEnd in
  let buffer = !stringBuffer
in
  if x = Bytes.length buffer then
    begin
      let newBuffer = Bytes.create (x*2) in
      Bytes.blit buffer 0 newBuffer 0 x;
      Bytes.set newBuffer x ch;
      stringBuffer := newBuffer;
      stringEnd := x+1
    end
  else
    begin
      Bytes.set buffer x ch;
      stringEnd := x+1
    end

let getStr () = Bytes.sub_string (!stringBuffer) 0 (!stringEnd)

let extractLineno yytext offset =
  int_of_string (String.sub yytext offset (String.length yytext - offset))
}


(* The main body of the lexical analyzer *)

rule main = parse
  [' ' '\009' '\012']+     { main lexbuf }

| [' ' '\009' '\012']*"\n" { newline lexbuf; main lexbuf }

| ['0'-'9']+
    { Parser.INTV{i=info lexbuf; v=int_of_string (text lexbuf)} }

| ['A'-'Z' 'a'-'z' '_']
  ['A'-'Z' 'a'-'z' '_' '0'-'9']*
    { createID (info lexbuf) (text lexbuf) }

| "!=" | "==" | "<=" | ">=" | "&&" | "||"
    { createID (info lexbuf) (text lexbuf) }

| ['+' '-' '*' '/' '(' ')' '{' '}' '<' '>' ';' '!' '=' ',']
    { createID (info lexbuf) (text lexbuf) }

| eof { Parser.EOF(info lexbuf) }

| _  { error (info lexbuf) "Illegal character" }

(*  *)
