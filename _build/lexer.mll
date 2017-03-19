{
open Lexing
open Parser
open Printf

let incr_linenum lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
          { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1;
          }
}

let digit = ['0'-'9']
let letter = ['A'-'Z'  'a'-'z']
let white = [' ' '\t' '\r']
let newline = ['\n']
let hex = ['a'-'f' 'A'-'F' '0'-'9']
let id = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
(* the meat of the lexer *)
rule lexer = parse

(* add file inclusion support *)
  | "#" ([^ '\n'])* "\n" as includes          {incr_linenum lexbuf; lexer lexbuf}

(* Keywords *)
  | "bool"     {T_Bool}
  | "break"    {T_Break}
  | "byref"    {T_Byref}
  | "char"     {T_Char}
  | "continue" {T_Cont}
  | "delete"   {T_Del}
  | "double"   {T_Double}
  | "else"     {T_Else}
  | "for"      {T_For}
  | "false"    {T_False}
  | "if"       {T_If}
  | "int"      {T_Int}
  | "new"      {T_New}
  | "NULL"     {T_Null}
  | "return"   {T_Return}
  | "true"     {T_True}
  | "void"     {T_Void}

(* identifiers *)
  | letter id* as ident      {T_Id(ident)}

(* int constants *)
  | digit+ as digs {T_Const_Int(int_of_string digs)}

(* real constants *)
  | digit+ '.' ('e'|'E' ('-'|'+')? digit+)? as real {T_Const_Real(float_of_string real)}
  | digit+ '.' digit+ as real {T_Const_Real(float_of_string real)}

(* constant chars *)
  | "'" ([^ '\'' '\"'  '\\' ] | ( "\\n" | "\\t" | "\\r" | "\\0" | "\\\\" |  "\\\'" | "\\\"" | "\\x ['0'-'7'] hex")) "'"  as cchar {T_Const_Char(cchar)}

(* strings *)
  | '"' ([^ '\'' '\"'  '\\' '\n' ] | ( "\\n" | "\\t" | "\\r" | "\\0" | "\\\\" | "\\\'" | "\\\"" | "\\x ['0'-'7'] hex"))* '"' as str { T_Const_String(str) }

(* Symbolic Operators *)
  | "="     {T_Eq}
  | "=="    {T_Equal}
  | "!="    {T_Neq}
  | ">"     {T_Gr}
  | "<"     {T_Le}
  | ">="    {T_Geq}
  | "<="    {T_Leq}
  | "+"     {T_Add}
  | "-"     {T_Sub}
  | "*"     {T_Mul}
  | "/"     {T_Div}
  | "%"     {T_Mod}
  | "&"     {T_Amp}
  | "!"     {T_Not}
  | "&&"    {T_And}
  | "||"    {T_Or}
  | "?"     {T_Quest}
  | ":"     {T_Colon}
  | ","     {T_Comma}
  | "++"    {T_Incr}
  | "--"    {T_Decr}
  | "+="    {T_PlusEq}
  | "-="    {T_Minus_eq}
  | "*="    {T_Dot_eq}
  | "/="    {T_Div_eq}
  | "%="    {T_Mod_eq}

(* separators *)
  | ";"     {T_Semicolon}
  | "("     {T_Lparen}
  | ")"     {T_Rparen}
  | "["     {T_Lbracket}
  | "]"     {T_Rbracket}
  | "{"     {T_Lbrace}
  | "}"     {T_Rbrace}

  | white+         { lexer lexbuf }
  | newline+ { incr_linenum lexbuf ; lexer lexbuf}

  | "//" [^ '\n']* "\n" { incr_linenum lexbuf ; lexer lexbuf}
  | "/*" (_)* "*/*"     { comments  lexbuf }

  | _ as c              { printf "ERROR (%c), \n" c ; lexer lexbuf }

  | eof                 { T_Eof }

and comments =  parse
  | "*/" { lexer lexbuf }
  | "\n" { incr_linenum lexbuf; lexer lexbuf }
  | _    { comments lexbuf }
  | eof  { T_Eof}

