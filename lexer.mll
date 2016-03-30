{
open Lexing

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


(* the meat of the lexer *)
rule edsger = parse

  | "#"                 {}

(* Keywords *)
  | "bool"              {}
  | "break"             {}
  | "byref"             {}
  | "char"              {}
  | "continue"          {}
  | "delete"            {}
  | "double"            {}
  | "else"              {}
  | "for"               {}
  | "false"             {}
  | "if"                {}
  | "int"               {}
  | "new"               {}
  | "NULL"              {}
  | "return"            {}
  | "true"              {}
  | "void"              {}

(* identifiers *)
  | letter id*          {}

(* int constants *)
  | digit+              {}

(* real constants *)
  | digit+ '.' ('e'|'E' ('-'|'+')? digit+)? {}

(* constant chars *)
  | "'" ([^ '\'' '\"'  '\\' ] | ( "\\n" | "\\t" | "\\r" | "\\0" | "\\\\" |  "\\\'" | "\\\"" )) "'" 

(* strings *)
  | '"' ([^ '\'' '\"'  '\\' '\n' ] | ( "\\n" | "\\t" | "\\r" | "\\0" | "\\\\" |  "\\\'" | "\\\"" ))* '"' 

(* Operators *)
  | "="                 {}
  | "=="                {}
  | "!="                {}
  | ">"                 {}
  | "<"                 {}
  | ">="                {}
  | "<="                {}
  | "+"                 {}
  | "-"                 {}
  | "*"                 {}
  | "/"                 {}
  | "%"                 {}
  | "&"                 {}
  | "!"                 {}
  | "&&"                {}
  | "||"                {}
  | "?"                 {}
  | ":"                 {}
  | ","                 {}
  | "++"                {}
  | "--"                {}
  | "+="                {}
  | "-="                {}
  | "*="                {}
  | "/="                {}
  | "%="                {}

(* separators *)
  | ";"                 {}
  | "("                 {}
  | ")"                 {}
  | "["                 {}
  | "]"                 {}
  | "{"                 {}
  | "}"                 {}

  | white+              { edsger lexbuf }
  | newline             { incr_linenum lexbuf ; edsger lexbuf}

  | "//" [^ '\n']* "\n" { incr_linenum lexbuf ; edsger lexbuf}
  | "/*" (_)* "*/*"     { comments  lexbuf } 

  | _ as c              { Printf.printf "Unrecognized character %c\n" c; }

  | eof                 { raise End_of_file }

and comments =  parse
  | "*/" { edsger lexbuf }
  | "\n" { incr_linenum lexbuf; edsger lexbuf }
  | _    { comments lexbuf }
  | eof  { }

  

{
let main () =
  let cin =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel cin in
  try edsger lexbuf
  with End_of_file -> ()
  let _ = Printexc.print main ()
}
