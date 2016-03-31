{
open Lexing
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

(* the meat of the lexer *)
rule edsger = parse

(* add file inclusion support *)
  | "#" ([^ '\n'])* "\n" as includes          {printf "%s" includes ; incr_linenum lexbuf; edsger lexbuf}

(* Keywords *)
  | "bool"
  | "break"
  | "byref"
  | "char"
  | "continue"
  | "delete"
  | "double"
  | "else"
  | "for"
  | "false"
  | "if"
  | "int"
  | "new"
  | "NULL"
  | "return"
  | "true"
  | "void"    as keyw                           {printf "%s" keyw;edsger lexbuf}

(* identifiers *)
  | letter id* as lelel                         {printf "%s" lelel; edsger  lexbuf }

(* int constants *)
  | digit+   as dig                             {let num = int_of_string dig in printf "%d" num; edsger lexbuf}

(* real constants *)
  | digit+ '.' ('e'|'E' ('-'|'+')? digit+)? as real {printf "%s" real; edsger lexbuf}


(* constant chars *)
  | "'" ([^ '\'' '\"'  '\\' ] | ( "\\n" | "\\t" | "\\r" | "\\0" | "\\\\" |  "\\\'" | "\\\"" | "\\x ['0'-'7'] hex")) "'"  as cchar {printf "%s" cchar;edsger lexbuf}

(* strings *)
  | '"' ([^ '\'' '\"'  '\\' '\n' ] | ( "\\n" | "\\t" | "\\r" | "\\0" | "\\\\" | "\\\'" | "\\\"" | "\\x ['0'-'7'] hex"))* '"' as str { printf "%s" str;edsger lexbuf}

(* Symbolic Operators *)
  | "="
  | "=="
  | "!="
  | ">"
  | "<"
  | ">="
  | "<="
  | "+"
  | "-"
  | "*"
  | "/"
  | "%"
  | "&"
  | "!"
  | "&&"
  | "||"
  | "?"
  | ":"
  | ","
  | "++"
  | "--"
  | "+="
  | "-="
  | "*="
  | "/="
  | "%="    as symop {Printf.printf "%s" symop; edsger lexbuf}

(* separators *)
  | ";"
  | "("
  | ")"
  | "["
  | "]"
  | "{"
  | "}"  as sep         {Printf.printf "%c" sep; edsger lexbuf}

  | white+  as ig       { Printf.printf "%s" ig;edsger lexbuf }
  | newline+  as ig     { Printf.printf "%s" ig;incr_linenum lexbuf ; edsger lexbuf}

  | "//" [^ '\n']* "\n" { incr_linenum lexbuf ; edsger lexbuf}
  | "/*" (_)* "*/*"     { comments  lexbuf }

  | _ as c              { printf "ERROR (%c), \n" c ; edsger lexbuf }

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
