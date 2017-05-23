(*let main =
  try
    Parser.program Lexer.lexer (Lexing.from_channel stdin);
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "smth";
    exit 1*)
open Ast
open Llvm
open Format
let main =
let lexbuf = Lexing.from_channel stdin in
try
        (Parser.program Lexer.lexer lexbuf);
 (*      let _ = print_teliko !ast_tree in*)
    let a=    Codegen.codegen_main !ast_tree 
    in print_module ("a2.ll") a 
with
| Failure msg -> print_endline ("Failure in " ^ msg)
| Parser.Error -> print_endline "Parse error"
| End_of_file ->
 print_endline "Parse error: unexpected end of string"

(*
open Core.Std
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.program Lexer.lexer lexbuf with
  | Lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)
let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    printf "%a\n" Json.output_value value;
    parse_and_print lexbuf
  | None -> ()

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx*)
        
