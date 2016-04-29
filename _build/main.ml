open Formula

let formula_of_string s = Parser.parse_formula Lexer.lex (Lexing.from_string s)

let _ = print_endline (str (nnf (formula_of_string "not (X and not Y)")))
    
