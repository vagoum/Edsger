(*open Ast
let get_function_entry f = match f.entry_info with
        |ENTRY_function a -> a
let  lamda_lift tree = match tree with
        |VarDecl a -> a
        |FunDecl a -> a
        |FunDef (a,b)-> lamda_fun_def a b [] []

let rec lamda_fun_def a b variables decls= match b with
        | [] -> decls
        | c::d -> (match c with
                        |VarDecl e -> lamda_fun_def a b (e::variables)
        )*)
