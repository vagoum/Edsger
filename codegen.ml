open Llvm
exception Error of string

let context = global_contect ()
let the_module = create_module context "My cmp "
let builder = builder context

let rec codegen_expr = function 
        |
