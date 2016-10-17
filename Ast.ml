open Format
open Symbol
open Types

type ast_program = ast_decl list 

and ast_decl = VarDecl of ast_var_decl
              | FunDecl of ast_fun_decl
              | FunDef of ast_fun_def
and ast_var_decl = entry list
and ast_fun_decl = entry
and ast_fun_def = ast_fun_decl *ast_decl list * ast_stmt list

and ast_param = Param of string*typ
              | ParamByRef of string*typ

and ast_stmt = SExpr of ast_expr option
             | SNewblock of ast_stmt list
             | Sfor of string option *ast_expr option* ast_expr  option* ast_expr  option* ast_stmt
             | Sif of ast_expr * ast_stmt * ast_stmt option
             | Sreturn of ast_expr option
             | SBreak of string option
             | SCont of string option

and ast_expr = Eint of int
             | Ereal of float
             | Echar of char
             | Eid of string
             | Estring of string
             | Ebool of bool
             | ENull
             | EAmber of ast_expr
             | EPointer of ast_expr
             | EUnAdd of ast_expr 
             | EUnMinus of ast_expr
             | Eplus of ast_expr * ast_expr
             | Eminus of ast_expr * ast_expr
             | Ediv of ast_expr * ast_expr
             | Emult of ast_expr * ast_expr
             | Emod of ast_expr * ast_expr
             | Eand of ast_expr * ast_expr
             | Eor of ast_expr * ast_expr
             | Ecomma of ast_expr * ast_expr
             | Elt of ast_expr * ast_expr
             | Elte of ast_expr * ast_expr
             | Egt of ast_expr * ast_expr
             | Egte of ast_expr * ast_expr
             | Eeq of ast_expr * ast_expr
             | Eneq of ast_expr * ast_expr
             | Enot of ast_expr
             | EPlusPlus of ast_expr*prec
             | EMinusMinus of ast_expr*prec
             | EAssignEq of ast_expr*ast_expr 
             | EPlusEq of ast_expr*ast_expr 
             | EMinusEq of ast_expr*ast_expr 
             | EDivEq of ast_expr*ast_expr 
             | EDotEq of ast_expr*ast_expr 
             | EModEq of ast_expr*ast_expr 
             | ENew of typ*ast_expr 
             | EDel of ast_expr
             | ECast of typ*ast_expr 
             | EQuestT of ast_expr* ast_expr * ast_expr 
             | Eapp of string * ast_expr list
             | ECall of string * ast_expr list option
             | EArray of ast_expr * ast_expr 
     and prec = PRE
                | AFTER;;
let ast_tree : ast_program ref = ref [];
