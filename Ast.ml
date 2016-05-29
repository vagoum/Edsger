open Format

type ast_program = ast_fun_def list

and ast_fun_def = Fundef of string * ast_param list * string list * ast_stmt list

and ast_param = Param of string*typ
              | ParamByRef of string*typ

and ast_stmt = Expr of ast_expr option
             | SNewblock of ast_stmt list
             | Sfor of string option *ast_expr list* ast_expr list* ast_expr list* ast_stmt
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
             | Elt of ast_expr * ast_expr
             | Elte of ast_expr * ast_expr
             | Egt of ast_expr * ast_expr
             | Egte of ast_expr * ast_expr
             | Eeq of ast_expr * ast_expr
             | Eneq of ast_expr * ast_expr
             | Enot of ast_expr
             | EPlusPlus of ast_expr*prec
             | EMinusMinus of ast_expr*prec
             | EPlusEq of ast_expr*ast_expr 
             | EMinusEq of ast_expr*ast_expr 
             | EDotEq of ast_expr*ast_expr 
             | EModEq of ast_expr*ast_expr 
             | ENew
             | EDel
             | Eapp of string * ast_expr list
     and prec = PRE
                | AFTER;;
let ast_tree = ref None

(* Pretty Prints the AST *)

let rec print_ast_program ppf ast =
  match ast with
  | []    -> 
    ()
  | h::[] -> 
    print_ast_fun_def ppf h
  | h::t  -> 
    print_ast_fun_def ppf h;
    print_newline ();
    print_newline ();
    print_ast_program ppf t

and print_ast_fun_def ppf ast =
  match ast with
  | Fundef (name, params, vars, stmts) ->
    fprintf ppf "%s (" name;
    print_ast_params ppf params;
    fprintf ppf ")";
    force_newline ();
    fprintf ppf "{";
    force_newline ();
    open_hovbox 2;
    fprintf ppf "  var ";
    print_vars ppf vars;
    force_newline ();
    print_ast_stmts ppf stmts;
    close_box ();
    force_newline ();
    fprintf ppf "}"

and print_vars ppf vars =
  match vars with
  | [] ->
    ()
  | [var] ->
    fprintf ppf "%s" var
  | var::rest ->
    fprintf ppf "%s" var;
    fprintf ppf ", ";
    print_vars ppf rest

and print_ast_params ppf params =
  match params with
  | [] ->
    ()
  | [param] ->
    print_ast_param ppf param
  | param::rest ->
    print_ast_param ppf param;
    fprintf ppf ", ";
    print_ast_params ppf rest

and print_ast_param ppf ast =
  match ast with
  | Param name ->
    fprintf ppf "%s" name
  | ParamByRef name ->
    fprintf ppf "byref %s" name

and print_ast_stmts ppf stmts =
  match stmts with
  | [] ->
    ()
  | [stmt] ->
    print_ast_stmt ppf stmt
  | stmt::rest ->
    print_ast_stmt ppf stmt;
    force_newline ();
    print_ast_stmts ppf rest

and print_ast_stmt ppf stmt =
  match stmt with
  | Sprint expr ->
    fprintf ppf "print ";
    print_ast_expr ppf expr
  | Slet (var, expr) ->
    fprintf ppf "let %s = " var;
    open_hovbox 2;
    force_newline ();
    print_ast_expr ppf expr;
    close_box ()
  | Sbegin stmts ->
    fprintf ppf "begin";
    open_hovbox 2;
    force_newline ();
    print_ast_stmts ppf stmts;
    close_box ();
    force_newline ();
    fprintf ppf "end"
  | Sfor (var, expr1, expr2, stmt) ->
    fprintf ppf "for %s = " var;
    print_ast_expr ppf expr1;
    fprintf ppf " to ";
    print_ast_expr ppf expr2;
    fprintf ppf " do";
    open_hovbox 2;
    force_newline ();
    print_ast_stmt ppf stmt;
    close_box ()
  | Swhile (expr, stmt) ->
    fprintf ppf "while ";
    print_ast_expr ppf expr;
    fprintf ppf " do";
    open_hovbox 2;
    force_newline ();
    print_ast_stmt ppf stmt;
    close_box ()
  | Sif (expr, stmt, maybe_stmt) ->
    fprintf ppf "if (";
    print_ast_expr ppf expr;
    fprintf ppf ") then ";
    print_ast_stmt ppf stmt;
    (match maybe_stmt with
     | None -> ()
     | Some else_stmt ->
      force_newline ();
      fprintf ppf "else ";
      print_ast_stmt ppf else_stmt;
      close_box ();
    )
  | Sreturn expr ->
    fprintf ppf "return ";
    print_ast_expr ppf expr

and print_ast_expr ppf ast =
  match ast with
  | Enum i ->
    fprintf ppf "%d" i
  | Eid name ->
    fprintf ppf "%s" name
  | Ebool b ->
    fprintf ppf "%b" b
  | Eplus (expr1, expr2) ->
    print_ast_expr ppf expr1;
    fprintf ppf " + ";
    print_ast_expr ppf expr2
  | Eminus (expr1, expr2) ->
    print_ast_expr ppf expr1;
    fprintf ppf " - ";
    print_ast_expr ppf expr2
  | Ediv (expr1, expr2) ->
    print_ast_expr ppf expr1;
    fprintf ppf " / ";
    print_ast_expr ppf expr2
  | Emult (expr1, expr2) ->
    print_ast_expr ppf expr1;
    fprintf ppf " * ";
    print_ast_expr ppf expr2
  | Eand (expr1, expr2) ->
    print_ast_expr ppf expr1;
    fprintf ppf " and ";
    print_ast_expr ppf expr2
  | Eor (expr1, expr2) ->
    print_ast_expr ppf expr1;
    fprintf ppf " or ";
    print_ast_expr ppf expr2
  | Elt (expr1, expr2) ->
    print_ast_expr ppf expr1;
    fprintf ppf " < ";
    print_ast_expr ppf expr2
  | Egt (expr1, expr2) ->
    print_ast_expr ppf expr1;
    fprintf ppf " > ";
    print_ast_expr ppf expr2
  | Eneq (expr1, expr2) ->
    print_ast_expr ppf expr1;
    fprintf ppf " <> ";
    print_ast_expr ppf expr2
  | Eeq (expr1, expr2) ->
    print_ast_expr ppf expr1;
    fprintf ppf " == ";
    print_ast_expr ppf expr2
  | Enot expr ->
    fprintf ppf "not ";
    print_ast_expr ppf expr
  | Eunary expr ->
    fprintf ppf "- ";
    print_ast_expr ppf expr
  | Eapp (name, exprs) ->
    fprintf ppf "%s (" name;
    print_ast_actual_params ppf exprs;
    fprintf ppf ")";

and print_ast_actual_params ppf exprs =
  match exprs with
  | [] ->
    ()
  | [expr] ->
    print_ast_expr ppf expr
  | expr::rest ->
    print_ast_expr ppf expr;
    fprintf ppf ", ";
    print_ast_actual_params ppf rest

and pretty_print ppf ast =
   match ast with
   | None ->
      eprintf "%s@." "AST is empty"
   | Some tree ->
      print_ast_program ppf tree

let print_ast ast_tree = 
  force_newline ();
  printf "*** Pretty Printing AST ***";
  force_newline ();
  printf "***************************";
  force_newline ();
  printf "%a" pretty_print ast_tree;
  force_newline ()
