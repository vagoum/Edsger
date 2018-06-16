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
             | ECall of string *ast_expr list option
             | EArray of ast_expr * ast_expr 
     and prec = PRE
                | AFTER
;;
let ast_tree : ast_program ref = ref [];;
(*
let rec print_program out t = match t with
        | [] -> (fprintf out "empty\n")
        | h :: [] -> print_declaration out h
        | h :: t ->
                print_declaration out h;
                print_newline ();
                print_newline ();
                print_program out t

and print_type_t out typ =   (match typ with
          | TYPE_none ->
      fprintf out "<undefined (probably wrong)>"
          | TYPE_int ->
      fprintf out " int "
        | TYPE_double ->
      fprintf out " double "
        | TYPE_char -> 
      fprintf out " char "
         | TYPE_bool ->
      fprintf out " boolean "
        | TYPE_pointer (ty) ->
      fprintf out "pointer ";
      print_type_t out ty
          | TYPE_array (et, sz) ->
      print_type_t out et;
      if sz > 0 then
        fprintf out " [%d] " sz
      else
        fprintf out " [] "
        | TYPE_byte -> fprintf out "byte"
        | TYPE_proc ->
      fprintf out " void ")
 
and print_declaration out t =( match t with
        | VarDecl (decs) ->
 fprintf out "Variable_dec( ";
 print_declarators out decs;
 fprintf out ")";
 force_newline();
        | FunDecl (t1) ->(
  fprintf out "Function_dec ";
  print_func_decl out t1;
  fprintf out ") ";
  force_newline ();)
        | FunDef (t1, dec, stm) ->(
  fprintf out "Function_def";
  print_func_decl out t1;
  force_newline ();
  fprintf out " {";
  force_newline ();
  open_hovbox 2;
  fprintf out "\t  val-fun declaration \n";
  print_program out dec; (* edw evala prin_program gt einai list *)
  force_newline ();
  fprintf out "\t stmts declaration \n";
  print_statements out stm;
  close_box ();
  force_newline ();
  fprintf out "} Function end"))
and print_func_decl out d = 
        match d.entry_info with 
                | ENTRY_function a ->
        print_type_t out a.function_result ;
        (*print_declarators out  a.function_paramlist;
        print_declarators out  a.function_redeflist;*)
        fprintf out "Function_declarator %s" d.entry_name

and print_declarators out t = (match t with
        | [] -> ()
        | [dec] -> print_declarator out dec;
        | dec :: rest ->
        print_declarator out dec;
        fprintf out ", ";
        print_declarators out rest)

and print_declarator out t =( 
        match t.entry_info with 
        | ENTRY_variable a ->
print_type_t out a.variable_type ;
fprintf out "Simple_declarator %s" t.entry_name
)


and print_statements out t = (match t with
        | [] -> ()
        | [stm] ->
  print_statement out stm
        | stm :: rest ->
  print_statement out stm;
force_newline ();
print_statements out rest)

and print_statement out t = match t with
        | SNewblock stms ->
  force_newline();	
  print_statements out stms;
        | SExpr ex ->
  ( match ex with
    | None -> ()
    | Some expr -> 
    (force_newline();
    print_expression out expr;)
  );
        | Sif (e,s1, s2) ->
          fprintf out "If_stmt (";
           print_expression out e;
        fprintf out ") then ";
        open_hovbox 2;
         print_statement out s1;
         close_box();
        force_newline ();
        ( match s2 with
        |None ->()
        | Some s2 ->(
                 fprintf out "else ";
                open_hovbox 2;
                print_statement out s2;
        close_box());)
        | Sfor (s1, e1, e2, e3, stmt) ->
  fprintf out "for (";
  ( match s1 with
        | None -> ()
        | Some s -> fprintf out "%s " s;
    );
  ( match e1 with
        | None -> ()
        | Some e -> print_expression out e;
  fprintf out "; ";
    );
  ( match e2 with
        | None -> ()
        | Some e -> print_expression out e;
  fprintf out "; ";
    );
  ( match e3 with
        | None -> ()
        | Some e -> print_expression out e;
    );
  fprintf out ")";
  force_newline ();
  open_hovbox 2;
  print_statement out stmt;
  force_newline ();
  close_box()
        | Sreturn exp ->
  ( match exp with
        | None -> fprintf out "return;\n";
        | Some e -> fprintf out "return ";
    print_expression out e;
  fprintf out ";\n";
    )

and print_expressions out t = match t with
        | [] -> ()
        | [exp] -> print_expression out exp;
        | exp :: rest -> print_expression out exp;
fprintf out ", ";
print_expressions out rest

and print_expression out t = match t with
        |  Eid n ->
fprintf out "Id %s" n;
        | Eint n -> fprintf out "Int %d" n
        | Ereal n -> fprintf out "Double %f" n
        | Echar n -> fprintf out "Char %c" n
        | Ebool n -> fprintf out "Bool %b" n
        | Estring n -> fprintf out "String %s" n
        | ECall (s, e) ->
   fprintf out ("Function_call (%s, ") s;
   (match e.ls with
        |None -> ()
        |Some e ->
   print_expressions out e;
   fprintf out (" );");)
        | EArray (t1, t2) ->  (* na dw giati einai expression * expression*)
   fprintf out "Array (";
   print_expression out t1;
   fprintf out "[";
   print_expression out t2;
   fprintf out "]";
   fprintf out ")"
        | EAmber ( e) ->
   fprintf out "Unary_op (&, ";
   print_expression out e;
   fprintf out ")"
        | EPointer ( e) ->
   fprintf out "Unary_op (*, ";
   print_expression out e;
   fprintf out ")"
        | EUnMinus ( e) ->
   fprintf out "Unary_op (-, ";
   print_expression out e;
   fprintf out ")"
        | EUnAdd ( e) ->
   fprintf out "Unary_op (+, ";
   print_expression out e;
   fprintf out ")"
        | Enot ( e) ->
   fprintf out "Unary_op (not, ";
   print_expression out e;
   fprintf out ")"
        | Eplus (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  "+";
   print_expression out t2;
   fprintf out ")"
        | Eminus (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  "-";
   print_expression out t2;
   fprintf out ")"
        | Ediv (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s " "\\";
   print_expression out t2;
   fprintf out ")"
        | Emult (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  "*";
   print_expression out t2;
   fprintf out ")"
        | Emod (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  "%";
   print_expression out t2;
   fprintf out ")"
        | Eand (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  "&";
   print_expression out t2;
   fprintf out ")"
        | Eor (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  "or";
   print_expression out t2;
   fprintf out ")"
        | Ecomma (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  ",";
   print_expression out t2;
   fprintf out ")"
        | Elt (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  "<";
   print_expression out t2;
   fprintf out ")"
        | Elte (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  "<=";
   print_expression out t2;
   fprintf out ")"
        | Egt (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  ">";
   print_expression out t2;
   fprintf out ")"
        | Egte (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  ">=";
   print_expression out t2;
   fprintf out ")"
        | Eeq (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  "==";
   print_expression out t2;
   fprintf out ")"
        | Eneq (t1,t2) ->
   fprintf out "Binary_op (";
   print_expression out t1;
   fprintf out " %s "  "!=";
   print_expression out t2;
   fprintf out ")"
        | EPlusPlus (s, t) ->
   fprintf out "Prefix_unary_as (++, " ;
   print_expression out s;
   fprintf out ")"
        | EMinusMinus (t, s) ->
   fprintf out "Postfix_unary_as(--,";
   print_expression out t;
   fprintf out ", )"
        | EAssignEq (t1, t2) ->
   fprintf out "Binary_as (";
   print_expression out t1;
   fprintf out " = ";
   print_expression out t2;
   fprintf out ")"
        | EPlusEq (t1, t2) ->
   fprintf out "Binary_as (";
   print_expression out t1;
   fprintf out " += ";
   print_expression out t2;
   fprintf out ")"
        | EMinusEq (t1, t2) ->
   fprintf out "Binary_as (";
   print_expression out t1;
   fprintf out " -= ";
   print_expression out t2;
   fprintf out ")"
        | EDivEq (t1, t2) ->
   fprintf out "Binary_as (";
   print_expression out t1;
   fprintf out " /= ";
   print_expression out t2;
   fprintf out ")"
        | EModEq (t1, t2) ->
   fprintf out "Binary_as (";
   print_expression out t1;
   fprintf out " mod= ";
   print_expression out t2;
   fprintf out ")"
        | EDotEq (t1, t2) ->
   fprintf out "Binary_as (";
   print_expression out t1;
   fprintf out " *= ";
   print_expression out t2;
   fprintf out ")"
        | ECast (ty, e)->
   fprintf out "Casting( ";
   print_type_t out ty;
   print_expression out e;
   fprintf out ")"
        |  EQuestT  (t1, t2,t3) ->
    fprintf out "Question(";
    print_expression out t1;
    fprintf out " ? ";
    print_expression out t2;
    fprintf out " : ";
    print_expression out t3;
    fprintf out ")"
        | ENew (y, ex) ->
   fprintf out "New( ";
   print_type_t  out  y;
     print_expression out ex;
   fprintf out ")"
        | EDel  (t) ->
   fprintf out "Delete( ";
   print_expression out t;
   fprintf out ")"

and pretty_print out t =(* match t with
          | None -> printf "Empty"
          | Some tree -> *)print_program out t

let print_teliko progr =
  force_newline ();
  printf  "*** Pretty Printing AST ***";
  force_newline ();
  printf "***************************";
  force_newline ();
  printf "%a" pretty_print  progr;
  force_newline ()*)
