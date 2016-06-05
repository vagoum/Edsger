open Ast
open Format
open List
open Option
open Error
open Symbol
open Identifier
open Types

exception Terminate of string

(*Lets make some symatics *)
let nested_loops = ref 0
let infun = ref 0
let has_return = ref false

let is_main() = try ignore (lookupEntry (id_make "main") LOOKUP_CURRENT_SCOPE true )with Not_found-> error "incorect program";;


let cast_allow x y = match (get_type x, get_type y) with 
| (TYPE_double ,TYPE_int), (TYPE_int,TYPE_double) -> true
| (y1,y2) ->if equalType y1 y2 then true else false;
| _ -> false;; (*cast*)


let get_entry_type enry = match enry.entry_info with
|ENTRY_variable x -> x.variable_type
|ENTRY_function x -> x.function_result
|ENTRY_parameter x -> x.parameter_type
|ENTRY_temporary x -> x.temporary_type
(*this is more like a type checker*)
let rec get_type expr = match expr with
        | Eid x ->  get_entry_type (lookupEntry (id_make x) LOOKUP_ALL_SCOPES true) (*add some warning message*)
        |Ebool _ -> TYPE_bool
        |Echar _ -> TYPE_char
        |Eint _ -> TYPE_int
        |Ereal _ -> TYPE_double
        |Estring x ->TYPE_array(TYPE_char,String.length x)
        |ENull -> TYPE_none
        |EAmber x -> get_type x
        |EPointer x | EUnAdd x | EUnMinus x |Enot x-> get_type x
        | Emult (x,y) | Ediv (x,y) |Eplus (x,y) | Eminus (x,y) -> (match (get_type x,get_type y) with | (TYPE_pointer x1,TYPE_int) -> TYPE_pointer x1
        | (TYPE_int,x1) -> x1
        | (x1,TYPE_int) ->x1
        | _ -> error "Type problem";TYPE_none (*I shall add something for char + char etc*)
        ) 
        | EAssignEq (x,y) | EPlusEq (x,y) |EMinusEq (x,y) | EDotEq (x,y) |EDivEq (x,y) -> (match (get_type x,get_type y) with 
        | (TYPE_pointer x1,TYPE_int) -> TYPE_pointer x1
        | (x,_) -> x (*need to check later*)
        | _ ->error "Type_problem";TYPE_none (*I shall add something for char + char etc*)
        ) 
        |Emod (x,y) |EModEq (x,y)-> (match (get_type x,get_type y) with 
                | (TYPE_int,TYPE_int) ->  TYPE_int 
                | _ -> Types.print_type (get_type x);Types.print_type (get_type y);error "Mod needs integer and integer" ; TYPE_none
        )
        | Elt (x,y) | Elte (x,y) | Egt (x,y) | Egte (x,y) | Eeq (x,y) | Eneq (x,y) -> (match (get_type x,get_type y) with 
        | (TYPE_int,TYPE_int) | (TYPE_int,TYPE_double) | (TYPE_double,TYPE_int) | (TYPE_double ,TYPE_double) | (TYPE_bool ,TYPE_bool)  -> TYPE_bool
        | (TYPE_array (x,_), TYPE_array (y,_) ) | (TYPE_pointer x,TYPE_pointer y)-> if equalType x y then TYPE_bool else TYPE_none  
        | _ ->error "sigkrisi xriazete arithmous";TYPE_none)
        | Eand (x,y) | Eor (x,y) -> (match (get_type x,get_type y) with
        | (TYPE_bool ,TYPE_bool) ->TYPE_bool 
        | _ -> print_type (get_type x); print_type (get_type y);error "type missimatch on boolean action" ; TYPE_none )
        |Ecomma (x,y) -> get_type y
        |EPlusPlus (x,_) | EMinusMinus (x,_) -> if (get_type x) = TYPE_int then TYPE_int else (error "++ -- needs integer"; TYPE_none) 
        | ECast (x,y) -> if cast_allow x (get_type y) then x else get_type y
        | EQuestT (x,y,z)-> if (get_type x = TYPE_bool) then (if equalType (get_type y) (get_type z) then get_type y else TYPE_none ) else (error "type error questionmark"; TYPE_none)
        | ENew (x,_) -> x
        | EDel _ -> TYPE_none
        | _ -> TYPE_none 


let rec check ast =
  match ast with
  | None      -> raise (Terminate "AST is empty")
  | Some tree -> check_program tree

and check_program defs =
  let _ = List.map (fun def -> check_fun_def def)  defs in
  ()

and check_fun_def def  =
  match def with
  | FunDef (entry, param,smth) -> 
                  has_return := false;
                  List.iter (fun x-> check_stmt entry x) smth;
                 ( if not ( (get_entry_type entry) = TYPE_none) then
                          (if !has_return then () else ignore (error "No return found on fuction")) else ());
                          ();
  | FunDecl e1 ->();
  | VarDecl e2 ->  ();

and check_stmt enrty tree=
  match tree with
  | SExpr (e) -> (may check_expr e);
  | Sif (expr, stmt, maybe_stmt)   ->  
                                      check_stmt enrty stmt;
                                      (match maybe_stmt with
                                       | None -> ()
                                      | Some else_stmt -> check_stmt enrty else_stmt );
  | Sreturn (Some expr) ->has_return:=true; 
  | Sreturn None -> has_return:=true;
  | _ -> ();
and check_expr  e=  ();

