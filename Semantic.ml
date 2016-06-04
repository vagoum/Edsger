open Ast
open Format
open List
open Error
open Symbol
open Types

exception Terminate of string

(*Lets make some symatics *)
let nested_loops = ref 0
let infun = ref 0
let has_return = ref false

let is_main() = try lookupEntry (make_id "main") LOOKUP_CURRENT_SCOPE true with Not_found-> error "incorect program";;





let get_entry_type enry = match enry.entry_info with
|ENTRY_variable x -> x.variable_type
|ENTRY_function x -> x.function_result
|ENTRY_parameter x -> x.parameter_type
|ENTRY_temporary x -> x.temporary_type
(*this is more like a type checker*)
let rec get_type expr = match expr with
        | Eid x ->  get_entry_type (lookupEntry (make_id x) LOOKUP_ALL_SCOPES true with Not_found -> error "Cant found indetifier")
        |Ebool _ -> TYPE_bool
        |Echar _ -> TYPE_char
        |Eint _ -> TYPE_int
        |Ereal _ -> TYPE_double
        |Estring x ->TYPE_array(TYPE_char,String.length x)
        |Enull -> TYPE_none
        |EAmber x -> get_type x
        |EPointer x | EUnAdd x | EUnMinus x |Enot -> get_type x
        | Emult (x,y) | Ediv (x,y) |Eplus (x,y) | Eminus (x,y) -> (match (get_type x,get_type y) with | (TYPE_Pointer x1,TYPE_int) -> TYPE_Pointer
        | (TYPE_int,x1) -> x1
        | (x1,TYPE_int) ->x1
        | _ -> error "Type problem";TYPE_none (*I shall add something for char + char etc*)
        ) 
        | EAssignEq (x,y) | EPlusEq (x,y) |EMinusEq (x,y) | EDotEq (x,y) |EDivEq (x,y) -> (match (get_type x,get_type y) with 
        | (TYPE_Pointer x1,TYPE_int) -> TYPE_Pointer
        | (x,_) -> x (*need to check later*)
        | _ ->error "Type_problem";TYPE_none (*I shall add something for char + char etc*)
        ) 
        |E_Mod (x,y) |EModEq-> (match (get_type x,get_type y) with 
                | (TYPE_int,TYPE_int) ->  TYPE_int 
                | _ -> error "Mod needs integer and integer" ; TYPE_none
        )
        | Elt (x,y) | Elte (x,y) | Egt (x,y) | Egte (x,y) | Eeq (x,y) | Eneq (x,y) -> (match (get_type x,get_type y) with 
        | (TYPE_int,TYPE_int) | (TYPE_int,TYPE_double) | (TYPE_double,TYPE_int) | (TYPE_double ,TYPE_double) -> TYPE_bool
        | _ ->error "sigkrisi xriazete boolean";TYPE_none)
        | Eand (x,y) | Eor (x,y) -> (match (get_type x,get_type y) with
        | (TYPE_bool ,TYPE_bool)) ->TYPE_bool 
        | _ -> error "type missimatch on boolean action" ; TYPE_none 
        |Ecomma (x,y) -> get_type y (*need fix*)
        |EplusPlus (x,_) | EMinusMinus (x,_) -> if (get_type x) = TYPE_int then TYPE_int else (error "++ -- needs integer"; TYPE_none) 
        | ECast (x,y) -> if cast_allow x (get_type y) then x else get_type y
        | EQuestT (x,y,z)-> if (get_type x = TYPE_bool) then (if equalType (get_type y) (get_type z) then get_type y else TYPE_none ) else (error "type error questionmark"; TYPE_none)
        | Enew (x,_) -> x
        | EDel -> TYPE_none
        | _ -> TYPE_none 


let rec check ast =
  match ast with
  | None      -> raise (Terminate "AST is empty")
  | Some tree -> check_program tree

and check_program defs =
  let _ = List.fold_left (fun funs def -> check_fun_def def funs) [] defs in
  ()

and check_fun_def def funs =
  match def with
  | FunDef (entry, param,smth) -> 
                  has_return := false;
                  check_stmt entry smth;
                  if get_entry_type entry != TYPE_none then
                          (if has_return then () else error "No return found on fuction") else ();
  | FunDecl e | VarDecl e ->  ();;

and check_stmt enrty tree=
  match tree with
  | SExpr (e) -> (may check_expr e);
  | Sif (expr, stmt, maybe_stmt)   -> (if get_type  expr = TYPE_bool then ()
                                        
  else error "If without a boolean type");
                                      check_stmt enrty stmt;
                                      (match maybe_stmt with
                                       | None -> ()
                                      | Some else_stmt -> check_stmt entry else_stmt );
  | Sreturn (Some expr) ->has_return:=true; if (get_entry_type enrty ) =( get_type expr)  then (); else error "Return type isnt comp with function"
  | Sreturn None -> has_return:=true; if get_entry_type entry != TYPE_none then "error doesnt return something" else ();

and check_expr  e=  ignore (get_type e);

