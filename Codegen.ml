open Llvm
open Ast 
open Types 
open  Semantic 
open Symbol
open Format
open Str
exception Error of string

let context = global_context ()
let the_module = create_module context "My cmp "
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t =Hashtbl.create 10
let functions:(string, llvalue) Hashtbl.t =Hashtbl.create 10
let functions_params:(string, Ast.ast_expr list) Hashtbl.t =Hashtbl.create 10
let functions_types:(string, typ) Hashtbl.t =Hashtbl.create 10
let jump_inst_break:(string , llbasicblock) Hashtbl.t =Hashtbl.create 10
let jump_inst_cont:(string , llbasicblock) Hashtbl.t =Hashtbl.create 10
type binary_ops = Plus|Minus|Div|Mult|Mod|And|Or|Comma|Lt|Lte|Eq|Neq|Gt|Gte|Enq
let find_variable s= Hashtbl.find named_values s;;
let get_function s=(* print_string s;*) Hashtbl.find functions s;;
let get_function_type s= Hashtbl.find functions_types s;;
let get_jump_break s= Hashtbl.find jump_inst_break s;;
let get_jump_cont s= Hashtbl.find jump_inst_cont s;;
let cont_stack= Stack.create();;
let break_stack = Stack.create();;
let fun_bbs:(llbasicblock list ref)= ref [];;
let global_decls:(Ast.ast_decl list ref) =ref [];;
module SS = Set.Make(String)
let fun_names : string list ref = ref []
type environment = Global of (string list)| Nested of (string list * environment)
let env:environment ref = ref (Global ([]))
let rec update_env_without_hashtbl name env =
  match env with
  | Global (names) -> Global(name::names)
  | Nested (names,e) -> Nested (name::names,e)

and clear_env env_list = List.iter (Hashtbl.remove named_values) env_list
            
and get_env_of_called env args params =
  let argscnt = List.length args in
  let paramscnt = Array.length params in
  let cnt = paramscnt - argscnt in
  let rec walk env =
    let l = List.length (env_to_list env) in
    if (l=cnt) then env else
      match env with
      | Global([])-> Global([])
      | Global(h::t)-> walk (Global(t))
      | Nested ([],e) -> walk e
      | Nested ((h::t),e) -> walk (Nested (t,e))
  in walk env 
and print_env env2 =
        let rec walk env = match env with
        | Global([]) -> ()
        | Global(h::t) -> print_string(h);print_string("\n"); walk(Global(t))
        | Nested ([],e)->print_string("Uper_level");print_string("\n"); walk(e)       
        | Nested ((h::t),e)->print_string(h);print_string("\n"); walk(Nested (t,e)) 
        in walk env2  
and count_env env2 =
        let rec walk env = match env with
        | Global([]) -> 0
        | Global(h::t) -> 1+ walk(Global(t))
        | Nested ([],e)-> 0       
        | Nested ((h::t),e)->1+ walk(Nested (t,e)) 
        in walk env2  
and update_env name env=
  let name_env = name in
  let name_to_env = match env with Global(_) -> name | _ -> name_env in
  match env with
  | Global (names) -> Global(name_to_env::names)
  | Nested (names,e) -> Nested (name_to_env::names,e)

                               
and update_env_with_params params en = List.iter (fun x-> env:=update_env x en) (get_param_names params)
and env_to_set env =
  let rec walk env acc =
    match env with
    | Global (names) -> let set_to_add = SS.of_list names in SS.union set_to_add acc 
    | Nested (names,env) -> let set_to_add = SS.of_list names in
                            let new_acc = SS.union set_to_add acc in
                            walk env new_acc
  in walk env SS.empty

          
and difference_with_env env params =
  let param_set = SS.of_list (get_param_names params) in
  let env_set = env_to_set env in
  SS.elements (SS.diff env_set param_set )

and get_env_params_types env =
  let find_type name =
      let v =
        try Hashtbl.find named_values name
        with Not_found -> print_string(name); raise Not_found
      in (type_of v)
  in List.map find_type env

and env_to_list env = SS.elements (env_to_set env)
              
            
and remove_env env =
  match env with
  | Global (names) -> Global([])
  | Nested (names,env) -> env

and env_to_id_list env =
  let env_list = env_to_list env in
  List.map (fun x -> Eid(x)) env_list 

and get_param_names params =
  let get_param_name p =p.entry_name
  in List.map get_param_name params
              

(*End of environment interface*)
                                                     

let rec find_function fun_name fun_name_list =
  match fun_name_list with
  | [] -> ( match lookup_function fun_name the_module with
            | Some calle -> calle
          (*  | None -> raise (Type_error "unknown function refernced"))*))
            
  | x::rest -> ( let to_found = String.concat "_" (fun_name::fun_name_list) in
                 match lookup_function to_found the_module with
                 |Some callee -> callee
                 | None -> find_function fun_name (List.tl fun_name_list))

let last_id = ref (-1)
let fresh () = incr last_id; !last_id

let gentmp s =
  let id = fresh () in
  Printf.sprintf "__%s_%i" s id


let assign_1 = ref false;;       
let rec ltype_of_type = function
        | TYPE_int ->  i32_type context 
        | TYPE_bool -> i1_type context
        | TYPE_char -> i8_type context 
        | TYPE_double ->x86fp80_type context (* double_type context *)
        | TYPE_void -> void_type context
        | TYPE_pointer t -> pointer_type (ltype_of_type t)
        | TYPE_array (a,b) ->(*pointer_type (ltype_of_type a) *)array_type (ltype_of_type a) b
        | TYPE_none -> ltype_of_type TYPE_void
        | TYPE_proc -> ltype_of_type TYPE_void;;
let rec ltype_of_array =function
        |TYPE_array (a,b)-> pointer_type (ltype_of_type a);;
let rec need_def = function
        | Eid _ -> true
        | EArray _ ->true
        |(* Eplus (e1,_) | Ediv (e1,_) | Eminus (e1,_) | Emod (e1,_) | Emod (e1,_) | Emult (e1,_) | Eand (e1,_) | Eor (e1,_) | *)(*| EUnAdd e1 |EUnMinus e1 *) EPlusPlus (e1,_) | EMinusMinus (e1,_)  -> false
        |  _->false;;
let default_val_type smth = match smth with 
        | TYPE_int ->  const_int (ltype_of_type smth) 0
        | TYPE_bool -> const_int (ltype_of_type smth) 0
        | TYPE_char -> const_int (ltype_of_type smth) 0
        | TYPE_double -> const_float (ltype_of_type smth) 0.0
        | TYPE_void -> const_int (ltype_of_type smth) 0;;


module M = Map.Make (String)

let rec codegen_stmt stmt builder= 
        match stmt with 
        SExpr (Some a) ->  codegen_expr a builder
        |SNewblock a ->List.hd @@ List.map (fun x-> codegen_stmt x builder) a
        |Sfor (a,b,c,d,e) -> codegen_for_loop a b c d e builder
        |Sif (a,b,c) -> ( let lval =  myderef a builder in 
        let cond_val = build_icmp Icmp.Ne lval (const_int (i1_type context) 0) "ifcond" builder in
        let start_bb = insertion_block builder in
        let the_function =block_parent start_bb in
        let then_bb =append_block context "then" the_function in
        position_at_end then_bb builder;
        let then_val =codegen_stmt b builder in
        let new_then_bb =insertion_block builder in
        let else_bb = append_block context "else" the_function in
        position_at_end else_bb builder;
        let else_val = if Option.is_some c then codegen_stmt (Option.get c) builder else codegen_expr (ENull) builder in
        let new_else_bb =insertion_block builder in
        let merge_bb= append_block context "ifcond" the_function in
        position_at_end merge_bb builder;
        let else_bb_v=value_of_block new_else_bb in
        position_at_end start_bb builder;

        ignore(build_cond_br cond_val then_bb else_bb builder);
        position_at_end new_then_bb builder; ignore(build_br merge_bb builder); 
        position_at_end new_else_bb builder; ignore(build_br merge_bb builder);
        position_at_end merge_bb builder ;
                 else_bb_v)
        |Sreturn (Some a) -> build_ret (myderef a builder) builder
        |Sreturn _ -> build_ret_void builder 
        |SBreak a -> codegen_break a builder
        |SCont a -> codegen_cont a builder
        |_ -> codegen_expr (ENull) builder
and  codegen_for_loop a b c d e  builder= 
        let the_function = block_parent @@ insertion_block builder
        in let _ = if Option.is_some b then  (fun x-> codegen_expr x builder) (Option.get b) else codegen_expr (ENull) builder in
        let loop_bb =append_block context "loop" the_function in
        let inc_bb =append_block context "inc" the_function in
        let cond_bb =append_block context "cond" the_function in
        let after_bb =append_block context "after" the_function in
        (*need to insert for break continue*)
        let _ =  Stack.push inc_bb cont_stack ; Stack.push after_bb break_stack ;
        if Option.is_some a then (Hashtbl.add jump_inst_cont (Option.get a) inc_bb ; Hashtbl.add jump_inst_break (Option.get a) after_bb; )else () in
        let _ = Hashtbl.add jump_inst_break "tmp" after_bb in
        let _ = build_br cond_bb builder in
        let _ = position_at_end loop_bb builder in
        let _ = codegen_stmt e builder in
        let bb = insertion_block builder in
        move_block_after bb inc_bb ;
        move_block_after inc_bb  cond_bb;
        move_block_after cond_bb after_bb ;
        ignore(build_br inc_bb builder);
        position_at_end inc_bb builder;
        let _ =Option.map ( (fun x-> codegen_expr x builder)) d in
        ignore(build_br cond_bb builder);
        position_at_end cond_bb builder;
        (*let cond_val = Option.map ((fun x-> codegen_expr x builder) ) c in let _=*)
        let cond_val = Option.map ((fun x-> myderef x builder) ) c in let _=
        if Option.is_some cond_val then build_cond_br (Option.get cond_val) loop_bb after_bb builder else build_cond_br (codegen_expr (Ebool true) builder) loop_bb after_bb builder in
        position_at_end after_bb builder;
        ignore(Stack.pop cont_stack) ;
        ignore(Stack.pop break_stack);
        const_null (ltype_of_type TYPE_double) ;
and  codegen_break a builder=
        if Option.is_some a then build_br (get_jump_break (Option.get a)) builder else 
                build_br (Stack.top break_stack ) builder
and  codegen_cont a builder=
        if Option.is_some a then build_br (get_jump_cont (Option.get a)) builder else 
                build_br (Stack.top cont_stack ) builder
and get_indetifier s builder= match s with
        Eid(a) -> find_variable a
        |_ -> codegen_expr s builder
and  codegen_expr expr builder= 
       match expr with
        |Eint a -> const_int (ltype_of_type TYPE_int) a
        |Ereal a -> const_float (ltype_of_type TYPE_double) a
        |Echar a ->const_int (ltype_of_type TYPE_char) (Char.code a)
        |Eid a -> codegen_id a false builder
        |Estring a->build_global_stringptr a "tmp" builder
        |Ebool a -> const_int (ltype_of_type TYPE_bool) (if a then 1 else 0)
        |ENull ->  build_add (default_val_type TYPE_int) (default_val_type TYPE_int) "tmp" builder
        |EAmber a ->  (get_indetifier a builder) (* to see it again*)
        (*|EPointer a-> let tmp =   (get_indetifier a builder) (*to see*) in let load_ = build_load tmp "tmp" builder in let pointerr=build_gep load_ [|(default_val_type TYPE_int)|] "tmp" builder in build_load pointerr "dereference" builder 
      *)|EPointer a-> let tmp =   (myderef a builder) (*to see*) in let load_ = build_load tmp "tmp" builder in load_
        |EUnAdd a-> codegen_expr (EUnMinus (EUnMinus a)) builder
        |EUnMinus a -> let lval = myderef a builder in let type_is = type_of lval
                                in let type_m m= if m = (ltype_of_type TYPE_int)  then
                                 build_neg lval "int_unoptmp" builder
                                else build_fneg lval "flt_unotmp" builder
                                in type_m type_is
        |Eplus (a,b) -> codegen_binary a b Plus builder
        |Eminus (a,b) -> codegen_binary a b Minus builder
        |Ediv (a,b) ->codegen_binary a b Div builder
        |Emult (a,b) -> codegen_binary a b Mult builder 
        |Emod (a,b) -> codegen_binary a b Mod builder 
        |Eand (a,b) -> codegen_binary a b And builder 
        |Eor (a,b) -> codegen_binary a b Or builder
        |Ecomma (a,b) -> codegen_binary a b Comma builder 
        |Elt (a,b) -> codegen_binary a b Lt builder 
        |Elte (a,b) -> codegen_binary a b Lte builder 
        |Egt (a,b) -> codegen_binary a b Gt builder 
        |Egte (a,b) -> codegen_binary a b Gte builder 
        |Eeq (a,b) -> codegen_binary a b Eq builder 
        |Eneq (a,b) -> codegen_binary a b Enq builder 
        |Enot a -> let lval = codegen_expr a builder in build_not lval "nottmp" builder 
        |EPlusPlus (a,b)-> let newval =codegen_assign a (Eplus(a,Eint (1))) builder in if b=PRE then (codegen_expr (Eplus(a,Eint (0))) builder) else codegen_expr (Eminus(a,Eint (1))) builder
        |EMinusMinus (a,b)->  let newval  =codegen_assign a (Eminus (a,Eint(1))) builder in if b=PRE then newval else codegen_expr (Eplus(a,Eint(1))) builder
        |EAssignEq (a,b)-> codegen_assign a b builder 
        |EPlusEq (a,b) -> codegen_assign a (Eplus(a,b)) builder
        |EMinusEq (a,b) ->  codegen_assign a (Eminus (a,b)) builder
        |EDivEq (a,b) -> codegen_assign a (Ediv(a,b)) builder
        |EDotEq (a,b) ->  codegen_assign a (Emult(a,b)) builder
        |EModEq (a,b) ->  codegen_assign a (Emod (a,b)) builder
        |ENew (a,b) -> codegen_array_create a b builder
        |EDel a -> build_free (build_load ( (get_indetifier a builder)) "tmp" builder) builder
        |ECast (a,b)-> codegen_cast a b builder
        |ECall (a,b) -> codegen_fuction_call a b builder
        |EArray (a,b) -> codegen_array_access a b builder
        |EQuestT (a,b,c) -> codegen_quest a b c builder

and codegen_quest a b c builder = 
 let lval =  myderef a builder in 
        let cond_val = build_icmp Icmp.Ne lval (const_int (i1_type context) 0) "ifcond" builder in
        let start_bb = insertion_block builder in
        let the_function =block_parent start_bb in
        let then_bb =append_block context "then" the_function in
        position_at_end then_bb builder;
        let then_val =myderef b builder in
        let new_then_bb =insertion_block builder in
        let else_bb = append_block context "else" the_function in
        position_at_end else_bb builder;
        let else_val =  myderef c builder in
        let new_else_bb =insertion_block builder in
        let merge_bb= append_block context "ifcond" the_function in
        position_at_end merge_bb builder;
        (*let else_bb_v=value_of_block new_else_bb in
        position_at_end start_bb builder;

        ignore(build_cond_br cond_val then_bb else_bb builder);
        position_at_end new_then_bb builder; ignore(build_br merge_bb builder); 
        position_at_end new_else_bb builder; ignore(build_br merge_bb builder);
        position_at_end merge_bb builder ;
                 else_bb_v)*)
	let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
	let phi = build_phi incoming "iftmp" builder in	
	position_at_end start_bb builder;
	ignore (build_cond_br cond_val then_bb else_bb builder);
	position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
	position_at_end new_else_bb builder; ignore (build_br merge_bb builder);
	position_at_end merge_bb builder;
	phi
and codegen_cast a b builder =
        let e = myderef b builder in
        let t = type_of e in
        let ctype = ltype_of_type a in
        let typen = string_of_lltype t in
        if (contains typen "x86_fp80") then build_fptosi e ctype "castftp" builder 
        else
                if (contains typen "i32") then match a with
                |TYPE_double -> build_sitofp e ctype "ok" builder
                | _ -> build_trunc_or_bitcast e ctype "ok2" builder
              else
                if (contains typen "i16") then match  a with
                |TYPE_double -> build_sitofp e ctype "ftops" builder 
                | _ -> build_trunc_or_bitcast e ctype "ftopstr" builder
         else
                 if (contains typen "i8") then match a with
                 |TYPE_int -> build_sext_or_bitcast e ctype "okk" builder
                 |TYPE_double ->build_sitofp e ctype "doublee" builder
                 |_->build_trunc_or_bitcast e ctype "pf" builder
                 else
                         match a with 
                         |TYPE_double -> build_sitofp e ctype "doubleagain" builder
                         | _ ->build_zext_or_bitcast e ctype "last" builder
and  contains s1 s2 =
 	 let re = Str.regexp_string s2
  	in
  	try ignore (Str.search_forward re s1 0); true
 	 with Not_found -> false
        (*if (t = (ltype_of_type TYPE_int)) then
                if (a = TYPE_double) then build_sitofp e (ltype_of_type a) "cast" builder 
                        else if((a= TYPE_bool) || (a= TYPE_char)) then build_trunc e (ltype_of_type a) "cast" builder else(build_sext_or_bitcast e (ltype_of_type a) "case" builder)  else
                if (t=(ltype_of_type TYPE_double))  then  if (a= TYPE_int ) then  build_fptosi e (ltype_of_type a) "cast" builder else    
                    (  build_trunc_or_bitcast e (ltype_of_type a) "cast" builder) else   build_sext_or_bitcast e (ltype_of_type a) "cast" builder (*else build_zext_or_bitcast e (ltype_of_type a) "cast" builder*)
        *)

and codegen_array_create e1 e2 builder =
        let size_t = myderef e2 builder in
        let t = ltype_of_type e1 in
        let arra = build_array_malloc  t size_t "tmp" builder in
         build_pointercast arra (pointer_type t) "tmp" builder 

and myderef e2 builder =
        let y = codegen_expr e2 builder in
        let y = if(need_def e2) then build_load y "tmp" builder else y in y
and  codegen_assign e1 e2 builder=  
        let lval = getAdreess e1 builder in
        let y = myderef e2 builder in 
        let _ =build_store y lval builder in
        y
and  codegen_fuction_call fuction  params2 builder =
	let callee = find_function fuction (!fun_names) in
        let params = params callee in
	let env_of_called = get_env_of_called !env (if Option.is_some params2 then (Option.get params2)else []) params in
        let env_args = env_to_id_list (env_of_called) in
        let env_args = try  Hashtbl.find functions_params fuction 
        with Not_found -> [] 
        in
      (*  let _ = print_string fuction in
        let _ = print_string ("\n") in
        let _ =List.iter (fun x-> match x with | Eid(a)-> print_string (a);print_string("\n")) env_args in*)
        let params= if Option.is_some params2 then ((Option.get params2)@env_args) else env_args 
        in
        let parametres =List.map type_of (Array.to_list (Llvm.params  ( callee))) in
        let actual_params = List.mapi (fun i ->fun x-> let y = codegen_expr x builder in if((need_def x) && (not ((type_of y) = (List.nth parametres  i ) ))) then build_load y "tmp" builder else y ) params in
        let actual_params = List.map2 (fun x-> fun y-> build_bitcast x (y) "cast" builder) actual_params parametres in
        if((String.sub (string_of_lltype (type_of (callee))) 0 5 ) = "void ") then
         (*let _ = print_string(string_of_lltype (return_type (type_of(get_function fuction )) )); print_string("\n") in*)  build_call (callee) (Array.of_list actual_params) "" builder
        else build_call (callee) (Array.of_list actual_params) "t" builder


and  codegen_array_access e1 e2 builder = 
        let lvaluee = (get_indetifier  e1 builder)  in 
        let rval =[| myderef e2 builder|] in (*  (codegen_expr e2 builder) in
        let rval = (if(need_def e2) then [|build_load (rval) "tmp" builder|] else [|rval|] in*)
       (* let deref =build_gep (
        match (*get_type e1*) (type_of lvaluee)  with 
       (* TYPE_pointer _ -> build_load lvaluee "tmp" builder*)
        | _ -> lvaluee) rval "array" builder (*need fix*)
        in build_load deref "array" builder*)
       build_gep (build_load lvaluee "tmp" builder) rval "accs" builder
and  codegen_local name t expr builder =
       (env := update_env name (!env));
                      
        let ltype = ltype_of_type t in
        (*let malloc = build_malloc ltype name builder in*)
        match t with
        |TYPE_array(a,b) -> let malloc = build_alloca (ltype_of_array t) name builder in
        let _ = Hashtbl.add named_values name malloc in  
        let arr = build_array_malloc ltype (codegen_expr (Eint b) builder) "mallocttmp" builder in let arr = build_bitcast arr (ltype_of_array t) "tmp" builder  in let _ = build_store arr malloc builder in malloc
        | _ ->(
        let malloc = build_alloca ltype name builder in
        let _ = Hashtbl.add named_values name malloc in        
        match expr with
               ENull -> malloc
        | _ -> build_store (codegen_expr expr builder) malloc builder)
and  codegen_binary e1 e2 expr  builder= 
        let e1n = codegen_expr e1 builder in
        let e2n = codegen_expr e2  builder in
        let e1n = if( (need_def e1)) then build_load e1n "tmp" builder else e1n in
        let e2n = if(need_def e2) then build_load e2n "tmp" builder else e2n in
        let int_fun expr = match expr with
                Plus -> build_add e1n e2n "addtmp" builder 
                |Minus ->build_sub e1n e2n "subtmp" builder
                |Div ->build_sdiv e1n e2n "divtmp" builder
                |Mult ->build_mul e1n e2n "multmp" builder
                |Mod -> build_srem e1n e2n "sremtmp" builder
                |And -> build_and e1n e2n "andtmp" builder
                |Or -> build_or e1n e2n "ortmp" builder
                |Lt ->build_icmp Icmp.Slt e1n e2n "lttmp" builder
                |Lte ->build_icmp Icmp.Sle e1n e2n "ltetmp" builder
                |Gt -> build_icmp Icmp.Sgt e1n e2n "gttmp" builder
                |Gte -> build_icmp Icmp.Sge e1n e2n "gtetmp" builder
                |Eq -> build_icmp Icmp.Eq e1n e2n "equaltmp" builder
                |Enq -> build_icmp Icmp.Ne e1n e2n "equaltmp" builder
                |Comma -> e2n
        in
        let float_fun expr =
                let e2nk = build_sitofp e2n (ltype_of_type TYPE_double) "cast" builder in
                let e1n = build_sitofp e1n (ltype_of_type TYPE_double) "cast" builder in
                match expr with
                Plus -> build_fadd e1n e2nk "addtmp" builder 
                |Minus ->build_fsub e1n e2nk "subtmp" builder
                |Div ->build_fdiv e1n e2nk "divtmp" builder
                |Mult ->build_fmul e1n e2nk "multmp" builder
                |Mod -> build_frem e1n e2nk "sremtmp" builder
                |Lt ->build_fcmp Fcmp.Ult e1n e2nk "lttmp" builder
                |Lte ->build_fcmp Fcmp.Ole e1n e2nk "ltetmp" builder
                |Gt -> build_fcmp Fcmp.Ogt e1n e2nk "gttmp" builder
                |Gte -> build_fcmp Fcmp.Oge e1n e2nk "gtetmp" builder
                |Eq -> build_fcmp Fcmp.Oeq e1n e2nk "equaltmp" builder
                |Enq -> build_fcmp Fcmp.One e1n e2nk "equaltmp" builder
                |Comma ->e2n
        in
        let pointer_fun e1 e2  expr = match expr with
        Plus -> codegen_array_access e1 e2 builder 
                | Minus -> codegen_array_access e1 (EUnMinus(e2)) builder in
        if (contains (string_of_lltype (type_of e1n)) "*") then pointer_fun e1 e2 expr  else 
        (*let _ =print_string(string_of_lltype (type_of e1n)) in*)
            if (( (type_of e1n) <> (ltype_of_type TYPE_double ) ) && ((type_of e2n) <> (ltype_of_type TYPE_double))) then int_fun expr else float_fun expr 
    and codegen_id id deref builder =match deref with
        |true ->let v=  try Hashtbl.find named_values id with |Not_found -> raise (Error "uknown variable name") in build_load v id builder
        |false ->let v=  try Hashtbl.find named_values id with |Not_found -> raise (Error "uknown variable name") in v
   (* and codegen_arg_alloca func args =
            Array.iteri (fun i ai-> let name =args.(i) in 
            let alloca = create_entry_block_alloca:*)
    and temp_var name =
	 let inf_p = {
            parameter_type = TYPE_none;
            parameter_offset = 0;
            parameter_mode = PASS_BY_REFERENCE
          } in     
	let e = {
      		entry_id = Identifier.id_make (name);
      		entry_name = name;
     		entry_scope = !currentScope;
      		entry_info = (ENTRY_parameter inf_p)
    } in  
	e

    and codegen_func func =
        let name = String.concat "_" (func.entry_name::!fun_names)  in
        if (Hashtbl.mem functions name) then Hashtbl.find functions name else (
        let parametres = List.map (fun x-> let y = ltype_of_type (get_parameter_f x.entry_info).parameter_type
        in 
        match ((get_parameter_f x.entry_info).parameter_mode ) with
        | PASS_BY_REFERENCE -> pointer_type y
        | _ -> y
        ) ((get_fuction_f func.entry_info).function_paramlist) in
        let fuction_type = function_type (ltype_of_type (get_fuction_f func.entry_info).function_result) (Array.of_list parametres) in
        let b=  declare_function name fuction_type the_module in
        let _ = Hashtbl.add functions name b in b)
        and param_type x = (fun y-> let z = ltype_of_type (get_parameter_f y.entry_info).parameter_type in match ((get_parameter_f y.entry_info).parameter_mode ) with
        | PASS_BY_REFERENCE -> pointer_type z
        | _ -> z) x

        and make_function func1 = match func1 with FunDef (func,b,c) -> 
        let name = func.entry_name  in
	let fn_name = String.concat "_" (name::!fun_names) in
	let parameters = (get_fuction_f func.entry_info).function_paramlist in
	let parameters_old =  (get_fuction_f func.entry_info).function_paramlist in
     	env:= Nested([],!env);
     	let env_params = difference_with_env !env parameters in 
     	update_env_with_params parameters !env; (*Should create side effect*)
     (* let _ = print_env_pars !env in *)
        let _ = Hashtbl.add functions_params name (List.map (fun x-> Eid(x)) env_params) in
     	let env_params_types =get_env_params_types env_params  in
     (* let _ = print_hashtbl named_values in *)
        let llenv = Array.of_list env_params_types in
     	let llpars = Array.of_list (List.map param_type parameters) in
     	let llpars = if(name = "main") then llpars else  Array.append llpars llenv in
     	let env_params_to_passed = List.map (temp_var) env_params in
     	let parameters = if(name = "main") then parameters else parameters@env_params_to_passed in
	fun_names := name :: !fun_names ;
        let fun_typ= ltype_of_type (get_fuction_f func.entry_info).function_result in
        let ft =function_type fun_typ llpars in
        let f = (if (Option.is_some (lookup_function fn_name the_module) ) then let v=( Option.get (lookup_function fn_name the_module)) in (delete_function v; declare_function fn_name ft the_module  ) else ( declare_function fn_name ft the_module (*codegen_func func*)) )in
        (*let builder = builder_at_end context (entry_block f) in*)
        let bbs =append_block context "smth" f in
        fun_bbs := bbs :: !fun_bbs;
        position_at_end bbs builder ;
        let params2 = Array.iteri ( fun i el -> let n = List.nth parameters i in let (n,(typea,by_ref)) = (n.entry_name,match n.entry_info with 
        |ENTRY_parameter asd-> (asd.parameter_type,asd.parameter_mode)) in 
        (match by_ref with
        | PASS_BY_REFERENCE -> set_value_name n el;ignore(Hashtbl.add named_values n el)
        | _ ->
                        (set_value_name n el;let g = (build_alloca (ltype_of_type typea)  n builder) in (*let el = build_pointercast el (ltype_of_type typea) "cast" builder in *)  ignore(build_store el g builder) ; ignore(Hashtbl.add named_values n g))) ) (params f) 
        in
        (*let _ = if (name="main") then List.iter (fun x-> ignore(codegen_decl x builder)) !global_decls else ()  in*)
        (*check*) let _ =List.map (fun x-> codegen_decl  x builder) (sort_d b) in
        let bb =List.hd !fun_bbs in
        position_at_end bb builder;
        let _  = List.map (fun x->codegen_stmt x builder) c in
        fun_bbs := List.tl !fun_bbs;
        let next_bb =try List.hd !fun_bbs 
                     with Failure ("hd") -> bb in
        let last= match block_end f with After (block) -> block in
        let return= ltype_of_type (get_fuction_f func.entry_info).function_result (*return_type (type_of f)*) in
        let _ =(match (instr_end last) with
                |After(ins)-> if ((instr_opcode ins) = Opcode.Ret) then ()
        else 
                if (return = (ltype_of_type TYPE_void) )  then ignore (build_ret_void builder) else 
                          ignore(build_ret (default_val_type TYPE_int) builder)
         |At_start(_) ->
                         if (return= (ltype_of_type TYPE_void) )  then( ignore (build_ret_void builder) )else 
                                  ignore(build_ret (default_val_type TYPE_int) builder) ;) in
        let _ =ignore(  delete_from_hash b); in
		      fun_names := List.tl !fun_names;
                      let _ = Array.iteri (fun i a -> 
                        let n = (List.nth parameters i) in
                        if(i>=(List.length parameters_old)) then () else(
                                let (n,(typea,by_ref))= (n.entry_name, match n.entry_info with
                                | ENTRY_parameter asd -> (asd.parameter_type,asd.parameter_mode)) in
                               match by_ref with
                               |PASS_BY_REFERENCE ->Hashtbl.remove named_values n
                                | _ -> Hashtbl.remove named_values n)) (params f) in
 		      clear_env env_params;
                      position_at_end next_bb builder;
                      ignore(env := remove_env !env); (* Go higher in env*)
                      (*ignore(  delete_from_hash_par func);*)
	

  and delete_from_hash b = List.iter (fun x-> 
                      match x with
                      | VarDecl(y) ->(List.iter (fun a->ignore( Hashtbl.remove named_values a.entry_name) ; ignore( if (Hashtbl.mem named_values a.entry_name) then set_value_name a.entry_name (Hashtbl.find named_values a.entry_name) else ()) )  y )
                        |_ -> () ) b
  and delete_from_hash_par b =  (fun y-> 
                      (List.iter (fun a->ignore( Hashtbl.remove named_values a.entry_name) ; ignore( if (Hashtbl.mem named_values a.entry_name) then set_value_name a.entry_name (Hashtbl.find named_values a.entry_name) else ()) )  y )
                        ) (get_fuction_f b.entry_info).function_paramlist 


  and codegen_decl decl builder = match decl with 
        VarDecl (a) -> List.map (fun x-> codegen_local x.entry_name ( (get_variable_f x.entry_info).variable_type) (ENull)  builder) a
       |FunDecl (a) -> [codegen_func a]
       |FunDef (a,b,c) -> let _ =make_function (FunDef (a,b,c) ) in []
and findinHash = find_variable 
and getAdreess expr builder =  match expr with 
 Eid(x) ->  findinHash x
 |EAmber (x)->  let y =  (get_indetifier x builder) in
        let dereference = build_struct_gep y 0 "tmp" builder in build_load dereference "tmp" builder
 |EPointer (x)-> let y =  (get_indetifier x builder) in let load_ = build_load y "temp" builder in load_
 |EArray(x,y) -> let index = codegen_expr y builder in let index =  if(need_def y) then build_load index "tmp" builder else index in
         let tmp_val =  build_load (get_indetifier x builder) "tmp" builder in
 let dereference = build_gep tmp_val  [|(*default_val_type TYPE_int;*)index|] "arrayval" builder in dereference 


 and codegen_global a = 
         let name = a.entry_name in
         let intitial = match (get_variable_f a.entry_info).variable_type with
         |TYPE_int -> const_int (ltype_of_type TYPE_int) 0 
         | TYPE_double ->const_float (ltype_of_type TYPE_double) 0.0
         | TYPE_bool -> const_int (ltype_of_type TYPE_bool ) 0 
         |TYPE_char -> const_int (ltype_of_type TYPE_char) 0
         |TYPE_void ->const_int (ltype_of_type TYPE_void) 0
         |TYPE_pointer a->const_pointer_null (ltype_of_type (TYPE_pointer a))
         | TYPE_array (a,b) ->const_array (ltype_of_type a) (Array.make b (default_val_type (a))) 
 in 
 Hashtbl.add named_values name (define_global a.entry_name intitial the_module)
 and cs_sort type1 type2 = match type1 with
        |VarDecl _ -> ( match type2 with 
                        |VarDecl _-> 0
                        | _ -> -1)
        | _ -> (match type2 with
                |VarDecl _ -> 1
                |_-> 0)
        and sort_d decls = let j=decls_a (List.stable_sort cs_sort decls)in
        (*let _ = ssf j in*)
        j
        and ssf a = List.iter (print_nameso) a
        and decls_a ls = 
                let (ls1,ls2)= List.partition (fun x-> match x with 
                |VarDecl _ -> true
                |_ -> false) ls in
                let (def,decs)= List.partition (fun x -> match x with
                |FunDef _ -> true
                | _ -> false) ls2 in
                let deds = List.map (fun x-> match x with 
                |FunDecl(a)  ->  List.find (sort_find_fun a.entry_name) def  ) decs in
                let last = List.filter (fun x-> match x with 
                | FunDef(a,b,c) ->not (List.exists (sort_find_fun a.entry_name) deds)) def in
              ls1@deds@last
        and sort_find_fun name el = (match el with 
         |FunDef(a,b,c) ->let name2 = a.entry_name  in (name=name2)
         | _ -> false)
 and print_nameso l = match l with
 |VarDecl a -> List.iter ( fun x-> print_string (x.entry_name)) a
 |FunDef (a,b,c)-> print_string (a.entry_name)
 |FunDecl(a) -> print_string (a.entry_name)

      
        
 let codegen_lib () = 
                let _ = Hashtbl.add functions ("writeString")  (declare_function "writeString" (function_type (ltype_of_type TYPE_void) [|ltype_of_type(TYPE_pointer TYPE_char)|]) the_module ) in 
                let _ = Hashtbl.add functions ("writeInteger")  (declare_function "writeInteger" (function_type (ltype_of_type TYPE_void) [|ltype_of_type(TYPE_int)|]) the_module ) in 
                let _ = Hashtbl.add functions ("writeBoolean")  (declare_function "writeBoolean" (function_type (ltype_of_type TYPE_void) [|ltype_of_type(TYPE_bool)|]) the_module ) in 
                let _ = Hashtbl.add functions ("writeChar")  (declare_function "writeChar" (function_type (ltype_of_type TYPE_void) [|ltype_of_type(TYPE_char)|]) the_module ) in 
                let _ = Hashtbl.add functions ("writeReal")  (declare_function "writeReal" (function_type (ltype_of_type TYPE_void) [|x86fp80_type context|]) the_module ) in 
                let _ = Hashtbl.add functions ("readInteger")  (declare_function "readInteger" (function_type (ltype_of_type TYPE_int) [||]) the_module ) in 
                let _ = Hashtbl.add functions ("readBoolean")  (declare_function "readBoolean" (function_type (ltype_of_type TYPE_bool) [||]) the_module ) in 
                let _ = Hashtbl.add functions ("readChar")  (declare_function "readChar" (function_type (ltype_of_type TYPE_char) [||]) the_module ) in 
                let _ = Hashtbl.add functions ("readReal")  (declare_function "readReal" (function_type (ltype_of_type TYPE_double) [||]) the_module ) in 
                let _ = Hashtbl.add functions ("readString")  (declare_function "readString" (function_type (ltype_of_type TYPE_double) [|ltype_of_type TYPE_int ; ltype_of_type (TYPE_pointer TYPE_char)|]) the_module ) in 
                let _ = Hashtbl.add functions ("abs")  (declare_function "abs" (function_type (ltype_of_type TYPE_int) [|ltype_of_type TYPE_int |]) the_module ) in 
                let _ = Hashtbl.add functions ("fabs")  (declare_function "fabs" (function_type (ltype_of_type TYPE_double) [|ltype_of_type TYPE_double |]) the_module ) in 
                let _ = Hashtbl.add functions ("sqrt")  (declare_function "sqrt" (function_type (ltype_of_type TYPE_double) [|ltype_of_type TYPE_double |]) the_module ) in 
                let _ = Hashtbl.add functions ("sin")  (declare_function "sin" (function_type (ltype_of_type TYPE_double) [|ltype_of_type TYPE_double |]) the_module ) in 
                let _ = Hashtbl.add functions ("cos")  (declare_function "cos" (function_type (ltype_of_type TYPE_double) [|ltype_of_type TYPE_double |]) the_module ) in 
                let _ = Hashtbl.add functions ("tan")  (declare_function "tan" (function_type (ltype_of_type TYPE_double) [|ltype_of_type TYPE_double |]) the_module ) in 
                let _ = Hashtbl.add functions ("atan")  (declare_function "atan" (function_type (ltype_of_type TYPE_double) [|ltype_of_type TYPE_double |]) the_module ) in 
                let _ = Hashtbl.add functions ("exp")  (declare_function "exp" (function_type (ltype_of_type TYPE_double) [|ltype_of_type TYPE_double |]) the_module ) in 
                let _ = Hashtbl.add functions ("ln")  (declare_function "ln" (function_type (ltype_of_type TYPE_double) [|ltype_of_type TYPE_double |]) the_module ) in 
                let _ = Hashtbl.add functions ("pi")  (declare_function "pi" (function_type (ltype_of_type TYPE_double) [| |]) the_module ) in 
                let _ = Hashtbl.add functions ("trunc")  (declare_function "trunc" (function_type (ltype_of_type TYPE_int) [|ltype_of_type TYPE_double |]) the_module ) in 
                let _ = Hashtbl.add functions ("round")  (declare_function "round" (function_type (ltype_of_type TYPE_int) [|ltype_of_type TYPE_double |]) the_module ) in 
                let _ = Hashtbl.add functions ("ord")  (declare_function "ord" (function_type (ltype_of_type TYPE_int) [|ltype_of_type TYPE_char |]) the_module ) in 
                let _ = Hashtbl.add functions ("chr")  (declare_function "chr" (function_type (ltype_of_type TYPE_char) [|ltype_of_type TYPE_int |]) the_module ) in 
                let _ = Hashtbl.add functions ("strlen")  (declare_function "strlen" (function_type (ltype_of_type TYPE_int) [|ltype_of_type (TYPE_pointer TYPE_char) |]) the_module ) in 
                let _ = Hashtbl.add functions ("strcmp")  (declare_function "strcmp" (function_type (ltype_of_type TYPE_int) [|ltype_of_type (TYPE_pointer TYPE_char) ; ltype_of_type (TYPE_pointer TYPE_char)|]) the_module ) in 
                let _ = Hashtbl.add functions ("strcpy")  (declare_function "strcpy" (function_type (ltype_of_type TYPE_void) [|ltype_of_type (TYPE_pointer TYPE_char) ; ltype_of_type (TYPE_pointer TYPE_char)|]) the_module ) in 
                let _ = Hashtbl.add functions ("strcat")  (declare_function "strcat" (function_type (ltype_of_type TYPE_void) [|ltype_of_type (TYPE_pointer TYPE_char) ; ltype_of_type (TYPE_pointer TYPE_char)|]) the_module ) in 
                ();;
let codegen_main main = 
     (*   let _ = codegen_lib() in*)
        let _ = is_main() in 

 	let _ =List.map (fun x-> match x with VarDecl(a) -> ignore((List.map (fun y-> codegen_global y) a) ) | _ ->()) main in
        let _ = List.map (fun x-> match x with FunDecl(a) -> ignore(codegen_func a
        )|_->()) main in
        let _ = List.map (fun x-> match x with FunDef(a,b,c) -> ignore(codegen_func a);ignore(make_function (FunDef(a,b,c))) | _ ->()) main in
        the_module ;;


