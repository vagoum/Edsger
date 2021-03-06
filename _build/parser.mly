%{
        open Lexing
        open Symbol
        open Types
        open Ast
        open Error
        open Semantic
        open Identifier
        open Option


        let get_first (x,_,_) =x ;;
        let get_first2 (x,_) =x ;;
        let get_second (_,x,_)=x;;
        let get_second2 (_,x)=x;;
        let get_third (_,_,x)=x;;
%}

(* Keyword tokens *)

%token T_Bool
%token T_Break
%token T_Byref
%token T_Char
%token T_Cont
%token T_Del
%token T_Double
%token T_Else
%token T_For
%token T_False
%token T_If
%token T_Int
%token T_New
%token T_Null
%token T_Return
%token T_True
%token T_Void

%token <string> T_Id
%token <int> T_Const_Int
%token <float> T_Const_Real
%token <char> T_Const_Char
%token <string> T_Const_String

(*Operators tokens*)

%token T_Eq
%token T_Equal
%token T_Neq
%token T_Gr
%token T_Le
%token T_Geq
%token T_Leq

%token T_Add
%token T_Sub
%token T_Mul
%token T_Div
%token T_Mod

%token T_Amp
%token T_Not
%token T_And
%token T_Or
%token T_Quest
%token T_Colon
%token T_Comma
%token T_Incr
%token T_Decr
%token T_PlusEq
%token T_Minus_eq
%token T_Dot_eq
%token T_Div_eq
%token T_Mod_eq



%token T_Semicolon
%token T_Lparen
%token T_Rparen
%token T_Lbracket
%token T_Rbracket
%token T_Lbrace
%token T_Rbrace

%token T_Eof

%token Incr_dcr_postfix
%token Incr_dcr_prefix
%token Adress_etc
%token Cast_
%token Special_Quest
%token Fuction_Call
%token Array_place
%token NonElse
%token SComma
%start program
%type<unit> program




%nonassoc NonElse
%nonassoc T_Else


%left T_Comma
%right T_Eq T_PlusEq T_Minus_eq T_Dot_eq T_Div_eq T_Mod_eq 
%nonassoc T_Colon 
%nonassoc T_Quest
%nonassoc Special_Quest
%left T_Or
%left T_And 
%nonassoc T_Gr T_Le T_Leq T_Geq T_Equal T_Neq
%left T_Add T_Sub
%left T_Mul T_Mod T_Div
%nonassoc Cast_
%nonassoc Incr_dcr_prefix
%nonassoc T_New  T_Del
%nonassoc T_Amp Adress_etc
%nonassoc Incr_dcr_postfix 
%right T_Rbracket
%left T_Lbracket
%nonassoc Array_place Fuction_Call T_Lparen 


%%

program: initialization declation+ T_Eof { (*ignore(is_main());*) ast_tree := ($2@(!ast_tree));check (Some $2);}

initialization: {(*ignore(initSymbolTable 256);*) ignore (openScope ());}
(*declation_plus: declation {}
        | declation_plus {}
*)
oScope :  {ignore(openScope();)}
cScope :  {ignore(closeScope();)}
cScope2 :  {ignore(closeScope2();)}
inFun : {ignore(infun := !infun +1)}
outFun : {ignore(infun := !infun -1)}
inLoop : {ignore(nested_loops := !nested_loops +1)}
outLoop : {ignore(nested_loops := !nested_loops -1)}

declation: function_def {FunDef $1};
        | variable_declation {VarDecl $1} (*maybe it will replaced with empty later*)
        | fuction_declation {FunDecl $1}

        variable_declation: type_i declator_plus {List.map (fun x -> 
                let typeA = if is_some (get_second2(x)) then (ignore(check_array_len (get_second2(x))); TYPE_array ($1,evaluate_constant (get (get_second2(x)))) ) else $1 in (* later 0-> lenth,doesnt needed for semantics yet*)
                newVariable (id_make (get_first2 x)) typeA true) $2};

declator_plus: declator T_Semicolon {[$1]}
        | declator T_Comma declator_plus {$1::$3};


type_i: basic_type T_Mul* {if $2 = [] then $1 else List.fold_left (fun x->fun y-> TYPE_pointer x ) $1 $2};

basic_type: T_Int  {TYPE_int}
        | T_Char {TYPE_char}
        | T_Bool {TYPE_bool}
        | T_Double {TYPE_double};


        declator: T_Id test? { ($1,$2)}; (*check if intialization type is correct*)
test: T_Lbracket constant_expression T_Rbracket {$2};

fuction_declation:  function_declation1  T_Semicolon cScope {$1};
function_declation1 : 
        type_i T_Id T_Lparen parameter_list? T_Rparen{
        let e= newFunction (id_make $2) false in 
	let _ = forwardFunction e in 
        let _ = openScope() in
                may (List.iter (fun x-> ignore(newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) $4 ;  
                endFunctionHeader e $1 ; 
                e}
        |  T_Void T_Id T_Lparen parameter_list? T_Rparen{
        let e= newFunction (id_make $2) false in 
	let _ = forwardFunction e in 
        let _ = openScope() in
                 may (List.iter (fun x-> ignore (newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) $4 ;  
                 endFunctionHeader e TYPE_none; 
                 e }
function_declation2 : 
        type_i T_Id T_Lparen parameter_list? T_Rparen{
        let e= newFunction (id_make $2) false in  
        let _ = openScope() in
                may (List.iter (fun x-> ignore(newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) $4 ;  
                endFunctionHeader e $1 ; 
                e}
        |  T_Void T_Id T_Lparen parameter_list? T_Rparen{
        let e= newFunction (id_make $2) false in 
        let _ = openScope() in
                 may (List.iter (fun x-> ignore (newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) $4 ;  
                 endFunctionHeader e TYPE_none; 
                 e }

        parameter_list: parameter test2* {[$1] @$2};
test2: T_Comma parameter {$2};

parameter: T_Byref? type_i T_Id {if is_some $1 then (PASS_BY_REFERENCE,$2,$3) else (PASS_BY_VALUE ,$2,$3)};

function_def: function_declation2 T_Lbrace oScope  declation* statement* cScope T_Rbrace cScope2{
        
        
        ($1,$4,$5)};

statement: T_Semicolon {SExpr None}
        | expression T_Semicolon {SExpr (Some $1)}
        | T_Lbrace oScope statement* cScope  T_Rbrace {SNewblock $3}
        | T_If  T_Lparen expression T_Rparen statement %prec NonElse {Sif ($3,$5,None)}
        | T_If  T_Lparen expression T_Rparen statement T_Else statement {Sif ($3,$5,Some $7)}
        | test4? T_For  T_Lparen expression? T_Semicolon expression? T_Semicolon expression? T_Rparen inLoop statement outLoop {Sfor ($1,$4,$6,$8,$11)}
        |T_Cont  T_Id? T_Semicolon {if !nested_loops =0 then (error "No continue in Loop"; SCont $2) else SCont $2}
        |T_Break T_Id? T_Semicolon {if !nested_loops = 0 then (error "No break in loop" ; SBreak $2)  else SBreak $2}
        |T_Return expression? T_Semicolon {Sreturn $2};

test4: T_Id T_Colon {$1};
fuction_call:T_Id  T_Lparen expression_list7? T_Rparen %prec Fuction_Call { let k = if is_some $3 then get_some1 $3 else [] in 
(match (check_name_lib $1) with 
| true ->()
|false -> ignore(check_function_call (lookupEntry (id_make $1) LOOKUP_ALL_SCOPES true) k) 
); ECall ($1,$3)} 
expression: expression1 {ignore(get_type $1);$1}
expression1: 
         fuction_call {$1}
        |T_Id {Eid $1}
        |expression array_expresion {EArray ($1,$2)} 
        | T_Lparen expression T_Rparen  {$2}
        |T_True {Ebool true}
        |T_False {Ebool false}
        |T_Null {ENull}
        |T_Const_Char {Echar $1}
        |T_Const_Int {Eint $1}
        |T_Const_Real {Ereal $1}
        |T_Const_String {Estring $1}
        |T_Amp expression {EAmber $2}
        |T_Mul expression %prec Adress_etc{EPointer $2}
        |T_Add expression %prec Adress_etc{EUnAdd $2}
        |T_Sub expression %prec Adress_etc{EUnMinus $2}
        |T_Not expression %prec Adress_etc{Enot $2}
        |expression T_Mul expression {Emult ($1,$3)}
        |expression T_Div expression {Ediv ($1,$3)}
        |expression T_Mod expression {Emod ($1,$3)}
        |expression T_Add expression {Eplus ($1,$3)}
        |expression T_Sub expression {Eminus ($1,$3)}
        |expression T_Le expression {Elt ($1,$3)}
        |expression T_Leq expression {Elte ($1,$3)}
        |expression T_Gr expression {Egt ($1,$3)}
        |expression T_Geq expression {Egte ($1,$3)}
        |expression T_Equal  expression {Eeq ($1,$3)}
        |expression T_Neq expression {Eneq ($1,$3)}
        |expression T_And expression {Eand ($1,$3)}
        |expression T_Or expression {Eor ($1,$3)}
        |expression T_Comma expression {Ecomma ($1,$3)}
        |T_Incr expression %prec Incr_dcr_postfix{EPlusPlus ($2,PRE)}
        |T_Decr expression %prec Incr_dcr_postfix{EMinusMinus ($2,PRE)}
        |expression T_Incr %prec Incr_dcr_prefix{EPlusPlus ($1,AFTER)}
        |expression T_Decr %prec Incr_dcr_prefix{EMinusMinus ($1,AFTER)}
        |expression T_Eq expression {EAssignEq ($1,$3)}
        |expression T_PlusEq expression {EPlusEq ($1,$3)}
        |expression T_Minus_eq expression {EMinusEq ($1,$3)}
        |expression T_Dot_eq expression {EDotEq ($1,$3)}
        |expression T_Div_eq expression {EDivEq ($1,$3)}
        |expression T_Mod_eq expression {EModEq ($1,$3)}
        |T_Lparen type_i T_Rparen expression %prec Cast_ {ECast ($2,$4)}
        |expression T_Quest expression T_Colon expression  {EQuestT ($1,$3,$5)}
        |T_New type_i  test8? {if Option.is_some $3 then ENew ($2,(Option.get $3)) else ENew ($2,Eint(1))}
        |T_Del expression {EDel $2};
test8:  T_Lbracket oScope expression cScope T_Rbracket {$3};
array_expresion: T_Lbracket expression T_Rbracket %prec Array_place {if (get_type $2)=TYPE_int then () else error "Not an int on array";$2}

expression_list7: expression7 test97* {check_expr $1;[$1] @ $2};
test97: T_Comma expression7 {check_expr $2 ;$2};
constant_expression:expression {$1};


expression7: 
         fuction_call {$1}
        |T_Id {Eid $1}
        |expression7 array_expresion {EArray ($1,$2)} 
        | T_Lparen expression T_Rparen  {$2}
        |T_True {Ebool true}
        |T_False {Ebool false}
        |T_Null {ENull}
        |T_Const_Char {Echar $1}
        |T_Const_Int {Eint $1}
        |T_Const_Real {Ereal $1}
        |T_Const_String {Estring $1}
        |T_Amp expression7 {EAmber $2}
        |T_Mul expression7 %prec Adress_etc{EPointer $2}
        |T_Add expression7 %prec Adress_etc{EUnAdd $2}
        |T_Sub expression7 %prec Adress_etc{EUnMinus $2}
        |T_Not expression7 %prec Adress_etc{Enot $2}
        |expression7 T_Mul expression7 {Emult ($1,$3)}
        |expression7 T_Div expression7 {Ediv ($1,$3)}
        |expression7 T_Mod expression7 {Emod ($1,$3)}
        |expression7 T_Add expression7 {Eplus ($1,$3)}
        |expression7 T_Sub expression7 {Eminus ($1,$3)}
        |expression7 T_Le expression7 {Elt ($1,$3)}
        |expression7 T_Leq expression7 {Elte ($1,$3)}
        |expression7 T_Gr expression7 {Egt ($1,$3)}
        |expression7 T_Geq expression7 {Egte ($1,$3)}
        |expression7 T_Equal  expression7 {Eeq ($1,$3)}
        |expression7 T_Neq expression7 {Eneq ($1,$3)}
        |expression7 T_And expression7 {Eand ($1,$3)}
        |expression7 T_Or expression7 {Eor ($1,$3)}
        |T_Incr expression7 %prec Incr_dcr_postfix{EPlusPlus ($2,PRE)}
        |T_Decr expression7 %prec Incr_dcr_postfix{EMinusMinus ($2,PRE)}
        |expression7 T_Incr %prec Incr_dcr_prefix{EPlusPlus ($1,AFTER)}
        |expression7 T_Decr %prec Incr_dcr_prefix{EMinusMinus ($1,AFTER)}
        |expression7 T_Eq expression7 {EAssignEq ($1,$3)}
        |expression7 T_PlusEq expression7 {EPlusEq ($1,$3)}
        |expression7 T_Minus_eq expression7 {EMinusEq ($1,$3)}
        |expression7 T_Dot_eq expression7 {EDotEq ($1,$3)}
        |expression7 T_Div_eq expression7 {EDivEq ($1,$3)}
        |expression7 T_Mod_eq expression7 {EModEq ($1,$3)}
        |T_Lparen type_i T_Rparen expression %prec Cast_ {ECast ($2,$4)}
        |expression7 T_Quest expression7 T_Colon expression7 {EQuestT ($1,$3,$5)}
        |T_New type_i  test8? {if Option.is_some $3 then ENew ($2,(Option.get $3)) else ENew ($2,Eint(1))}
        |T_Del expression7 {EDel $2};
