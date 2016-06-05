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
%token <string> T_Const_Char
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


%start program
%type<unit> program

%nonassoc T_Lbracket T_Rbracket 
%nonassoc T_Incr T_Decr 
%left T_Amp 
%left T_New 
%left T_Del 
%left T_Mul T_Mod T_Div
%left T_Add T_Sub
%nonassoc T_Gr T_Le T_Leq T_Geq T_Equal T_Neq
%left T_And T_Or 
%nonassoc T_Quest T_Colon
%right T_Eq T_PlusEq T_Minus_eq T_Dot_eq T_Div_eq T_Mod_eq 

%%

program: initialization declation+ T_Eof { ignore(is_main()); ast_tree := $2;check (Some $2);}

initialization: {ignore(initSymbolTable 256); ignore (openScope ());}
(*declation_plus: declation {}
        | declation_plus {}
*)
test64: {error "test";}
oScope :  {ignore(openScope();)}
cScope :  {ignore(closeScope();)}
inFun : {ignore(infun := !infun +1)}
outFun : {ignore(infun := !infun -1)}
inLoop : {ignore(nested_loops := !nested_loops +1)}
outLoop : {ignore(nested_loops := !nested_loops -1)}

declation: function_def {FunDef $1};
        | variable_declation {VarDecl $1} (*maybe it will replaced with empty later*)
        | fuction_declation {FunDecl $1}

        variable_declation: type_i declator_plus {List.map (fun x -> 
                let typeA = if is_some (get_second2(x)) then TYPE_array ($1,0) else $1 in (* later 0-> lenth,doesnt needed for semantics yet*)
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

fuction_declation:  function_declation1  T_Semicolon {$1};
function_declation1 : 
        type_i T_Id T_Lparen parameter_list? T_Rparen{
        let e= newFunction (id_make $2) true in  
                may (List.iter (fun x-> ignore(newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) $4 ;  
                endFunctionHeader e $1 ; 
                e}
        |  T_Void T_Id T_Lparen parameter_list? T_Rparen{
        let e= newFunction (id_make $2) true in 
                 may (List.iter (fun x-> ignore (newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) $4 ;  
                 endFunctionHeader e TYPE_none; 
                 e }


        parameter_list: parameter test2* {[$1] @$2};
test2: T_Comma parameter {$2};

parameter: T_Byref? type_i T_Id {if is_some $1 then (PASS_BY_REFERENCE,$2,$3) else (PASS_BY_VALUE ,$2,$3)};

function_def: function_declation1 T_Lbrace oScope  declation* statement* cScope T_Rbrace {
        
        
        ($2,$4,$5)};

statement: T_Semicolon {SExpr None}
        | expression T_Semicolon {SExpr (Some $1)}
        | T_Lbrace oScope statement* cScope  T_Rbrace {SNewblock $3}
        | T_If  T_Lparen expression T_Rparen statement test3? {Sif ($3,$5,$6)}
        | test4? T_For  T_Lparen expression_list? T_Semicolon expression_list? T_Semicolon expression_list? T_Rparen inLoop statement outLoop {Sfor ($1,$4,$6,$8,$11)}
        |T_Cont  T_Id? T_Semicolon {if !nested_loops =0 then (error "No continue in Loop"; SCont $2) else SCont $2}
        |T_Break T_Id? T_Semicolon {if !nested_loops = 0 then (error "No break in loop" ; SBreak $2)  else SBreak $2}
        |T_Return expression? T_Semicolon {Sreturn $2};

test3: T_Else statement{$2};
test4: T_Id T_Colon {$1};

expression: T_Id {Eid $1}
        | T_Lparen expression T_Rparen  {$2}
        |T_True {Ebool true}
        |T_False {Ebool false}
        |T_Null {ENull}
        |T_Const_Char {Estring $1}
        |T_Const_Int {Eint $1}
        |T_Const_Real {Ereal $1}
        |T_Const_String {Estring $1}
        |T_Id  T_Lparen expression_list? T_Rparen {ENull} (* FIX THIS*)
        |expression T_Lbracket oScope expression cScope T_Rbracket {ENull} (* Fix this*)
        |T_Amp expression {EAmber $2}
        |T_Mul expression {EPointer $2}
        |T_Add expression {EUnAdd $2}
        |T_Sub expression {EUnMinus $2}
        |T_Not expression {Enot $2}
        |expression T_Mul expression {Emult ($1,$3)}
        |expression T_Div expression {Ediv ($1,$3)}
        |expression T_Mod expression {Emod ($1,$3)}
        |expression T_Add expression {Eplus ($1,$3)}
        |expression T_Sub expression {Eminus ($1,$3)}
        |expression T_Le expression {Elt ($1,$3)}
        |expression T_Leq expression {Elte ($1,$3)}
        |expression T_Gr expression {Egt ($1,$3)}
        |expression T_Geq expression {Egte ($1,$3)}
        |expression T_Equal expression {Eeq ($1,$3)}
        |expression T_Neq expression {Eneq ($1,$3)}
        |expression T_And expression {Eand ($1,$3)}
        |expression T_Or expression {Eor ($1,$3)}
        |expression T_Comma expression {Ecomma ($1,$3)}
        |T_Incr expression {EPlusPlus ($2,PRE)}
        |T_Decr expression {EMinusMinus ($2,PRE)}
        |expression T_Incr {EPlusPlus ($1,AFTER)}
        |expression T_Decr {EMinusMinus ($1,AFTER)}
        |expression T_Eq expression {EAssignEq ($1,$3)}
        |expression T_PlusEq expression {EPlusEq ($1,$3)}
        |expression T_Minus_eq expression {EMinusEq ($1,$3)}
        |expression T_Dot_eq expression {EDotEq ($1,$3)}
        |expression T_Div_eq expression {EDivEq ($1,$3)}
        |expression T_Mod_eq expression {EModEq ($1,$3)}
        |T_Lparen type_i T_Rparen expression {ECast ($2,$4)}
        |expression T_Quest expression T_Colon expression {EQuestT ($1,$3,$5)}
        |T_New type_i  test8? {ENew ($2,$3)}
        |T_Del expression {EDel $2};
test8:  T_Lbracket oScope expression cScope T_Rbracket {$3};

expression_list: expression test9* {check_expr $1;[$1] @ $2};
test9: T_Comma expression {check_expr $2 ;$2};

constant_expression:expression {$1};

