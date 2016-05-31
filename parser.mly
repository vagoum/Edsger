%{
        open Lexing
        open Symbol
        open Types
        open Ast
        open Error
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

program: declation+ T_Eof {ignore(initSymbolTable 256 ); ignore(openScope()); ignore(is_main()); ast_tree := $1};

(*declation_plus: declation {}
        | declation_plus {}
*)
declation: variable_declation {}
        | function_declation {}
        | function_def {};

variable_declation: type_i declator_plus {};

declator_plus: declator T_Semicolon {}
        | declator T_Comma declator_plus {};


type_i: basic_type T_Mul* {};

basic_type: T_Int  {}
        | T_Char {}
        | T_Bool {}
        | T_Double {};


declator: T_Id test? {};
test: T_Lbracket constant_expression T_Rbracket {};

function_declation : 
        type_i T_Id T_Lparen parameter_list? T_Rparen T_Semicolon {}
        |T_Void T_Id T_Lparen parameter_list? T_Rparen T_Semicolon {};

result_type: type_i{}
        | T_Void {};

parameter_list: parameter test2* {};
test2: T_Comma parameter {};

parameter: T_Byref? type_i T_Id {};

function_def:
        type_i T_Id T_Lparen parameter_list? T_Rparen  T_Lbrace declation* statement* T_Rbrace {};
        |T_Void T_Id T_Lparen parameter_list? T_Rparen  T_Lbrace declation* statement* T_Rbrace {};


statement: T_Semicolon {SExpr None}
        | expression T_Semicolon {SExpr (Some $1)}
        | T_Lbrace statement*  T_Rbrace {SNewBlock $2}
        | T_If  T_Lparen expression T_Rparen statement test3? {Sif ($3,$5,$6)}
        | test4? T_For  T_Lparen expression_list? T_Semicolon expression_list? T_Semicolon expression_list? T_Rparen statement {Sfor ($1,$4,$6,$8,$10)}
        |T_Cont  T_Id? T_Semicolon {SCont $2}
        |T_Break T_Id? T_Semicolon {SBreak $2}
        |T_Return expression? T_Semicolon {Sreturn $2};

test3: T_Else statement{$2};
test4: T_Id T_Colon {};

expression: T_Id {}
        | T_Lparen expression T_Rparen  {}
        |T_True {Ebool true}
        |T_False {Ebool false}
        |T_Null {ENull}
        |T_Const_Char {Echar $1}
        |T_Const_Int {Eint $1}
        |T_Const_Real {Ereal $1}
        |T_Const_String {Estring $1}
        |T_Id  T_Lparen expression_list? T_Rparen {}
        |expression T_Lbracket expression T_Rbracket {}
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
        |expression T_Incr {EPlusPlus ($2,AFTER)}
        |expression T_Decr {EMinusMinus ($2,AFTER)}
        |expression T_Eq expression {EAssignEq ($1,$3)}
        |expression T_PlusEq expression {EPlusEq ($1,$3)}
        |expression T_Minus_eq expression {EMinusEq ($1,$3)}
        |expression T_Dot_eq expression {EDotEq ($1,$3)}
        |expression T_Div_eq expression {EDivEq ($1,$3)}
        |expression T_Mod_eq expression {EModEq ($1,$3)}
        |T_Lparen type_i T_Rparen expression {}
        |expression T_Quest expression T_Colon expression {}
        |T_New type_i  test8? {Enew}
        |T_Del expression {EDel};
test8:  T_Lbracket expression T_Rbracket {};

expression_list: expression test9* {};
test9: T_Comma expression {};

constant_expression:expression {};

