%{
        open Lexing
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
%token <string> T_Const_Int
%token  <string> T_Const_Real
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

program: declation+ T_Eof {};

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


        declator: T_Id option(test) {};
        test: T_Lbracket constant_expression T_Rbracket {};

        function_declation : result_type T_Id T_Lparen parameter_list? T_Rparen T_Semicolon {};

result_type: type_i {}
        | T_Void {};

parameter_list: parameter test2* {};
test2: T_Comma parameter {};

parameter: T_Byref? type_i T_Id {};

function_def: result_type T_Id T_Lparen parameter_list? T_Rparen  T_Lbrace test52* T_Rbrace {};

test52: declation? statement* {} ;

statement: T_Semicolon {}
        | expression T_Semicolon {}
        | T_Lbrace statement*  T_Rbrace {}
        | T_If  T_Lparen expression T_Rparen statement test3?
        | test4? T_For  T_Lparen expression_list? T_Semicolon expression? T_Semicolon expression? T_Rparen statement {}
        |T_Cont  T_Id? T_Semicolon {}
        |T_Break T_Id? T_Semicolon {}
        |T_Return expression? T_Semicolon {};

test3: T_Else statement{};
test4: T_Id T_Colon {};

expression: T_Id {}
        | T_Lparen expression T_Rparen  {}
        |T_True {}
        |T_False {}
        |T_Null {}
        |T_Const_Char {}
        |T_Const_Int {}
        |T_Const_Real {}
        |T_Const_String {}
        |T_Id  T_Lparen expression_list? T_Rparen {}
        |expression T_Lbracket expression T_Rbracket {}
        |unary_operator expression {}
        |expression binary_operator expression {}
        |unary_assig expression {}
        |expression unary_assig {}
        |expression binary_assig expression {}
        |T_Lparen type_i T_Rparen expression {}
        |expression T_Quest expression T_Colon expression {}
        |T_New type_i  test8? {}
        |T_Del expression {};
test8:  T_Lbracket expression T_Rbracket {};

expression_list: expression test9* {};
test9: T_Comma expression {};

constant_expression:expression {};

unary_operator: T_Amp {}
        | T_Mul {}
        | T_Add {}
        | T_Sub {}
        | T_Not {};


binary_operator: T_Mul {}
        |T_Div {}
        |T_Mod {}
        |T_Add {}
        |T_Sub {}
        |T_Le {}
        |T_Leq {}
        |T_Gr {}
        |T_Geq {}
        |T_Equal {}
        |T_Neq {}
        |T_And {}
        |T_Or {}
        |T_Comma {};

unary_assig: T_Incr {}
        |T_Decr {};

binary_assig: T_Eq {}
        | T_PlusEq {}
        | T_Minus_eq {}
        | T_Dot_eq {}
        | T_Div_eq {}
        | T_Mod_eq {};
