%{

%}

(*Keyword tokens*)

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
%token <string> T_Const_Real
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
%token <string> T_Plus_Eq
%token <string> T_Minus_Eq
%token <string> T_Dot_Eq
%token <string> T_Div_Eq
%token <string> T_Mod_Eq



%token T_Semicolon
%token T_Lparen
%token T_Rparen
%token T_Lbracket
%token T_Rbracket
%token T_Lbrace
%token T_Rbrace

%token T_Eof



