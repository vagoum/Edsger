
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | T_Void
    | T_True
    | T_Sub
    | T_Semicolon
    | T_Rparen
    | T_Return
    | T_Rbracket
    | T_Rbrace
    | T_Quest
    | T_PlusEq
    | T_Or
    | T_Null
    | T_Not
    | T_New
    | T_Neq
    | T_Mul
    | T_Mod_eq
    | T_Mod
    | T_Minus_eq
    | T_Lparen
    | T_Leq
    | T_Le
    | T_Lbracket
    | T_Lbrace
    | T_Int
    | T_Incr
    | T_If
    | T_Id of (
# 40 "parser.mly"
       (string)
# 38 "parser.ml"
  )
    | T_Gr
    | T_Geq
    | T_For
    | T_False
    | T_Equal
    | T_Eq
    | T_Eof
    | T_Else
    | T_Double
    | T_Dot_eq
    | T_Div_eq
    | T_Div
    | T_Del
    | T_Decr
    | T_Cont
    | T_Const_String of (
# 44 "parser.mly"
       (string)
# 58 "parser.ml"
  )
    | T_Const_Real of (
# 42 "parser.mly"
       (float)
# 63 "parser.ml"
  )
    | T_Const_Int of (
# 41 "parser.mly"
       (int)
# 68 "parser.ml"
  )
    | T_Const_Char of (
# 43 "parser.mly"
       (char)
# 73 "parser.ml"
  )
    | T_Comma
    | T_Colon
    | T_Char
    | T_Byref
    | T_Break
    | T_Bool
    | T_And
    | T_Amp
    | T_Add
    | Special_Quest
    | SComma
    | NonElse
    | Incr_dcr_prefix
    | Incr_dcr_postfix
    | Fuction_Call
    | Cast_
    | Array_place
    | Adress_etc
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState284
  | MenhirState282
  | MenhirState277
  | MenhirState274
  | MenhirState272
  | MenhirState270
  | MenhirState267
  | MenhirState264
  | MenhirState262
  | MenhirState260
  | MenhirState256
  | MenhirState251
  | MenhirState247
  | MenhirState244
  | MenhirState243
  | MenhirState242
  | MenhirState240
  | MenhirState239
  | MenhirState238
  | MenhirState235
  | MenhirState233
  | MenhirState232
  | MenhirState231
  | MenhirState229
  | MenhirState222
  | MenhirState214
  | MenhirState213
  | MenhirState212
  | MenhirState208
  | MenhirState207
  | MenhirState205
  | MenhirState204
  | MenhirState203
  | MenhirState201
  | MenhirState198
  | MenhirState197
  | MenhirState196
  | MenhirState195
  | MenhirState191
  | MenhirState190
  | MenhirState189
  | MenhirState188
  | MenhirState187
  | MenhirState186
  | MenhirState185
  | MenhirState184
  | MenhirState183
  | MenhirState182
  | MenhirState181
  | MenhirState180
  | MenhirState179
  | MenhirState178
  | MenhirState177
  | MenhirState176
  | MenhirState175
  | MenhirState174
  | MenhirState173
  | MenhirState172
  | MenhirState171
  | MenhirState170
  | MenhirState169
  | MenhirState168
  | MenhirState167
  | MenhirState166
  | MenhirState165
  | MenhirState164
  | MenhirState163
  | MenhirState162
  | MenhirState161
  | MenhirState160
  | MenhirState159
  | MenhirState158
  | MenhirState157
  | MenhirState156
  | MenhirState155
  | MenhirState154
  | MenhirState153
  | MenhirState152
  | MenhirState151
  | MenhirState150
  | MenhirState149
  | MenhirState145
  | MenhirState144
  | MenhirState143
  | MenhirState142
  | MenhirState141
  | MenhirState140
  | MenhirState139
  | MenhirState137
  | MenhirState136
  | MenhirState131
  | MenhirState130
  | MenhirState127
  | MenhirState125
  | MenhirState124
  | MenhirState123
  | MenhirState121
  | MenhirState120
  | MenhirState119
  | MenhirState118
  | MenhirState117
  | MenhirState116
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState112
  | MenhirState111
  | MenhirState110
  | MenhirState109
  | MenhirState108
  | MenhirState107
  | MenhirState106
  | MenhirState105
  | MenhirState104
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState98
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState94
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState65
  | MenhirState64
  | MenhirState59
  | MenhirState58
  | MenhirState56
  | MenhirState55
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState30
  | MenhirState28
  | MenhirState22
  | MenhirState19
  | MenhirState18
  | MenhirState11
  | MenhirState9
  | MenhirState8
  | MenhirState5
  | MenhirState2

# 1 "parser.mly"
  
        open Lexing
        open Symbol
        open Types
        open Ast
        open Error
        open Semantic
        open Identifier
        open Option
        open Lamda_lift


        let get_first (x,_,_) =x ;;
        let get_first2 (x,_) =x ;;
        let get_second (_,x,_)=x;;
        let get_second2 (_,x)=x;;
        let get_third (_,_,x)=x;;

# 306 "parser.ml"

let rec _menhir_goto_list_test97_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.ast_expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.ast_expr))) = _menhir_stack in
        let _v : (Ast.ast_expr list) = 
# 187 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 319 "parser.ml"
         in
        _menhir_goto_list_test97_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Ast.ast_expr list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
        let _v : (Ast.ast_expr list) = 
# 260 "parser.mly"
                                      (check_expr _1;[_1] @ _2)
# 330 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.ast_expr list)) = _v in
        let _v : (Ast.ast_expr list option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 338 "parser.ml"
         in
        _menhir_goto_option_expression_list7_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_T_Id_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState247 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (string option))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_stmt) = 
# 199 "parser.mly"
                                   (if !nested_loops =0 then (error "No continue in Loop"; SCont _2) else SCont _2)
# 363 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState251 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (string option))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_stmt) = 
# 200 "parser.mly"
                                   (if !nested_loops = 0 then (error "No break in loop" ; SBreak _2)  else SBreak _2)
# 387 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce119 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr list) = 
# 185 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 404 "parser.ml"
     in
    _menhir_goto_list_test97_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run196 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196

and _menhir_run142 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142

and _menhir_run153 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153

and _menhir_run155 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155

and _menhir_run157 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157

and _menhir_run144 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144

and _menhir_run173 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173

and _menhir_run149 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149

and _menhir_run175 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175

and _menhir_run161 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161

and _menhir_run163 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163

and _menhir_run165 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState165
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165

and _menhir_run167 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState167

and _menhir_run169 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169

and _menhir_run177 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177

and _menhir_run179 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179

and _menhir_run181 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181

and _menhir_run151 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151

and _menhir_run171 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171

and _menhir_run159 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState159
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159

and _menhir_run140 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140

and _menhir_run146 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) = 
# 297 "parser.mly"
                                                 (EPlusPlus (_1,AFTER))
# 1404 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run147 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) = 
# 298 "parser.mly"
                                                 (EMinusMinus (_1,AFTER))
# 1417 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_nonempty_list_declation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Eof ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (_1 : (unit))), _, (_2 : (Ast.ast_program))) = _menhir_stack in
            let _3 = () in
            let _v : (
# 99 "parser.mly"
     (unit)
# 1438 "parser.ml"
            ) = 
# 129 "parser.mly"
                                         ( ignore(is_main()); ast_tree := _2;check (Some _2);)
# 1442 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 99 "parser.mly"
     (unit)
# 1449 "parser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState284 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_decl))), _, (xs : (Ast.ast_program))) = _menhir_stack in
        let _v : (Ast.ast_program) = 
# 197 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1465 "parser.ml"
         in
        _menhir_goto_nonempty_list_declation_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_statement_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_stmt list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState272 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_stmt))), _, (xs : (Ast.ast_stmt list))) = _menhir_stack in
        let _v : (Ast.ast_stmt list) = 
# 187 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1482 "parser.ml"
         in
        _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState274
    | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState277
    | _ ->
        _menhir_fail ()

and _menhir_reduce128 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 1499 "parser.ml"
     in
    _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_test4_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_For ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Lparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_Amp ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_Const_Char _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | T_Const_Int _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | T_Const_Real _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | T_Const_String _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | T_Decr ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_Del ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_False ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | T_Semicolon ->
                _menhir_reduce128 _menhir_env (Obj.magic _menhir_stack) MenhirState260
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState260)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce126 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (string option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 1580 "parser.ml"
     in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run248 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 40 "parser.mly"
       (string)
# 1587 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 40 "parser.mly"
       (string)
# 1595 "parser.ml"
    )) = _v in
    let _v : (string option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 1600 "parser.ml"
     in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_declation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState232 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Amp ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Break ->
            _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Const_Char _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
        | T_Const_Int _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
        | T_Const_Real _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
        | T_Const_String _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
        | T_Cont ->
            _menhir_run247 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Decr ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Del ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_False ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Id _v ->
            _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
        | T_If ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Lbrace ->
            _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Return ->
            _menhir_run235 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Semicolon ->
            _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_For ->
            _menhir_reduce136 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | T_Rbrace ->
            _menhir_reduce115 _menhir_env (Obj.magic _menhir_stack) MenhirState233
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState233)
    | MenhirState282 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_decl))), _, (xs : (Ast.ast_decl list))) = _menhir_stack in
        let _v : (Ast.ast_decl list) = 
# 187 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1676 "parser.ml"
         in
        _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_expression_list7_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr list option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Rparen ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (
# 40 "parser.mly"
       (string)
# 1696 "parser.ml"
        ))), _, (_3 : (Ast.ast_expr list option))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (Ast.ast_expr) = 
# 204 "parser.mly"
                                                                          ( let k = if is_some _3 then get_some1 _3 else [] in 
(match (check_name_lib _1) with 
| true ->()
|false -> ignore(check_function_call (lookupEntry (id_make _1) LOOKUP_ALL_SCOPES true) k) 
); ECall (_1,_3))
# 1707 "parser.ml"
         in
        (match _menhir_s with
        | MenhirState233 | MenhirState240 | MenhirState272 | MenhirState244 | MenhirState256 | MenhirState260 | MenhirState262 | MenhirState264 | MenhirState267 | MenhirState242 | MenhirState235 | MenhirState33 | MenhirState35 | MenhirState37 | MenhirState41 | MenhirState42 | MenhirState43 | MenhirState203 | MenhirState44 | MenhirState56 | MenhirState123 | MenhirState58 | MenhirState59 | MenhirState64 | MenhirState117 | MenhirState115 | MenhirState113 | MenhirState111 | MenhirState109 | MenhirState107 | MenhirState105 | MenhirState103 | MenhirState101 | MenhirState99 | MenhirState97 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState65 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.ast_expr)) = _v in
            let _v : (Ast.ast_expr) = 
# 211 "parser.mly"
                      (_1)
# 1717 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | MenhirState196 | MenhirState46 | MenhirState48 | MenhirState50 | MenhirState55 | MenhirState127 | MenhirState130 | MenhirState131 | MenhirState136 | MenhirState183 | MenhirState181 | MenhirState179 | MenhirState177 | MenhirState175 | MenhirState173 | MenhirState171 | MenhirState169 | MenhirState167 | MenhirState165 | MenhirState163 | MenhirState161 | MenhirState159 | MenhirState157 | MenhirState155 | MenhirState153 | MenhirState151 | MenhirState149 | MenhirState144 | MenhirState142 | MenhirState140 | MenhirState137 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.ast_expr)) = _v in
            let _v : (Ast.ast_expr) = 
# 266 "parser.mly"
                      (_1)
# 1727 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) = 
# 270 "parser.mly"
                (Ebool true)
# 1747 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) = 
# 272 "parser.mly"
                (ENull)
# 1806 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Bool ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_Char ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_Double ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_Int ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Bool ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Char ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Double ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Int ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_run128 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 40 "parser.mly"
       (string)
# 2028 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (
# 40 "parser.mly"
       (string)
# 2042 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.ast_expr) = 
# 267 "parser.mly"
              (Eid _1)
# 2047 "parser.ml"
         in
        _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run129 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) = 
# 271 "parser.mly"
                 (Ebool false)
# 2065 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run130 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131

and _menhir_run132 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 44 "parser.mly"
       (string)
# 2166 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 44 "parser.mly"
       (string)
# 2174 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 276 "parser.mly"
                        (Estring _1)
# 2179 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run133 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 42 "parser.mly"
       (float)
# 2186 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 42 "parser.mly"
       (float)
# 2194 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 275 "parser.mly"
                      (Ereal _1)
# 2199 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run134 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 41 "parser.mly"
       (int)
# 2206 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 41 "parser.mly"
       (int)
# 2214 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 274 "parser.mly"
                     (Eint _1)
# 2219 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 43 "parser.mly"
       (char)
# 2226 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 43 "parser.mly"
       (char)
# 2234 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 273 "parser.mly"
                      (Echar _1)
# 2239 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run136 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136

and _menhir_run137 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137

and _menhir_goto_option_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState235 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr option))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_stmt) = 
# 201 "parser.mly"
                                          (Sreturn _2)
# 2356 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState260 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_Amp ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_Const_Char _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
            | T_Const_Int _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
            | T_Const_Real _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
            | T_Const_String _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
            | T_Decr ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_Del ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_False ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | T_Semicolon ->
                _menhir_reduce128 _menhir_env (Obj.magic _menhir_stack) MenhirState262
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState262)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState262 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_Amp ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_Const_Char _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
            | T_Const_Int _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
            | T_Const_Real _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
            | T_Const_String _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
            | T_Decr ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_Del ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_False ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | T_Rparen ->
                _menhir_reduce128 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState264 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit) = 
# 141 "parser.mly"
         (ignore(nested_loops := !nested_loops +1))
# 2493 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Amp ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Break ->
                _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Const_Char _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState267 _v
            | T_Const_Int _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState267 _v
            | T_Const_Real _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState267 _v
            | T_Const_String _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState267 _v
            | T_Cont ->
                _menhir_run247 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Decr ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Del ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_False ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Id _v ->
                _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState267 _v
            | T_If ->
                _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Lbrace ->
                _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Return ->
                _menhir_run235 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Semicolon ->
                _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | T_For ->
                _menhir_reduce136 _menhir_env (Obj.magic _menhir_stack) MenhirState267
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState267)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run105 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run107 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_run97 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run99 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run101 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101

and _menhir_run109 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_run111 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run113 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run115 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115

and _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) = 
# 243 "parser.mly"
                                                (EPlusPlus (_1,AFTER))
# 3606 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) = 
# 244 "parser.mly"
                                                (EMinusMinus (_1,AFTER))
# 3619 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 279 "parser.mly"
                                           (EUnAdd _2)
# 3647 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_And ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Colon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState141 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Amp ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Const_Char _v ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Const_Int _v ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Const_Real _v ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Const_String _v ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Decr ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Del ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_False ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Id _v ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Incr ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Lparen ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Mul ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_New ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Not ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Null ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Sub ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_True ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Div_eq ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Dot_eq ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Eq ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Equal ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Geq ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Gr ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Le ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Leq ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Minus_eq ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Mod_eq ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Neq ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Or ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_PlusEq ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | T_Add | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 286 "parser.mly"
                                       (Eminus (_1,_3))
# 3782 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 282 "parser.mly"
                                       (Emult (_1,_3))
# 3809 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 284 "parser.mly"
                                       (Emod (_1,_3))
# 3836 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 283 "parser.mly"
                                       (Ediv (_1,_3))
# 3863 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_And ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Div_eq ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Dot_eq ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Eq ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Equal ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Geq ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Gr ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Le ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Leq ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Minus_eq ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Mod_eq ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Neq ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Or ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_PlusEq ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Colon | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 300 "parser.mly"
                                          (EPlusEq (_1,_3))
# 3928 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_And ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Equal ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Geq ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Gr ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Le ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Leq ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Neq ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 294 "parser.mly"
                                      (Eor (_1,_3))
# 3979 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156)
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 292 "parser.mly"
                                       (Eneq (_1,_3))
# 4016 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Add | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 285 "parser.mly"
                                       (Eplus (_1,_3))
# 4049 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 288 "parser.mly"
                                       (Elte (_1,_3))
# 4086 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162)
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 287 "parser.mly"
                                      (Elt (_1,_3))
# 4123 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164)
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 289 "parser.mly"
                                      (Egt (_1,_3))
# 4160 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166)
    | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 290 "parser.mly"
                                       (Egte (_1,_3))
# 4197 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 291 "parser.mly"
                                          (Eeq (_1,_3))
# 4234 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170)
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Equal ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Geq ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Gr ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Le ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Leq ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Neq ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 293 "parser.mly"
                                       (Eand (_1,_3))
# 4283 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172)
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_And ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Div_eq ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Dot_eq ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Eq ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Equal ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Geq ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Gr ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Le ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Leq ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Minus_eq ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Mod_eq ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Neq ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Or ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_PlusEq ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | T_Colon | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 304 "parser.mly"
                                          (EModEq (_1,_3))
# 4348 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_And ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Div_eq ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Dot_eq ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Eq ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Equal ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Geq ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Gr ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Le ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Leq ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Minus_eq ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Mod_eq ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Neq ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Or ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_PlusEq ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | T_Colon | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 301 "parser.mly"
                                            (EMinusEq (_1,_3))
# 4413 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176)
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_And ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Div_eq ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Dot_eq ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Eq ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Equal ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Geq ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Gr ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Le ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Leq ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Minus_eq ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Mod_eq ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Neq ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Or ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_PlusEq ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Colon | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 299 "parser.mly"
                                      (EAssignEq (_1,_3))
# 4478 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178)
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_And ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Div_eq ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Dot_eq ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Eq ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Equal ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Geq ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Gr ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Le ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Leq ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Minus_eq ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Mod_eq ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Neq ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Or ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_PlusEq ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | T_Colon | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 302 "parser.mly"
                                          (EDotEq (_1,_3))
# 4543 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180)
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_And ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Div_eq ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Dot_eq ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Eq ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Equal ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Geq ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Gr ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Le ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Leq ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Minus_eq ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Mod_eq ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Neq ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Or ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_PlusEq ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | T_Colon | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 303 "parser.mly"
                                          (EDivEq (_1,_3))
# 4608 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182)
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_And ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Equal ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Geq ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Gr ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Le ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Leq ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Neq ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Or ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_PlusEq | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 306 "parser.mly"
                                                                                 (EQuestT (_1,_3,_5))
# 4662 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 277 "parser.mly"
                           (EAmber _2)
# 4689 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185)
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 296 "parser.mly"
                                                  (EMinusMinus (_2,PRE))
# 4714 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 308 "parser.mly"
                           (EDel _2)
# 4741 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 295 "parser.mly"
                                                  (EPlusPlus (_2,PRE))
# 4766 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 278 "parser.mly"
                                           (EPointer _2)
# 4793 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 281 "parser.mly"
                                           (Enot _2)
# 4820 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 280 "parser.mly"
                                           (EUnMinus _2)
# 4847 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_And ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Comma ->
            _menhir_run196 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Div_eq ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Dot_eq ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Eq ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Equal ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Geq ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Gr ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Le ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Leq ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Minus_eq ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Mod_eq ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Neq ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Or ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_PlusEq ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Rparen ->
            _menhir_reduce119 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState195)
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_And ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Decr ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Div ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Div_eq ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Dot_eq ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Eq ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Equal ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Geq ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Gr ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Incr ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Le ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Leq ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Minus_eq ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Mod ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Mod_eq ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Mul ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Neq ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Or ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_PlusEq ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Quest ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Sub ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 261 "parser.mly"
                            (check_expr _2 ;_2)
# 4971 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Comma ->
                _menhir_run196 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | T_Rparen ->
                _menhir_reduce119 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState197)
    | _ ->
        _menhir_fail ()

and _menhir_goto_declator_plus : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.ast_expr option) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : ((string * Ast.ast_expr option) list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Types.typ))) = _menhir_stack in
        let _v : (Ast.ast_var_decl) = 
# 148 "parser.mly"
                                                 (List.map (fun x -> 
                let typeA = if is_some (get_second2(x)) then (ignore(check_array_len (get_second2(x))); TYPE_array (_1,evaluate_constant (get (get_second2(x)))) ) else _1 in (* later 0-> lenth,doesnt needed for semantics yet*)
                newVariable (id_make (get_first2 x)) typeA true) _2)
# 5006 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.ast_var_decl)) = _v in
        let _v : (Ast.ast_decl) = 
# 145 "parser.mly"
                             (VarDecl _1)
# 5014 "parser.ml"
         in
        _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
    | MenhirState222 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : ((string * Ast.ast_expr option) list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (string * Ast.ast_expr option))) = _menhir_stack in
        let _2 = () in
        let _v : ((string * Ast.ast_expr option) list) = 
# 153 "parser.mly"
                                         (_1::_3)
# 5026 "parser.ml"
         in
        _menhir_goto_declator_plus _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_stmt) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState244 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Else ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Amp ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Break ->
                _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Const_Char _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _v
            | T_Const_Int _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _v
            | T_Const_Real _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _v
            | T_Const_String _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _v
            | T_Cont ->
                _menhir_run247 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Decr ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Del ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_False ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Id _v ->
                _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _v
            | T_If ->
                _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Lbrace ->
                _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Return ->
                _menhir_run235 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Semicolon ->
                _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | T_For ->
                _menhir_reduce136 _menhir_env (Obj.magic _menhir_stack) MenhirState256
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState256)
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_stmt))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast_stmt) = 
# 196 "parser.mly"
                                                                     (Sif (_3,_5,None))
# 5109 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState256 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_stmt))), _, (_7 : (Ast.ast_stmt))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.ast_stmt) = 
# 197 "parser.mly"
                                                                        (Sif (_3,_5,Some _7))
# 5129 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState267 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (unit) = 
# 142 "parser.mly"
          (ignore(nested_loops := !nested_loops -1))
# 5138 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_12 : (unit)) = _v in
        let ((((((_menhir_stack, _menhir_s, (_1 : (string option))), _, (_4 : (Ast.ast_expr option))), _, (_6 : (Ast.ast_expr option))), _, (_8 : (Ast.ast_expr option))), (_10 : (unit))), _, (_11 : (Ast.ast_stmt))) = _menhir_stack in
        let _9 = () in
        let _7 = () in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ast.ast_stmt) = 
# 198 "parser.mly"
                                                                                                                               (Sfor (_1,_4,_6,_8,_11))
# 5152 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState233 | MenhirState272 | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Amp ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Break ->
            _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Const_Char _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _v
        | T_Const_Int _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _v
        | T_Const_Real _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _v
        | T_Const_String _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _v
        | T_Cont ->
            _menhir_run247 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Decr ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Del ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_False ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Id _v ->
            _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _v
        | T_If ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Lbrace ->
            _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Return ->
            _menhir_run235 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Semicolon ->
            _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_For ->
            _menhir_reduce136 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | T_Rbrace ->
            _menhir_reduce115 _menhir_env (Obj.magic _menhir_stack) MenhirState272
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState272)
    | _ ->
        _menhir_fail ()

and _menhir_goto_declation : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState282 | MenhirState232 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState282
        | T_Char ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState282
        | T_Double ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState282
        | T_Int ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState282
        | T_Void ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState282
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce113 _menhir_env (Obj.magic _menhir_stack) MenhirState282
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState282)
    | MenhirState284 | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | T_Char ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | T_Double ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | T_Int ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | T_Void ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | T_Eof ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ast_decl))) = _menhir_stack in
            let _v : (Ast.ast_program) = 
# 195 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 5265 "parser.ml"
             in
            _menhir_goto_nonempty_list_declation_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState284)
    | _ ->
        _menhir_fail ()

and _menhir_reduce115 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_stmt list) = 
# 185 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 5280 "parser.ml"
     in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce136 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (string option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 5289 "parser.ml"
     in
    _menhir_goto_option_test4_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run234 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_stmt) = 
# 193 "parser.mly"
                       (SExpr None)
# 5301 "parser.ml"
     in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run235 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState235 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState235 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState235 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState235 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState235 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | T_Semicolon ->
        _menhir_reduce128 _menhir_env (Obj.magic _menhir_stack) MenhirState235
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState235

and _menhir_run239 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce123 _menhir_env (Obj.magic _menhir_stack) MenhirState239

and _menhir_run241 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Lparen ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | T_Amp ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | T_Const_Char _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | T_Const_Int _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | T_Const_Real _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | T_Const_String _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | T_Decr ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | T_Del ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | T_False ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | T_Id _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState242
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState242)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run245 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 40 "parser.mly"
       (string)
# 5421 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Colon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (
# 40 "parser.mly"
       (string)
# 5435 "parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (string) = 
# 203 "parser.mly"
                    (_1)
# 5441 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (string)) = _v in
        let _v : (string option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 5449 "parser.ml"
         in
        _menhir_goto_option_test4_ _menhir_env _menhir_stack _menhir_s _v
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Semicolon | T_Sub ->
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run247 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run248 _menhir_env (Obj.magic _menhir_stack) MenhirState247 _v
    | T_Semicolon ->
        _menhir_reduce126 _menhir_env (Obj.magic _menhir_stack) MenhirState247
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState247

and _menhir_run251 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run248 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _v
    | T_Semicolon ->
        _menhir_reduce126 _menhir_env (Obj.magic _menhir_stack) MenhirState251
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState251

and _menhir_reduce113 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_decl list) = 
# 185 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 5498 "parser.ml"
     in
    _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce17 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 40 "parser.mly"
       (string)
# 5505 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 40 "parser.mly"
       (string)
# 5511 "parser.ml"
    ))) = _menhir_stack in
    let _v : (Ast.ast_expr) = 
# 212 "parser.mly"
              (Eid _1)
# 5516 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 40 "parser.mly"
       (string)
# 5523 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Rparen ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState46 in
        let _v : (Ast.ast_expr list option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 5571 "parser.ml"
         in
        _menhir_goto_option_expression_list7_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_goto_expression1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Ast.ast_expr)) = _v in
    let _v : (Ast.ast_expr) = 
# 209 "parser.mly"
                        (ignore(get_type _1);_1)
# 5587 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 224 "parser.mly"
                                          (EUnAdd _2)
# 5611 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Colon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState70 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | T_Amp ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | T_Const_Char _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | T_Const_Int _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | T_Const_Real _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | T_Const_String _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | T_Decr ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | T_Del ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | T_False ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Add | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 231 "parser.mly"
                                     (Eminus (_1,_3))
# 5748 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 227 "parser.mly"
                                     (Emult (_1,_3))
# 5775 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState76 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 256 "parser.mly"
                                                                    (if (get_type _2)=TYPE_int then () else error "Not an int on array";_2)
# 5844 "parser.ml"
             in
            (match _menhir_s with
            | MenhirState243 | MenhirState270 | MenhirState238 | MenhirState214 | MenhirState213 | MenhirState212 | MenhirState208 | MenhirState207 | MenhirState205 | MenhirState204 | MenhirState201 | MenhirState125 | MenhirState124 | MenhirState121 | MenhirState120 | MenhirState119 | MenhirState68 | MenhirState70 | MenhirState118 | MenhirState72 | MenhirState74 | MenhirState76 | MenhirState116 | MenhirState79 | MenhirState106 | MenhirState108 | MenhirState110 | MenhirState112 | MenhirState114 | MenhirState81 | MenhirState104 | MenhirState102 | MenhirState100 | MenhirState98 | MenhirState96 | MenhirState94 | MenhirState83 | MenhirState92 | MenhirState90 | MenhirState85 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_2 : (Ast.ast_expr)) = _v in
                let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
                let _v : (Ast.ast_expr) = 
# 213 "parser.mly"
                                    (EArray (_1,_2))
# 5855 "parser.ml"
                 in
                _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
            | MenhirState195 | MenhirState197 | MenhirState191 | MenhirState190 | MenhirState189 | MenhirState188 | MenhirState187 | MenhirState186 | MenhirState185 | MenhirState139 | MenhirState141 | MenhirState184 | MenhirState154 | MenhirState174 | MenhirState176 | MenhirState178 | MenhirState180 | MenhirState182 | MenhirState156 | MenhirState172 | MenhirState170 | MenhirState168 | MenhirState166 | MenhirState164 | MenhirState162 | MenhirState158 | MenhirState160 | MenhirState143 | MenhirState152 | MenhirState150 | MenhirState145 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_2 : (Ast.ast_expr)) = _v in
                let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
                let _v : (Ast.ast_expr) = 
# 268 "parser.mly"
                                     (EArray (_1,_2))
# 5866 "parser.ml"
                 in
                _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 246 "parser.mly"
                                        (EPlusEq (_1,_3))
# 5935 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 239 "parser.mly"
                                    (Eor (_1,_3))
# 5986 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 237 "parser.mly"
                                     (Eneq (_1,_3))
# 6023 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 229 "parser.mly"
                                     (Emod (_1,_3))
# 6050 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 228 "parser.mly"
                                     (Ediv (_1,_3))
# 6077 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Add | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 230 "parser.mly"
                                     (Eplus (_1,_3))
# 6110 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 233 "parser.mly"
                                     (Elte (_1,_3))
# 6147 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 232 "parser.mly"
                                    (Elt (_1,_3))
# 6184 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 234 "parser.mly"
                                    (Egt (_1,_3))
# 6221 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 235 "parser.mly"
                                     (Egte (_1,_3))
# 6258 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 236 "parser.mly"
                                        (Eeq (_1,_3))
# 6295 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 238 "parser.mly"
                                     (Eand (_1,_3))
# 6344 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 250 "parser.mly"
                                        (EModEq (_1,_3))
# 6409 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 247 "parser.mly"
                                          (EMinusEq (_1,_3))
# 6474 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 245 "parser.mly"
                                    (EAssignEq (_1,_3))
# 6539 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 248 "parser.mly"
                                        (EDotEq (_1,_3))
# 6604 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 249 "parser.mly"
                                        (EDivEq (_1,_3))
# 6669 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 240 "parser.mly"
                                       (Ecomma (_1,_3))
# 6734 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 252 "parser.mly"
                                                                              (EQuestT (_1,_3,_5))
# 6788 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 222 "parser.mly"
                          (EAmber _2)
# 6815 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 242 "parser.mly"
                                                 (EMinusMinus (_2,PRE))
# 6840 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 254 "parser.mly"
                          (EDel _2)
# 6867 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))), _, (_4 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 305 "parser.mly"
                                                         (ECast (_2,_4))
# 6895 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState125 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 269 "parser.mly"
                                        (_2)
# 6964 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 241 "parser.mly"
                                                 (EPlusPlus (_2,PRE))
# 6991 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState201)
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))), _, (_4 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 251 "parser.mly"
                                                         (ECast (_2,_4))
# 7019 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState204)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState205 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 214 "parser.mly"
                                        (_2)
# 7088 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 223 "parser.mly"
                                          (EPointer _2)
# 7117 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState207)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Rbracket ->
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState208)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 226 "parser.mly"
                                          (Enot _2)
# 7203 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState212)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState213
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState213
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState213
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState213
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 225 "parser.mly"
                                          (EUnMinus _2)
# 7230 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState213)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
            let _v : (Ast.ast_expr) = 
# 262 "parser.mly"
                               (_1)
# 7296 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Rbracket ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, (_2 : (Ast.ast_expr))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (Ast.ast_expr) = 
# 165 "parser.mly"
                                                (_2)
# 7313 "parser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (x : (Ast.ast_expr)) = _v in
                let _v : (Ast.ast_expr option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 7321 "parser.ml"
                 in
                _menhir_goto_option_test_ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState214)
    | MenhirState260 | MenhirState262 | MenhirState264 | MenhirState235 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ast_expr))) = _menhir_stack in
            let _v : (Ast.ast_expr option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 7393 "parser.ml"
             in
            _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState238)
    | MenhirState242 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState243 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Amp ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Break ->
                _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Const_Char _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
            | T_Const_Int _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
            | T_Const_Real _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
            | T_Const_String _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
            | T_Cont ->
                _menhir_run247 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Decr ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Del ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_False ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Id _v ->
                _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
            | T_If ->
                _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Lbrace ->
                _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Return ->
                _menhir_run235 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Semicolon ->
                _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | T_For ->
                _menhir_reduce136 _menhir_env (Obj.magic _menhir_stack) MenhirState244
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState244)
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState243)
    | MenhirState233 | MenhirState240 | MenhirState272 | MenhirState244 | MenhirState256 | MenhirState267 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Decr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Div ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Incr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Lbracket ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Mod ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Neq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Or ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_PlusEq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Quest ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState270 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_stmt) = 
# 194 "parser.mly"
                                 (SExpr (Some _1))
# 7579 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState270)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_test8_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (Ast.ast_expr option)) = _v in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.ast_expr) = 
# 307 "parser.mly"
                              (if Option.is_some _3 then ENew (_2,(Option.get _3)) else ENew (_2,Eint(1)))
# 7603 "parser.ml"
         in
        _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (Ast.ast_expr option)) = _v in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.ast_expr) = 
# 253 "parser.mly"
                              (if Option.is_some _3 then ENew (_2,(Option.get _3)) else ENew (_2,Eint(1)))
# 7615 "parser.ml"
         in
        _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_test_ : _menhir_env -> 'ttv_tail -> (Ast.ast_expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_2 : (Ast.ast_expr option)) = _v in
    let (_menhir_stack, _menhir_s, (_1 : (
# 40 "parser.mly"
       (string)
# 7629 "parser.ml"
    ))) = _menhir_stack in
    let _v : (string * Ast.ast_expr option) = 
# 164 "parser.mly"
                             ( (_1,_2))
# 7634 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Comma ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Id _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState222 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Lbracket ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
            | T_Comma | T_Semicolon ->
                _menhir_reduce134 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState222)
    | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (string * Ast.ast_expr option))) = _menhir_stack in
        let _2 = () in
        let _v : ((string * Ast.ast_expr option) list) = 
# 152 "parser.mly"
                                    ([_1])
# 7676 "parser.ml"
         in
        _menhir_goto_declator_plus _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_test2_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Symbol.pass_mode * Types.typ * string) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Symbol.pass_mode * Types.typ * string) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Symbol.pass_mode * Types.typ * string))) = _menhir_stack in
        let _v : ((Symbol.pass_mode * Types.typ * string) list) = 
# 187 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 7697 "parser.ml"
         in
        _menhir_goto_list_test2_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : ((Symbol.pass_mode * Types.typ * string) list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Symbol.pass_mode * Types.typ * string))) = _menhir_stack in
        let _v : ((Symbol.pass_mode * Types.typ * string) list) = 
# 183 "parser.mly"
                                         ([_1] @_2)
# 7708 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((Symbol.pass_mode * Types.typ * string) list)) = _v in
        let _v : ((Symbol.pass_mode * Types.typ * string) list option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 7716 "parser.ml"
         in
        _menhir_goto_option_parameter_list_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) = 
# 137 "parser.mly"
          (ignore(closeScope();))
# 7727 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (unit))), _, (_3 : (Ast.ast_expr))), _, (_4 : (unit))) = _menhir_stack in
            let _5 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 255 "parser.mly"
                                                       (_3)
# 7746 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (Ast.ast_expr)) = _v in
            let _v : (Ast.ast_expr option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 7754 "parser.ml"
             in
            _menhir_goto_option_test8_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Symbol.entry))), _, (_3 : (unit))) = _menhir_stack in
        let _2 = () in
        let _v : (Symbol.entry) = 
# 167 "parser.mly"
                                                            (_1)
# 7771 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Symbol.entry)) = _v in
        let _v : (Ast.ast_decl) = 
# 146 "parser.mly"
                            (FunDecl _1)
# 7779 "parser.ml"
         in
        _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
    | MenhirState274 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rbrace ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (unit))), _, (_3 : (Ast.ast_stmt list))), _, (_4 : (unit))) = _menhir_stack in
            let _5 = () in
            let _1 = () in
            let _v : (Ast.ast_stmt) = 
# 195 "parser.mly"
                                                      (SNewblock _3)
# 7797 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState277 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rbrace ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit) = 
# 138 "parser.mly"
           (ignore(closeScope2();))
# 7818 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_8 : (unit)) = _v in
            let (((((_menhir_stack, _menhir_s, (_1 : (Symbol.entry))), _, (_3 : (unit))), _, (_4 : (Ast.ast_decl list))), _, (_5 : (Ast.ast_stmt list))), _, (_6 : (unit))) = _menhir_stack in
            let _7 = () in
            let _2 = () in
            let _v : (Ast.ast_fun_def) = 
# 188 "parser.mly"
                                                                                                (
        
        
        (_1,_4,_5))
# 7832 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.ast_fun_def)) = _v in
            let _v : (Ast.ast_decl) = 
# 144 "parser.mly"
                        (FunDef _1)
# 7840 "parser.ml"
             in
            _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce123 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) = 
# 136 "parser.mly"
          (ignore(openScope();))
# 7857 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Amp ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Const_Char _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | T_Const_Int _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | T_Const_Real _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | T_Const_String _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | T_Decr ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Del ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_False ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Id _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState231 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | T_Char ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | T_Double ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | T_Int ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | T_Void ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce113 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState232)
    | MenhirState239 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Amp ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Break ->
            _menhir_run251 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Const_Char _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | T_Const_Int _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | T_Const_Real _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | T_Const_String _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | T_Cont ->
            _menhir_run247 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Decr ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Del ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_False ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Id _v ->
            _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | T_If ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Lbrace ->
            _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Return ->
            _menhir_run235 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Semicolon ->
            _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_For ->
            _menhir_reduce136 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Rbrace ->
            _menhir_reduce115 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState240)
    | _ ->
        _menhir_fail ()

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) = 
# 215 "parser.mly"
                (Ebool true)
# 7999 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) = 
# 217 "parser.mly"
                (ENull)
# 8058 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Bool ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Char ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Double ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Int ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Bool ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Char ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Double ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Int ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 40 "parser.mly"
       (string)
# 8280 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) = 
# 216 "parser.mly"
                 (Ebool false)
# 8306 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 44 "parser.mly"
       (string)
# 8407 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 44 "parser.mly"
       (string)
# 8415 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 221 "parser.mly"
                        (Estring _1)
# 8420 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 42 "parser.mly"
       (float)
# 8427 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 42 "parser.mly"
       (float)
# 8435 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 220 "parser.mly"
                      (Ereal _1)
# 8440 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 41 "parser.mly"
       (int)
# 8447 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 41 "parser.mly"
       (int)
# 8455 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 219 "parser.mly"
                     (Eint _1)
# 8460 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 43 "parser.mly"
       (char)
# 8467 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 43 "parser.mly"
       (char)
# 8475 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 218 "parser.mly"
                      (Echar _1)
# 8480 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_reduce138 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 8583 "parser.ml"
     in
    _menhir_goto_option_test8_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce123 _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_reduce134 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _v : (Ast.ast_expr option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 8598 "parser.ml"
     in
    _menhir_goto_option_test_ _menhir_env _menhir_stack _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_reduce117 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Symbol.pass_mode * Types.typ * string) list) = 
# 185 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 8653 "parser.ml"
     in
    _menhir_goto_list_test2_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Byref ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | T_Bool | T_Char | T_Double | T_Int ->
        _menhir_reduce124 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_function_declation1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Symbol.entry) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Lbrace ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce123 _menhir_env (Obj.magic _menhir_stack) MenhirState231
    | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState229
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_T_Mul_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (unit list)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let x = () in
        let _v : (unit list) = 
# 187 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 8711 "parser.ml"
         in
        _menhir_goto_list_T_Mul_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (unit list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Types.typ))) = _menhir_stack in
        let _v : (Types.typ) = 
# 156 "parser.mly"
                          (if _2 = [] then _1 else List.fold_left (fun x->fun y-> TYPE_pointer x ) _1 _2)
# 8722 "parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState11 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Id _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_3 : (
# 40 "parser.mly"
       (string)
# 8738 "parser.ml"
                )) = _v in
                let ((_menhir_stack, _menhir_s, (_1 : (unit option))), _, (_2 : (Types.typ))) = _menhir_stack in
                let _v : (Symbol.pass_mode * Types.typ * string) = 
# 186 "parser.mly"
                                (if is_some _1 then (PASS_BY_REFERENCE,_2,_3) else (PASS_BY_VALUE ,_2,_3))
# 8744 "parser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                (match _menhir_s with
                | MenhirState30 | MenhirState5 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | T_Comma ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
                    | T_Rparen ->
                        _menhir_reduce117 _menhir_env (Obj.magic _menhir_stack) MenhirState8
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
                | MenhirState9 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _, (_2 : (Symbol.pass_mode * Types.typ * string))) = _menhir_stack in
                    let _1 = () in
                    let _v : (Symbol.pass_mode * Types.typ * string) = 
# 184 "parser.mly"
                         (_2)
# 8769 "parser.ml"
                     in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | T_Comma ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | T_Rparen ->
                        _menhir_reduce117 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
                | _ ->
                    _menhir_fail ())
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState284 | MenhirState282 | MenhirState232 | MenhirState2 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Id _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState28 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Lbracket ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
                | T_Lparen ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | T_Byref ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                    | T_Rparen ->
                        _menhir_reduce132 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                    | T_Bool | T_Char | T_Double | T_Int ->
                        _menhir_reduce124 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
                | T_Comma | T_Semicolon ->
                    _menhir_reduce134 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
        | MenhirState38 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Lbracket ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
                _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack) MenhirState39
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
        | MenhirState51 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Lbracket ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
                _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
        | MenhirState56 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Rparen ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Add ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_Amp ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_Const_Char _v ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | T_Const_Int _v ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | T_Const_Real _v ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | T_Const_String _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | T_Decr ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_Del ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_False ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_Id _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | T_Incr ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_Lparen ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_Mul ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_New ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_Not ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_Null ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_Sub ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_True ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState43 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Rparen ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Add ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | T_Amp ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | T_Const_Char _v ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
                | T_Const_Int _v ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
                | T_Const_Real _v ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
                | T_Const_String _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
                | T_Decr ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | T_Del ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | T_False ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | T_Id _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
                | T_Incr ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | T_Lparen ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | T_Mul ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | T_New ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | T_Not ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | T_Null ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | T_Sub ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | T_True ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_parameter_list_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Symbol.pass_mode * Types.typ * string) list option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (_2 : (
# 40 "parser.mly"
       (string)
# 8992 "parser.ml"
            ))), _, (_4 : ((Symbol.pass_mode * Types.typ * string) list option))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Symbol.entry) = 
# 175 "parser.mly"
                                                        (
        let e= newFunction (id_make _2) true in 
        let _ = openScope() in
                 may (List.iter (fun x-> ignore (newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) _4 ;  
                 endFunctionHeader e TYPE_none; 
                 e )
# 9005 "parser.ml"
             in
            _menhir_goto_function_declation1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Types.typ))), _, (_2 : (
# 40 "parser.mly"
       (string)
# 9026 "parser.ml"
            ))), _, (_4 : ((Symbol.pass_mode * Types.typ * string) list option))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : (Symbol.entry) = 
# 169 "parser.mly"
                                                     (
        let e= newFunction (id_make _2) true in  
        let _ = openScope() in
                may (List.iter (fun x-> ignore(newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) _4 ;  
                endFunctionHeader e _1 ; 
                e)
# 9038 "parser.ml"
             in
            _menhir_goto_function_declation1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_T_Byref_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Bool ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | T_Char ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | T_Double ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | T_Int ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_reduce111 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) = 
# 185 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 9075 "parser.ml"
     in
    _menhir_goto_list_T_Mul_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Id _ | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce111 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_reduce124 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 9099 "parser.ml"
     in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce132 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Symbol.pass_mode * Types.typ * string) list option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 9108 "parser.ml"
     in
    _menhir_goto_option_parameter_list_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 9120 "parser.ml"
     in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_basic_type : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Id _ | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce111 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState284 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState282 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState277 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState274 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState272 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState270 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState267 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState264 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState262 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState260 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState256 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState251 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState247 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState244 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState243 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState242 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState239 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState238 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState235 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState232 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState231 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState222 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState214 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState204 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState201 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState189 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Lparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Byref ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | T_Rparen ->
                _menhir_reduce132 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | T_Bool | T_Char | T_Double | T_Int ->
                _menhir_reduce124 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) = 
# 158 "parser.mly"
                   (TYPE_int)
# 9898 "parser.ml"
     in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) = 
# 161 "parser.mly"
                   (TYPE_double)
# 9910 "parser.ml"
     in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) = 
# 159 "parser.mly"
                 (TYPE_char)
# 9922 "parser.ml"
     in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) = 
# 160 "parser.mly"
                 (TYPE_bool)
# 9934 "parser.ml"
     in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 99 "parser.mly"
     (unit)
# 9953 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) = 
# 131 "parser.mly"
                (ignore(initSymbolTable 256); ignore (openScope ());)
# 9969 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Bool ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | T_Char ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | T_Double ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | T_Int ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | T_Void ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)

# 219 "/home/zinc/.opam/system/lib/menhir/standard.mly"
  


# 9995 "parser.ml"
