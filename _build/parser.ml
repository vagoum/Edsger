
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
# 39 "parser.mly"
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
# 43 "parser.mly"
       (string)
# 58 "parser.ml"
  )
    | T_Const_Real of (
# 41 "parser.mly"
       (float)
# 63 "parser.ml"
  )
    | T_Const_Int of (
# 40 "parser.mly"
       (int)
# 68 "parser.ml"
  )
    | T_Const_Char of (
# 42 "parser.mly"
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
  | MenhirState285
  | MenhirState283
  | MenhirState280
  | MenhirState275
  | MenhirState272
  | MenhirState270
  | MenhirState268
  | MenhirState265
  | MenhirState262
  | MenhirState260
  | MenhirState258
  | MenhirState254
  | MenhirState249
  | MenhirState245
  | MenhirState242
  | MenhirState241
  | MenhirState240
  | MenhirState238
  | MenhirState237
  | MenhirState236
  | MenhirState233
  | MenhirState231
  | MenhirState230
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
  | MenhirState194
  | MenhirState193
  | MenhirState192
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
  | MenhirState149
  | MenhirState148
  | MenhirState147
  | MenhirState146
  | MenhirState145
  | MenhirState144
  | MenhirState143
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
  | MenhirState88
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
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


        let get_first (x,_,_) =x ;;
        let get_first2 (x,_) =x ;;
        let get_second (_,x,_)=x;;
        let get_second2 (_,x)=x;;
        let get_third (_,_,x)=x;;

# 305 "parser.ml"

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
# 318 "parser.ml"
         in
        _menhir_goto_list_test97_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Ast.ast_expr list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
        let _v : (Ast.ast_expr list) = 
# 271 "parser.mly"
                                      (check_expr _1;[_1] @ _2)
# 329 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.ast_expr list)) = _v in
        let _v : (Ast.ast_expr list option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 337 "parser.ml"
         in
        _menhir_goto_option_expression_list7_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_T_Id_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState245 ->
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
# 212 "parser.mly"
                                   (if !nested_loops =0 then (error "No continue in Loop"; SCont _2) else SCont _2)
# 362 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState249 ->
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
# 213 "parser.mly"
                                   (if !nested_loops = 0 then (error "No break in loop" ; SBreak _2)  else SBreak _2)
# 386 "parser.ml"
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

and _menhir_reduce121 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr list) = 
# 185 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 403 "parser.ml"
     in
    _menhir_goto_list_test97_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run154 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154

and _menhir_run162 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162

and _menhir_run164 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164

and _menhir_run166 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166

and _menhir_run168 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168

and _menhir_run156 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156

and _menhir_run184 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184

and _menhir_run158 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158

and _menhir_run186 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186

and _menhir_run172 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172

and _menhir_run174 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174

and _menhir_run176 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176

and _menhir_run178 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178

and _menhir_run180 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180

and _menhir_run188 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState188
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188

and _menhir_run190 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190

and _menhir_run192 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192

and _menhir_run160 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160

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

and _menhir_run182 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182

and _menhir_run170 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | T_Amp ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | T_Const_Char _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
    | T_Const_Int _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
    | T_Const_Real _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
    | T_Const_String _v ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
    | T_Decr ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | T_Del ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | T_False ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | T_Id _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
    | T_Incr ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | T_Lparen ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | T_Mul ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | T_New ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | T_Not ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | T_Null ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | T_Sub ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | T_True ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170

and _menhir_run140 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) = 
# 308 "parser.mly"
                                                 (EPlusPlus (_1,AFTER))
# 1403 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run141 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) = 
# 309 "parser.mly"
                                                 (EMinusMinus (_1,AFTER))
# 1416 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_nonempty_list_declation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_decl list) -> 'ttv_return =
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
            let ((_menhir_stack, (_1 : (unit))), _, (_2 : (Ast.ast_decl list))) = _menhir_stack in
            let _3 = () in
            let _v : (
# 98 "parser.mly"
     (unit)
# 1437 "parser.ml"
            ) = 
# 129 "parser.mly"
                                         ( (*ignore(is_main());*) ast_tree := (_2@(!ast_tree));check (Some _2);)
# 1441 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 98 "parser.mly"
     (unit)
# 1448 "parser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState285 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_decl))), _, (xs : (Ast.ast_decl list))) = _menhir_stack in
        let _v : (Ast.ast_decl list) = 
# 197 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1464 "parser.ml"
         in
        _menhir_goto_nonempty_list_declation_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_statement_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_stmt list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState270 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_stmt))), _, (xs : (Ast.ast_stmt list))) = _menhir_stack in
        let _v : (Ast.ast_stmt list) = 
# 187 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1481 "parser.ml"
         in
        _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState238 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState272
    | MenhirState231 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState275
    | _ ->
        _menhir_fail ()

and _menhir_reduce130 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 1498 "parser.ml"
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
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_Amp ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_Const_Char _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | T_Const_Int _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | T_Const_Real _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | T_Const_String _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | T_Decr ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_Del ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_False ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | T_Semicolon ->
                _menhir_reduce130 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState258)
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

and _menhir_reduce128 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (string option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 1579 "parser.ml"
     in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run246 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 39 "parser.mly"
       (string)
# 1586 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 39 "parser.mly"
       (string)
# 1594 "parser.ml"
    )) = _v in
    let _v : (string option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 1599 "parser.ml"
     in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_declation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState230 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Amp ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Break ->
            _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Const_Char _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _v
        | T_Const_Int _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _v
        | T_Const_Real _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _v
        | T_Const_String _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _v
        | T_Cont ->
            _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Decr ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Del ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_False ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Id _v ->
            _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _v
        | T_If ->
            _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Lbrace ->
            _menhir_run237 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Return ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Semicolon ->
            _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_For ->
            _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | T_Rbrace ->
            _menhir_reduce117 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState231)
    | MenhirState283 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_decl))), _, (xs : (Ast.ast_decl list))) = _menhir_stack in
        let _v : (Ast.ast_decl list) = 
# 187 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1675 "parser.ml"
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
# 39 "parser.mly"
       (string)
# 1695 "parser.ml"
        ))), _, (_3 : (Ast.ast_expr list option))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (Ast.ast_expr) = 
# 217 "parser.mly"
                                                                          ( let k = if is_some _3 then get_some1 _3 else [] in 
(match (check_name_lib _1) with 
| true ->()
|false -> ignore(check_function_call (lookupEntry (id_make _1) LOOKUP_ALL_SCOPES true) k) 
); ECall (_1,_3))
# 1706 "parser.ml"
         in
        (match _menhir_s with
        | MenhirState231 | MenhirState238 | MenhirState270 | MenhirState242 | MenhirState254 | MenhirState258 | MenhirState260 | MenhirState262 | MenhirState265 | MenhirState240 | MenhirState233 | MenhirState33 | MenhirState35 | MenhirState37 | MenhirState41 | MenhirState42 | MenhirState43 | MenhirState203 | MenhirState44 | MenhirState56 | MenhirState123 | MenhirState58 | MenhirState59 | MenhirState64 | MenhirState117 | MenhirState115 | MenhirState113 | MenhirState111 | MenhirState109 | MenhirState107 | MenhirState105 | MenhirState103 | MenhirState101 | MenhirState99 | MenhirState97 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState83 | MenhirState80 | MenhirState78 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState65 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.ast_expr)) = _v in
            let _v : (Ast.ast_expr) = 
# 224 "parser.mly"
                      (_1)
# 1716 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | MenhirState196 | MenhirState194 | MenhirState192 | MenhirState190 | MenhirState188 | MenhirState186 | MenhirState184 | MenhirState182 | MenhirState180 | MenhirState178 | MenhirState176 | MenhirState174 | MenhirState172 | MenhirState170 | MenhirState168 | MenhirState166 | MenhirState164 | MenhirState162 | MenhirState160 | MenhirState158 | MenhirState156 | MenhirState154 | MenhirState46 | MenhirState48 | MenhirState50 | MenhirState55 | MenhirState127 | MenhirState130 | MenhirState131 | MenhirState136 | MenhirState137 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.ast_expr)) = _v in
            let _v : (Ast.ast_expr) = 
# 277 "parser.mly"
                      (_1)
# 1726 "parser.ml"
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
# 281 "parser.mly"
                (Ebool true)
# 1746 "parser.ml"
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
# 283 "parser.mly"
                (ENull)
# 1805 "parser.ml"
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
# 39 "parser.mly"
       (string)
# 2027 "parser.ml"
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
# 39 "parser.mly"
       (string)
# 2041 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.ast_expr) = 
# 278 "parser.mly"
              (Eid _1)
# 2046 "parser.ml"
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
# 282 "parser.mly"
                 (Ebool false)
# 2064 "parser.ml"
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
# 43 "parser.mly"
       (string)
# 2165 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 43 "parser.mly"
       (string)
# 2173 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 287 "parser.mly"
                        (Estring _1)
# 2178 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run133 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 41 "parser.mly"
       (float)
# 2185 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 41 "parser.mly"
       (float)
# 2193 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 286 "parser.mly"
                      (Ereal _1)
# 2198 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run134 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 40 "parser.mly"
       (int)
# 2205 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 40 "parser.mly"
       (int)
# 2213 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 285 "parser.mly"
                     (Eint _1)
# 2218 "parser.ml"
     in
    _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 42 "parser.mly"
       (char)
# 2225 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 42 "parser.mly"
       (char)
# 2233 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 284 "parser.mly"
                      (Echar _1)
# 2238 "parser.ml"
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
    | MenhirState233 ->
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
# 214 "parser.mly"
                                          (Sreturn _2)
# 2355 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState258 ->
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
                _menhir_reduce130 _menhir_env (Obj.magic _menhir_stack) MenhirState260
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
            | T_Rparen ->
                _menhir_reduce130 _menhir_env (Obj.magic _menhir_stack) MenhirState262
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
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit) = 
# 140 "parser.mly"
         (ignore(nested_loops := !nested_loops +1))
# 2492 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Amp ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Break ->
                _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Const_Char _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState265 _v
            | T_Const_Int _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState265 _v
            | T_Const_Real _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState265 _v
            | T_Const_String _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState265 _v
            | T_Cont ->
                _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Decr ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Del ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_False ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Id _v ->
                _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState265 _v
            | T_If ->
                _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Lbrace ->
                _menhir_run237 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Return ->
                _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Semicolon ->
                _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | T_For ->
                _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack) MenhirState265
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState265)
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

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

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

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) = 
# 256 "parser.mly"
                                                (EPlusPlus (_1,AFTER))
# 3605 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) = 
# 257 "parser.mly"
                                                (EMinusMinus (_1,AFTER))
# 3618 "parser.ml"
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
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 290 "parser.mly"
                                           (EUnAdd _2)
# 3644 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 288 "parser.mly"
                           (EAmber _2)
# 3669 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 307 "parser.mly"
                                                  (EMinusMinus (_2,PRE))
# 3694 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 319 "parser.mly"
                           (EDel _2)
# 3719 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 306 "parser.mly"
                                                  (EPlusPlus (_2,PRE))
# 3744 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 289 "parser.mly"
                                           (EPointer _2)
# 3769 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 292 "parser.mly"
                                           (Enot _2)
# 3794 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 291 "parser.mly"
                                           (EUnMinus _2)
# 3819 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_And ->
            _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Comma ->
            _menhir_run196 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Div_eq ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Dot_eq ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Eq ->
            _menhir_run188 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Equal ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Geq ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Gr ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Le ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Leq ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Minus_eq ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Mod_eq ->
            _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Neq ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Or ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_PlusEq ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Quest ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Rparen ->
            _menhir_reduce121 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | T_Add | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 297 "parser.mly"
                                       (Eminus (_1,_3))
# 3909 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 293 "parser.mly"
                                       (Emult (_1,_3))
# 3934 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 295 "parser.mly"
                                       (Emod (_1,_3))
# 3959 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159)
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 294 "parser.mly"
                                       (Ediv (_1,_3))
# 3984 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_And ->
            _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Colon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState163 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | T_Amp ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | T_Const_Char _v ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
            | T_Const_Int _v ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
            | T_Const_Real _v ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
            | T_Const_String _v ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
            | T_Decr ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | T_Del ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | T_False ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | T_Id _v ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
            | T_Incr ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | T_Lparen ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | T_Mul ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | T_New ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | T_Not ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | T_Null ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | T_Sub ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | T_True ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194)
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Div_eq ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Dot_eq ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Eq ->
            _menhir_run188 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Equal ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Geq ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Gr ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Le ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Leq ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Minus_eq ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Mod_eq ->
            _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Neq ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Or ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_PlusEq ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Quest ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_And ->
            _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Div_eq ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Dot_eq ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Eq ->
            _menhir_run188 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Equal ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Geq ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Gr ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Le ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Leq ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Minus_eq ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Mod_eq ->
            _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Neq ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Or ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_PlusEq ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Quest ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Colon | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 311 "parser.mly"
                                          (EPlusEq (_1,_3))
# 4151 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165)
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_And ->
            _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Equal ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Geq ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Gr ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Le ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Leq ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Neq ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 305 "parser.mly"
                                      (Eor (_1,_3))
# 4200 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState167)
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 303 "parser.mly"
                                       (Eneq (_1,_3))
# 4235 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169)
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState171
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState171
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState171
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState171
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState171
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState171
        | T_Add | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 296 "parser.mly"
                                       (Eplus (_1,_3))
# 4266 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171)
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 299 "parser.mly"
                                       (Elte (_1,_3))
# 4301 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173)
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 298 "parser.mly"
                                      (Elt (_1,_3))
# 4336 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175)
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 300 "parser.mly"
                                      (Egt (_1,_3))
# 4371 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177)
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState179
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState179
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState179
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState179
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState179
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState179
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState179
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState179
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 301 "parser.mly"
                                       (Egte (_1,_3))
# 4406 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179)
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 302 "parser.mly"
                                          (Eeq (_1,_3))
# 4441 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181)
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Equal ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Geq ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Gr ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Le ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Leq ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Neq ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 304 "parser.mly"
                                       (Eand (_1,_3))
# 4488 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_And ->
            _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Div_eq ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Dot_eq ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Eq ->
            _menhir_run188 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Equal ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Geq ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Gr ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Le ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Leq ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Minus_eq ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Mod_eq ->
            _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Neq ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Or ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_PlusEq ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Quest ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | T_Colon | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 315 "parser.mly"
                                          (EModEq (_1,_3))
# 4553 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185)
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_And ->
            _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Div_eq ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Dot_eq ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Eq ->
            _menhir_run188 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Equal ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Geq ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Gr ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Le ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Leq ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Minus_eq ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Mod_eq ->
            _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Neq ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Or ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_PlusEq ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Quest ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | T_Colon | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 312 "parser.mly"
                                            (EMinusEq (_1,_3))
# 4618 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_And ->
            _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Div_eq ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Dot_eq ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Eq ->
            _menhir_run188 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Equal ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Geq ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Gr ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Le ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Leq ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Minus_eq ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Mod_eq ->
            _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Neq ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Or ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_PlusEq ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Quest ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Colon | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 310 "parser.mly"
                                      (EAssignEq (_1,_3))
# 4683 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189)
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_And ->
            _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Div_eq ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Dot_eq ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Eq ->
            _menhir_run188 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Equal ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Geq ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Gr ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Le ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Leq ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Minus_eq ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Mod_eq ->
            _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Neq ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Or ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_PlusEq ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Quest ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Colon | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 313 "parser.mly"
                                          (EDotEq (_1,_3))
# 4748 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191)
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_And ->
            _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Div_eq ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Dot_eq ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Eq ->
            _menhir_run188 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Equal ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Geq ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Gr ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Le ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Leq ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Minus_eq ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Mod_eq ->
            _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Neq ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Or ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_PlusEq ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Quest ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Colon | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 314 "parser.mly"
                                          (EDivEq (_1,_3))
# 4813 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState193)
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_And ->
            _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Equal ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Geq ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Gr ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Le ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Leq ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Neq ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Or ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Quest ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_PlusEq | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 317 "parser.mly"
                                                             (EQuestT (_1,_3,_5))
# 4867 "parser.ml"
             in
            _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_And ->
            _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Decr ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Div ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Div_eq ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Dot_eq ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Eq ->
            _menhir_run188 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Equal ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Geq ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Gr ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Incr ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Le ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Leq ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Minus_eq ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Mod ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Mod_eq ->
            _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Mul ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Neq ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Or ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_PlusEq ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Quest ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Sub ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | T_Comma | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 272 "parser.mly"
                            (check_expr _2 ;_2)
# 4932 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Comma ->
                _menhir_run196 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | T_Rparen ->
                _menhir_reduce121 _menhir_env (Obj.magic _menhir_stack) MenhirState198
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
# 147 "parser.mly"
                                                 (List.map (fun x -> 
                let typeA = if is_some (get_second2(x)) then (ignore(check_array_len (get_second2(x))); TYPE_array (_1,evaluate_constant (get (get_second2(x)))) ) else _1 in (* later 0-> lenth,doesnt needed for semantics yet*)
                newVariable (id_make (get_first2 x)) typeA true) _2)
# 4967 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.ast_var_decl)) = _v in
        let _v : (Ast.ast_decl) = 
# 144 "parser.mly"
                             (VarDecl _1)
# 4975 "parser.ml"
         in
        _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
    | MenhirState222 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : ((string * Ast.ast_expr option) list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (string * Ast.ast_expr option))) = _menhir_stack in
        let _2 = () in
        let _v : ((string * Ast.ast_expr option) list) = 
# 152 "parser.mly"
                                         (_1::_3)
# 4987 "parser.ml"
         in
        _menhir_goto_declator_plus _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_declation : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState283 | MenhirState230 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | T_Char ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | T_Double ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | T_Int ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | T_Void ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce115 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState283)
    | MenhirState285 | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState285
        | T_Char ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState285
        | T_Double ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState285
        | T_Int ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState285
        | T_Void ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState285
        | T_Eof ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ast_decl))) = _menhir_stack in
            let _v : (Ast.ast_decl list) = 
# 195 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 5039 "parser.ml"
             in
            _menhir_goto_nonempty_list_declation_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState285)
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_stmt) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState242 ->
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
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Amp ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Break ->
                _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Const_Char _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
            | T_Const_Int _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
            | T_Const_Real _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
            | T_Const_String _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
            | T_Cont ->
                _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Decr ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Del ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_False ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Id _v ->
                _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
            | T_If ->
                _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Lbrace ->
                _menhir_run237 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Return ->
                _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Semicolon ->
                _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | T_For ->
                _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack) MenhirState254
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState254)
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_stmt))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast_stmt) = 
# 209 "parser.mly"
                                                                     (Sif (_3,_5,None))
# 5126 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState254 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_stmt))), _, (_7 : (Ast.ast_stmt))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.ast_stmt) = 
# 210 "parser.mly"
                                                                        (Sif (_3,_5,Some _7))
# 5146 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState265 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (unit) = 
# 141 "parser.mly"
          (ignore(nested_loops := !nested_loops -1))
# 5155 "parser.ml"
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
# 211 "parser.mly"
                                                                                                                               (Sfor (_1,_4,_6,_8,_11))
# 5169 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState231 | MenhirState270 | MenhirState238 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Amp ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Break ->
            _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Const_Char _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState270 _v
        | T_Const_Int _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState270 _v
        | T_Const_Real _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState270 _v
        | T_Const_String _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState270 _v
        | T_Cont ->
            _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Decr ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Del ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_False ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Id _v ->
            _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState270 _v
        | T_If ->
            _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Lbrace ->
            _menhir_run237 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Return ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Semicolon ->
            _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_For ->
            _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | T_Rbrace ->
            _menhir_reduce117 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState270)
    | _ ->
        _menhir_fail ()

and _menhir_reduce117 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_stmt list) = 
# 185 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 5241 "parser.ml"
     in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce138 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (string option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 5250 "parser.ml"
     in
    _menhir_goto_option_test4_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run232 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_stmt) = 
# 206 "parser.mly"
                       (SExpr None)
# 5262 "parser.ml"
     in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run233 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState233
    | T_Amp ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState233
    | T_Const_Char _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
    | T_Const_Int _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
    | T_Const_Real _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
    | T_Const_String _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
    | T_Decr ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState233
    | T_Del ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState233
    | T_False ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState233
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState233
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
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState233
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState233
    | T_Semicolon ->
        _menhir_reduce130 _menhir_env (Obj.magic _menhir_stack) MenhirState233
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState233

and _menhir_run237 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce125 _menhir_env (Obj.magic _menhir_stack) MenhirState237

and _menhir_run239 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Amp ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Const_Char _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | T_Const_Int _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | T_Const_Real _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | T_Const_String _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | T_Decr ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Del ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_False ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_Id _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState240
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
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState240)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run243 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 39 "parser.mly"
       (string)
# 5382 "parser.ml"
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
# 39 "parser.mly"
       (string)
# 5396 "parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (string) = 
# 216 "parser.mly"
                    (_1)
# 5402 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (string)) = _v in
        let _v : (string option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 5410 "parser.ml"
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

and _menhir_run245 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run246 _menhir_env (Obj.magic _menhir_stack) MenhirState245 _v
    | T_Semicolon ->
        _menhir_reduce128 _menhir_env (Obj.magic _menhir_stack) MenhirState245
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState245

and _menhir_run249 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run246 _menhir_env (Obj.magic _menhir_stack) MenhirState249 _v
    | T_Semicolon ->
        _menhir_reduce128 _menhir_env (Obj.magic _menhir_stack) MenhirState249
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState249

and _menhir_reduce115 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_decl list) = 
# 185 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 5459 "parser.ml"
     in
    _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce17 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 39 "parser.mly"
       (string)
# 5466 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 39 "parser.mly"
       (string)
# 5472 "parser.ml"
    ))) = _menhir_stack in
    let _v : (Ast.ast_expr) = 
# 225 "parser.mly"
              (Eid _1)
# 5477 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 39 "parser.mly"
       (string)
# 5484 "parser.ml"
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
# 5532 "parser.ml"
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
# 222 "parser.mly"
                        (ignore(get_type _1);_1)
# 5548 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 237 "parser.mly"
                                          (EUnAdd _2)
# 5570 "parser.ml"
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
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Decr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState70
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
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState70 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 269 "parser.mly"
                                                                    (if (get_type _2)=TYPE_int then () else error "Not an int on array";_2)
# 5639 "parser.ml"
             in
            (match _menhir_s with
            | MenhirState241 | MenhirState268 | MenhirState236 | MenhirState214 | MenhirState213 | MenhirState212 | MenhirState208 | MenhirState207 | MenhirState205 | MenhirState204 | MenhirState201 | MenhirState125 | MenhirState124 | MenhirState121 | MenhirState120 | MenhirState119 | MenhirState68 | MenhirState70 | MenhirState84 | MenhirState118 | MenhirState116 | MenhirState86 | MenhirState106 | MenhirState108 | MenhirState110 | MenhirState112 | MenhirState114 | MenhirState88 | MenhirState104 | MenhirState102 | MenhirState100 | MenhirState98 | MenhirState96 | MenhirState94 | MenhirState90 | MenhirState92 | MenhirState72 | MenhirState81 | MenhirState79 | MenhirState74 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_2 : (Ast.ast_expr)) = _v in
                let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
                let _v : (Ast.ast_expr) = 
# 226 "parser.mly"
                                    (EArray (_1,_2))
# 5650 "parser.ml"
                 in
                _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
            | MenhirState153 | MenhirState197 | MenhirState163 | MenhirState195 | MenhirState165 | MenhirState185 | MenhirState187 | MenhirState189 | MenhirState191 | MenhirState193 | MenhirState167 | MenhirState183 | MenhirState181 | MenhirState179 | MenhirState177 | MenhirState175 | MenhirState173 | MenhirState169 | MenhirState171 | MenhirState155 | MenhirState161 | MenhirState159 | MenhirState157 | MenhirState149 | MenhirState148 | MenhirState147 | MenhirState146 | MenhirState145 | MenhirState144 | MenhirState143 | MenhirState139 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_2 : (Ast.ast_expr)) = _v in
                let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
                let _v : (Ast.ast_expr) = 
# 279 "parser.mly"
                                     (EArray (_1,_2))
# 5661 "parser.ml"
                 in
                _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Add | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 244 "parser.mly"
                                     (Eminus (_1,_3))
# 5696 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 240 "parser.mly"
                                     (Emult (_1,_3))
# 5721 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Decr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 242 "parser.mly"
                                     (Emod (_1,_3))
# 5746 "parser.ml"
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
        | T_Decr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 241 "parser.mly"
                                     (Ediv (_1,_3))
# 5771 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Colon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState84 in
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
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Decr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Decr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 259 "parser.mly"
                                        (EPlusEq (_1,_3))
# 5940 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Decr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 252 "parser.mly"
                                    (Eor (_1,_3))
# 5989 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Decr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 250 "parser.mly"
                                     (Eneq (_1,_3))
# 6024 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Add | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 243 "parser.mly"
                                     (Eplus (_1,_3))
# 6055 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 246 "parser.mly"
                                     (Elte (_1,_3))
# 6090 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 245 "parser.mly"
                                    (Elt (_1,_3))
# 6125 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 247 "parser.mly"
                                    (Egt (_1,_3))
# 6160 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 248 "parser.mly"
                                     (Egte (_1,_3))
# 6195 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 249 "parser.mly"
                                        (Eeq (_1,_3))
# 6230 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_And | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 251 "parser.mly"
                                     (Eand (_1,_3))
# 6277 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106
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
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 263 "parser.mly"
                                        (EModEq (_1,_3))
# 6342 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108
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
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 260 "parser.mly"
                                          (EMinusEq (_1,_3))
# 6407 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState110
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
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 258 "parser.mly"
                                    (EAssignEq (_1,_3))
# 6472 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState112
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
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 261 "parser.mly"
                                        (EDotEq (_1,_3))
# 6537 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState114
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
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 262 "parser.mly"
                                        (EDivEq (_1,_3))
# 6602 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState116
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
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Colon | T_Comma | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 253 "parser.mly"
                                       (Ecomma (_1,_3))
# 6667 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Colon | T_Comma | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast_expr) = 
# 265 "parser.mly"
                                                           (EQuestT (_1,_3,_5))
# 6721 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 235 "parser.mly"
                          (EAmber _2)
# 6746 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 255 "parser.mly"
                                                 (EMinusMinus (_2,PRE))
# 6771 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 267 "parser.mly"
                          (EDel _2)
# 6796 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rparen | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))), _, (_4 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 316 "parser.mly"
                                                         (ECast (_2,_4))
# 6822 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState125
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
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState125 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 280 "parser.mly"
                                        (_2)
# 6891 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 254 "parser.mly"
                                                 (EPlusPlus (_2,PRE))
# 6918 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))), _, (_4 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 264 "parser.mly"
                                                         (ECast (_2,_4))
# 6944 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState205
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
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState205 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 227 "parser.mly"
                                        (_2)
# 7013 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 236 "parser.mly"
                                          (EPointer _2)
# 7040 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState208
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
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState208
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 239 "parser.mly"
                                          (Enot _2)
# 7124 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState213
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState213
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState213
        | T_Add | T_And | T_Colon | T_Comma | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) = 
# 238 "parser.mly"
                                          (EUnMinus _2)
# 7149 "parser.ml"
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState214
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
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
            let _v : (Ast.ast_expr) = 
# 273 "parser.mly"
                               (_1)
# 7215 "parser.ml"
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
# 164 "parser.mly"
                                                (_2)
# 7232 "parser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (x : (Ast.ast_expr)) = _v in
                let _v : (Ast.ast_expr option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 7240 "parser.ml"
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
    | MenhirState258 | MenhirState260 | MenhirState262 | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Decr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ast_expr))) = _menhir_stack in
            let _v : (Ast.ast_expr option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 7312 "parser.ml"
             in
            _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState236)
    | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Decr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState241 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_Amp ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_Break ->
                _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_Const_Char _v ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
            | T_Const_Int _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
            | T_Const_Real _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
            | T_Const_String _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
            | T_Cont ->
                _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_Decr ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_Del ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_False ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_Id _v ->
                _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
            | T_If ->
                _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_Lbrace ->
                _menhir_run237 _menhir_env (Obj.magic _menhir_stack) MenhirState242
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
            | T_Return ->
                _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_Semicolon ->
                _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | T_For ->
                _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState242)
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState241)
    | MenhirState231 | MenhirState238 | MenhirState270 | MenhirState242 | MenhirState254 | MenhirState265 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_And ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Comma ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Decr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Div ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Div_eq ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Dot_eq ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Eq ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Equal ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Geq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Gr ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Lbracket ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Le ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Leq ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Minus_eq ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Mod ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Mod_eq ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Mul ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Neq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Or ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_PlusEq ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Quest ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState268 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_stmt) = 
# 207 "parser.mly"
                                 (SExpr (Some _1))
# 7498 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState268)
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
# 318 "parser.mly"
                              (if Option.is_some _3 then ENew (_2,(Option.get _3)) else ENew (_2,Eint(1)))
# 7522 "parser.ml"
         in
        _menhir_goto_expression7 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (Ast.ast_expr option)) = _v in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.ast_expr) = 
# 266 "parser.mly"
                              (if Option.is_some _3 then ENew (_2,(Option.get _3)) else ENew (_2,Eint(1)))
# 7534 "parser.ml"
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
# 39 "parser.mly"
       (string)
# 7548 "parser.ml"
    ))) = _menhir_stack in
    let _v : (string * Ast.ast_expr option) = 
# 163 "parser.mly"
                             ( (_1,_2))
# 7553 "parser.ml"
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
                _menhir_reduce136 _menhir_env (Obj.magic _menhir_stack)
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
# 151 "parser.mly"
                                    ([_1])
# 7595 "parser.ml"
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
# 7616 "parser.ml"
         in
        _menhir_goto_list_test2_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : ((Symbol.pass_mode * Types.typ * string) list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Symbol.pass_mode * Types.typ * string))) = _menhir_stack in
        let _v : ((Symbol.pass_mode * Types.typ * string) list) = 
# 196 "parser.mly"
                                         ([_1] @_2)
# 7627 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((Symbol.pass_mode * Types.typ * string) list)) = _v in
        let _v : ((Symbol.pass_mode * Types.typ * string) list option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 7635 "parser.ml"
         in
        _menhir_goto_option_parameter_list_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) = 
# 136 "parser.mly"
          (ignore(closeScope();))
# 7646 "parser.ml"
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
# 268 "parser.mly"
                                                       (_3)
# 7665 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (Ast.ast_expr)) = _v in
            let _v : (Ast.ast_expr option) = 
# 102 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 7673 "parser.ml"
             in
            _menhir_goto_option_test8_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState272 ->
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
# 208 "parser.mly"
                                                      (SNewblock _3)
# 7697 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState275 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rbrace ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit) = 
# 137 "parser.mly"
           (ignore(closeScope2();))
# 7718 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_8 : (unit)) = _v in
            let (((((_menhir_stack, _menhir_s, (_1 : (Symbol.entry))), _, (_3 : (unit))), _, (_4 : (Ast.ast_decl list))), _, (_5 : (Ast.ast_stmt list))), _, (_6 : (unit))) = _menhir_stack in
            let _7 = () in
            let _2 = () in
            let _v : (Ast.ast_fun_def) = 
# 201 "parser.mly"
                                                                                                (
        
        
        (_1,_4,_5))
# 7732 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.ast_fun_def)) = _v in
            let _v : (Ast.ast_decl) = 
# 143 "parser.mly"
                        (FunDef _1)
# 7740 "parser.ml"
             in
            _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState280 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Symbol.entry))), _, (_3 : (unit))) = _menhir_stack in
        let _2 = () in
        let _v : (Symbol.entry) = 
# 166 "parser.mly"
                                                            (_1)
# 7757 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Symbol.entry)) = _v in
        let _v : (Ast.ast_decl) = 
# 145 "parser.mly"
                            (FunDecl _1)
# 7765 "parser.ml"
         in
        _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce125 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) = 
# 135 "parser.mly"
          (ignore(openScope();))
# 7776 "parser.ml"
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
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | T_Char ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | T_Double ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | T_Int ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | T_Void ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce115 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState230)
    | MenhirState237 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Amp ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Break ->
            _menhir_run249 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Const_Char _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | T_Const_Int _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | T_Const_Real _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | T_Const_String _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | T_Cont ->
            _menhir_run245 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Decr ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Del ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_False ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Id _v ->
            _menhir_run243 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | T_If ->
            _menhir_run239 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Lbrace ->
            _menhir_run237 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Return ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Semicolon ->
            _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_For ->
            _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | T_Rbrace ->
            _menhir_reduce117 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState238)
    | _ ->
        _menhir_fail ()

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) = 
# 228 "parser.mly"
                (Ebool true)
# 7918 "parser.ml"
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
# 230 "parser.mly"
                (ENull)
# 7977 "parser.ml"
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
# 39 "parser.mly"
       (string)
# 8199 "parser.ml"
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
# 229 "parser.mly"
                 (Ebool false)
# 8225 "parser.ml"
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
# 43 "parser.mly"
       (string)
# 8326 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 43 "parser.mly"
       (string)
# 8334 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 234 "parser.mly"
                        (Estring _1)
# 8339 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 41 "parser.mly"
       (float)
# 8346 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 41 "parser.mly"
       (float)
# 8354 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 233 "parser.mly"
                      (Ereal _1)
# 8359 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 40 "parser.mly"
       (int)
# 8366 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 40 "parser.mly"
       (int)
# 8374 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 232 "parser.mly"
                     (Eint _1)
# 8379 "parser.ml"
     in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 42 "parser.mly"
       (char)
# 8386 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 42 "parser.mly"
       (char)
# 8394 "parser.ml"
    )) = _v in
    let _v : (Ast.ast_expr) = 
# 231 "parser.mly"
                      (Echar _1)
# 8399 "parser.ml"
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

and _menhir_reduce140 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 8502 "parser.ml"
     in
    _menhir_goto_option_test8_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce125 _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_reduce136 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _v : (Ast.ast_expr option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 8517 "parser.ml"
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

and _menhir_reduce119 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Symbol.pass_mode * Types.typ * string) list) = 
# 185 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 8572 "parser.ml"
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
        _menhir_reduce126 _menhir_env (Obj.magic _menhir_stack) MenhirState9
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
    | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState280
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_function_declation2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Symbol.entry) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Lbrace ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce125 _menhir_env (Obj.magic _menhir_stack) MenhirState229
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
# 8644 "parser.ml"
         in
        _menhir_goto_list_T_Mul_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (unit list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Types.typ))) = _menhir_stack in
        let _v : (Types.typ) = 
# 155 "parser.mly"
                          (if _2 = [] then _1 else List.fold_left (fun x->fun y-> TYPE_pointer x ) _1 _2)
# 8655 "parser.ml"
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
# 39 "parser.mly"
       (string)
# 8671 "parser.ml"
                )) = _v in
                let ((_menhir_stack, _menhir_s, (_1 : (unit option))), _, (_2 : (Types.typ))) = _menhir_stack in
                let _v : (Symbol.pass_mode * Types.typ * string) = 
# 199 "parser.mly"
                                (if is_some _1 then (PASS_BY_REFERENCE,_2,_3) else (PASS_BY_VALUE ,_2,_3))
# 8677 "parser.ml"
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
                        _menhir_reduce119 _menhir_env (Obj.magic _menhir_stack) MenhirState8
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
# 197 "parser.mly"
                         (_2)
# 8702 "parser.ml"
                     in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | T_Comma ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | T_Rparen ->
                        _menhir_reduce119 _menhir_env (Obj.magic _menhir_stack) MenhirState22
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
        | MenhirState285 | MenhirState283 | MenhirState230 | MenhirState2 ->
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
                        _menhir_reduce134 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                    | T_Bool | T_Char | T_Double | T_Int ->
                        _menhir_reduce126 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
                | T_Comma | T_Semicolon ->
                    _menhir_reduce136 _menhir_env (Obj.magic _menhir_stack)
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
                _menhir_reduce140 _menhir_env (Obj.magic _menhir_stack) MenhirState39
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
                _menhir_reduce140 _menhir_env (Obj.magic _menhir_stack) MenhirState52
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
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Lbrace ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), (_2 : (
# 39 "parser.mly"
       (string)
# 8928 "parser.ml"
                ))), _, (_4 : ((Symbol.pass_mode * Types.typ * string) list option))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Symbol.entry) = 
# 189 "parser.mly"
                                                        (
        let e= newFunction (id_make _2) false in 
        let _ = openScope() in
                 may (List.iter (fun x-> ignore (newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) _4 ;  
                 endFunctionHeader e TYPE_none; 
                 e )
# 8941 "parser.ml"
                 in
                _menhir_goto_function_declation2 _menhir_env _menhir_stack _menhir_s _v
            | T_Semicolon ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), (_2 : (
# 39 "parser.mly"
       (string)
# 8949 "parser.ml"
                ))), _, (_4 : ((Symbol.pass_mode * Types.typ * string) list option))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Symbol.entry) = 
# 175 "parser.mly"
                                                        (
        let e= newFunction (id_make _2) false in 
	let _ = forwardFunction e in 
        let _ = openScope() in
                 may (List.iter (fun x-> ignore (newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) _4 ;  
                 endFunctionHeader e TYPE_none; 
                 e )
# 8963 "parser.ml"
                 in
                _menhir_goto_function_declation1 _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Lbrace ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, (_1 : (Types.typ))), _, (_2 : (
# 39 "parser.mly"
       (string)
# 8993 "parser.ml"
                ))), _, (_4 : ((Symbol.pass_mode * Types.typ * string) list option))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _v : (Symbol.entry) = 
# 183 "parser.mly"
                                                     (
        let e= newFunction (id_make _2) false in  
        let _ = openScope() in
                may (List.iter (fun x-> ignore(newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) _4 ;  
                endFunctionHeader e _1 ; 
                e)
# 9005 "parser.ml"
                 in
                _menhir_goto_function_declation2 _menhir_env _menhir_stack _menhir_s _v
            | T_Semicolon ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, (_1 : (Types.typ))), _, (_2 : (
# 39 "parser.mly"
       (string)
# 9013 "parser.ml"
                ))), _, (_4 : ((Symbol.pass_mode * Types.typ * string) list option))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _v : (Symbol.entry) = 
# 168 "parser.mly"
                                                     (
        let e= newFunction (id_make _2) false in 
	let _ = forwardFunction e in 
        let _ = openScope() in
                may (List.iter (fun x-> ignore(newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) _4 ;  
                endFunctionHeader e _1 ; 
                e)
# 9026 "parser.ml"
                 in
                _menhir_goto_function_declation1 _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_reduce113 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) = 
# 185 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 9069 "parser.ml"
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
        _menhir_reduce113 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_reduce126 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 9093 "parser.ml"
     in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce134 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Symbol.pass_mode * Types.typ * string) list option) = 
# 100 "/home/zinc/.opam/system/lib/menhir/standard.mly"
    ( None )
# 9102 "parser.ml"
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
# 9114 "parser.ml"
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
        _menhir_reduce113 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState285 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState283 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState280 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState275 ->
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
    | MenhirState268 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState265 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState262 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState260 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState254 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState249 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState245 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState242 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState241 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState238 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState237 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState236 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState231 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState230 ->
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
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState193 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState189 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
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
                _menhir_reduce134 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | T_Bool | T_Char | T_Double | T_Int ->
                _menhir_reduce126 _menhir_env (Obj.magic _menhir_stack) MenhirState5
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
# 157 "parser.mly"
                   (TYPE_int)
# 9892 "parser.ml"
     in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) = 
# 160 "parser.mly"
                   (TYPE_double)
# 9904 "parser.ml"
     in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) = 
# 158 "parser.mly"
                 (TYPE_char)
# 9916 "parser.ml"
     in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) = 
# 159 "parser.mly"
                 (TYPE_bool)
# 9928 "parser.ml"
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
# 98 "parser.mly"
     (unit)
# 9947 "parser.ml"
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
                ((*ignore(initSymbolTable 256);*) ignore (openScope ());)
# 9963 "parser.ml"
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
  


# 9989 "parser.ml"
