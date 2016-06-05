
exception Error

let _eRR =
  Error

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
  | T_Id of (string)
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
  | T_Const_String of (string)
  | T_Const_Real of (float)
  | T_Const_Int of (int)
  | T_Const_Char of (string)
  | T_Comma
  | T_Colon
  | T_Char
  | T_Byref
  | T_Break
  | T_Bool
  | T_And
  | T_Amp
  | T_Add

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState208
  | MenhirState204
  | MenhirState200
  | MenhirState197
  | MenhirState195
  | MenhirState191
  | MenhirState188
  | MenhirState185
  | MenhirState183
  | MenhirState181
  | MenhirState177
  | MenhirState172
  | MenhirState168
  | MenhirState165
  | MenhirState164
  | MenhirState163
  | MenhirState161
  | MenhirState160
  | MenhirState159
  | MenhirState156
  | MenhirState154
  | MenhirState153
  | MenhirState152
  | MenhirState146
  | MenhirState139
  | MenhirState136
  | MenhirState134
  | MenhirState133
  | MenhirState130
  | MenhirState127
  | MenhirState125
  | MenhirState117
  | MenhirState116
  | MenhirState115
  | MenhirState110
  | MenhirState109
  | MenhirState106
  | MenhirState105
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState99
  | MenhirState96
  | MenhirState95
  | MenhirState94
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
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
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState48
  | MenhirState47
  | MenhirState46
  | MenhirState45
  | MenhirState44
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState33
  | MenhirState32
  | MenhirState31
  | MenhirState26
  | MenhirState25
  | MenhirState23
  | MenhirState21
  | MenhirState20
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState15
  | MenhirState14
  | MenhirState12
  | MenhirState10
  | MenhirState8
  | MenhirState2
  
        open Lexing
        open Symbol
        open Types
        open Ast
        open Error
        open Semantic
        open Identifier
        open Option


        let get_first (x,_,_) =x ;;
        let get_second (_,x,_)=x;;
        let get_third (_,_,x)=x;;

let rec _menhir_goto_list_declation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Amp ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Break ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Const_Char _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | T_Const_Int _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | T_Const_Real _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | T_Const_String _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | T_Cont ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Decr ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Del ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_False ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Id _v ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | T_If ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Incr ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Lbrace ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Lparen ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Mul ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_New ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Not ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Null ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Return ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Semicolon ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Sub ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_True ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_For ->
            _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Rbrace ->
            _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
    | MenhirState204 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_decl))), _, (xs : (Ast.ast_decl list))) = _menhir_stack in
        let _v : (Ast.ast_decl list) =     ( x :: xs ) in
        _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

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
            let _v : (unit) =                                          ( ignore(is_main()); ast_tree := _2;check (Some _2);) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (unit)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_decl))), _, (xs : (Ast.ast_program))) = _menhir_stack in
        let _v : (Ast.ast_program) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_declation_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_test2_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Symbol.pass_mode * Types.typ * bytes) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Symbol.pass_mode * Types.typ * bytes) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Symbol.pass_mode * Types.typ * bytes))) = _menhir_stack in
        let _v : ((Symbol.pass_mode * Types.typ * bytes) list) =     ( x :: xs ) in
        _menhir_goto_list_test2_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : ((Symbol.pass_mode * Types.typ * bytes) list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Symbol.pass_mode * Types.typ * bytes))) = _menhir_stack in
        let _v : ((Symbol.pass_mode * Types.typ * bytes) list) =                                          ([_1] @_2) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((Symbol.pass_mode * Types.typ * bytes) list)) = _v in
        let _v : ((Symbol.pass_mode * Types.typ * bytes) list option) =     ( Some x ) in
        _menhir_goto_option_parameter_list_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_decl list) =     ( [] ) in
    _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_expression_list_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr list option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (string))), _, (_3 : (Ast.ast_expr list option))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                                   (ENull) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState181 ->
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
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Amp ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Const_Char _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Const_Int _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Const_Real _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Const_String _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Decr ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Del ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_False ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Id _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Incr ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Lparen ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Mul ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_New ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Not ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Null ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Sub ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_True ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Semicolon ->
                _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState183 ->
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
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Amp ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Const_Char _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | T_Const_Int _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | T_Const_Real _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | T_Const_String _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | T_Decr ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Del ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_False ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Id _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | T_Incr ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Lparen ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Mul ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_New ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Not ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Null ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Sub ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_True ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Rparen ->
                _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit) =          (ignore(nested_loops := !nested_loops +1)) in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Amp ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Break ->
                _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Const_Char _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | T_Const_Int _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | T_Const_Real _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | T_Const_String _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | T_Cont ->
                _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Decr ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Del ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_False ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Id _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | T_If ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Incr ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Lbrace ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Lparen ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Mul ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_New ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Not ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Null ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Return ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Semicolon ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Sub ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_True ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_For ->
                _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_declator_plus : _menhir_env -> 'ttv_tail -> _menhir_state -> (bytes list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (bytes list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Types.typ))) = _menhir_stack in
        let _v : (Ast.ast_var_decl) =                                                  (List.map (fun x -> newVariable (id_make x) _1 true) _2) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.ast_var_decl)) = _v in
        let _v : (Ast.ast_decl) =                               (VarDecl _1) in
        _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (bytes list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (bytes))) = _menhir_stack in
        let _2 = () in
        let _v : (bytes list) =                                          (_1::_3) in
        _menhir_goto_declator_plus _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_test9_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.ast_expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.ast_expr))) = _menhir_stack in
        let _v : (Ast.ast_expr list) =     ( x :: xs ) in
        _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Ast.ast_expr list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
        let _v : (Ast.ast_expr list) =                                    (check_expr _1;[_1] @ _2) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.ast_expr list)) = _v in
        let _v : (Ast.ast_expr list option) =     ( Some x ) in
        _menhir_goto_option_expression_list_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_declation : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState204 | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | T_Char ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | T_Double ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | T_Int ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | T_Void ->
            _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState204)
    | MenhirState208 | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Char ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Double ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Int ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Void ->
            _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | T_Eof ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ast_decl))) = _menhir_stack in
            let _v : (Ast.ast_program) =     ( [ x ] ) in
            _menhir_goto_nonempty_list_declation_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState208)
    | _ ->
        _menhir_fail ()

and _menhir_reduce70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Symbol.pass_mode * Types.typ * bytes) list) =     ( [] ) in
    _menhir_goto_list_test2_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run134 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Byref ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | T_Bool | T_Char | T_Double | T_Int ->
        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134

and _menhir_goto_option_test8_ : _menhir_env -> 'ttv_tail -> (Ast.ast_expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_3 : (Ast.ast_expr option)) = _v in
    let ((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))) = _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) =                               (ENew (_2,_3)) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Lbracket ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | T_Amp ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | T_Const_Char _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | T_Const_Int _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | T_Const_Real _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | T_Const_String _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | T_Decr ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | T_Del ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | T_False ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | T_Id _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | T_Incr ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | T_Lparen ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | T_Mul ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | T_New ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | T_Not ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | T_Null ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | T_Sub ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | T_True ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
    | T_Comma | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (Ast.ast_expr option) =     ( None ) in
        _menhir_goto_option_test_ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_test3_ : _menhir_env -> 'ttv_tail -> (Ast.ast_stmt option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_6 : (Ast.ast_stmt option)) = _v in
    let ((((_menhir_stack, _menhir_s), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_stmt))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : (Ast.ast_stmt) =                                                               (Sif (_3,_5,_6)) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_T_Id_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (bytes option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (bytes option))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_stmt) =                                    (if !nested_loops =0 then (error "No continue in Loop"; SCont _2) else SCont _2) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (bytes option))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_stmt) =                                    (if !nested_loops = 0 then (error "No break in loop" ; SBreak _2)  else SBreak _2) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_function_declation1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Symbol.entry) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Lbrace ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState152 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Char ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Double ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Int ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Void ->
            _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | T_Semicolon ->
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152

and _menhir_reduce83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr list option) =     ( None ) in
    _menhir_goto_option_expression_list_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_test_ : _menhir_env -> 'ttv_tail -> (Ast.ast_expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_2 : (Ast.ast_expr option)) = _v in
    let (_menhir_stack, _menhir_s, (_1 : (string))) = _menhir_stack in
    let _v : (bytes) =                      ( ignore (Option.map (fun x -> (if (get_type x) = (get_entry_type (lookupEntry (id_make _1) LOOKUP_ALL_SCOPES true)) then () else error "constant intialization type error"; )) _2) ;_1) in
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
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (bytes))) = _menhir_stack in
        let _2 = () in
        let _v : (bytes list) =                                     ([_1]) in
        _menhir_goto_declator_plus _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr list) =     ( [] ) in
    _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) =           (ignore(closeScope();)) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (unit))), _, (_4 : (Ast.ast_expr))), _, (_5 : (unit))) = _menhir_stack in
            let _6 = () in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                                                    (ENull) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _, (_2 : (unit))), _, (_3 : (Ast.ast_expr))), _, (_4 : (unit))) = _menhir_stack in
            let _5 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                                        (_3) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (Ast.ast_expr)) = _v in
            let _v : (Ast.ast_expr option) =     ( Some x ) in
            _menhir_goto_option_test8_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState197 ->
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
            let _v : (Ast.ast_stmt) =                                                       (SNewblock _3) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState200 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rbrace ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (_1 : (Symbol.entry))), _), _, (_3 : (Ast.ast_decl list))), _, (_4 : (Ast.ast_stmt list))), _, (_5 : (unit))) = _menhir_stack in
            let _6 = () in
            let _2 = () in
            let _v : (Ast.ast_fun_def) =                                                                                   (
        
        
        (_1,_3,_4)) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.ast_fun_def)) = _v in
            let _v : (Ast.ast_decl) =                        (FunDef _1) in
            _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Symbol.entry))), _, (_2 : (unit))) = _menhir_stack in
            let _3 = () in
            let _v : (Symbol.entry) =                                                            (_1) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Symbol.entry)) = _v in
            let _v : (Ast.ast_decl) =                             (FunDecl _1) in
            _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce42 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.ast_expr)) * _menhir_state) * _menhir_state * (Ast.ast_expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                                        (Ecomma (_1,_3)) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                            (EPlusPlus (_1,AFTER)) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                            (EMinusMinus (_1,AFTER)) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_goto_list_T_Mul_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (unit list)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let x = () in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_T_Mul_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (unit list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Types.typ))) = _menhir_stack in
        let _v : (Types.typ) =                           (if _2 = [] then _1 else List.fold_left (fun x->fun y-> TYPE_pointer x ) _1 _2) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState208 | MenhirState204 | MenhirState153 | MenhirState2 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Id _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
        | MenhirState15 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Lbracket ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (Ast.ast_expr option) =     ( None ) in
                _menhir_goto_option_test8_ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState20 ->
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
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | T_Amp ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | T_Const_Char _v ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | T_Const_Int _v ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | T_Const_Real _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | T_Const_String _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | T_Decr ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | T_Del ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | T_False ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | T_Id _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | T_Incr ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | T_Lparen ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | T_Mul ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | T_New ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | T_Not ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | T_Null ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | T_Sub ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | T_True ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState136 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Id _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_3 : (string)) = _v in
                let ((_menhir_stack, _menhir_s, (_1 : (unit option))), _, (_2 : (Types.typ))) = _menhir_stack in
                let _v : (Symbol.pass_mode * Types.typ * bytes) =                                 (if is_some _1 then (PASS_BY_REFERENCE,_2,_3) else (PASS_BY_VALUE ,_2,_3)) in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                (match _menhir_s with
                | MenhirState146 | MenhirState130 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | T_Comma ->
                        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                    | T_Rparen ->
                        _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
                | MenhirState134 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _, (_2 : (Symbol.pass_mode * Types.typ * bytes))) = _menhir_stack in
                    let _1 = () in
                    let _v : (Symbol.pass_mode * Types.typ * bytes) =                          (_2) in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | T_Comma ->
                        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                    | T_Rparen ->
                        _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
                | _ ->
                    _menhir_fail ())
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState127 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
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
                        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | T_Rparen ->
                        _menhir_reduce85 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | T_Bool | T_Char | T_Double | T_Int ->
                        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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

and _menhir_goto_list_statement_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_stmt list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_stmt))), _, (xs : (Ast.ast_stmt list))) = _menhir_stack in
        let _v : (Ast.ast_stmt list) =     ( x :: xs ) in
        _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState197
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState200
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_stmt) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState165 ->
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
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Amp ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Break ->
                _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Const_Char _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | T_Const_Int _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | T_Const_Real _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | T_Const_String _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | T_Cont ->
                _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Decr ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Del ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_False ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Id _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | T_If ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Incr ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Lbrace ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Lparen ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Mul ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_New ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Not ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Null ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Return ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Semicolon ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Sub ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_True ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_For ->
                _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177)
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (Ast.ast_stmt option) =     ( None ) in
            _menhir_goto_option_test3_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (_2 : (Ast.ast_stmt))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.ast_stmt) =                        (_2) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.ast_stmt)) = _v in
        let _v : (Ast.ast_stmt option) =     ( Some x ) in
        _menhir_goto_option_test3_ _menhir_env _menhir_stack _v
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (unit) =           (ignore(nested_loops := !nested_loops -1)) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_12 : (unit)) = _v in
        let ((((((_menhir_stack, _menhir_s, (_1 : (bytes option))), _, (_4 : (Ast.ast_expr list option))), _, (_6 : (Ast.ast_expr list option))), _, (_8 : (Ast.ast_expr list option))), (_10 : (unit))), _, (_11 : (Ast.ast_stmt))) = _menhir_stack in
        let _9 = () in
        let _7 = () in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ast.ast_stmt) =                                                                                                                                               (Sfor (_1,_4,_6,_8,_11)) in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState154 | MenhirState195 | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Amp ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Break ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Const_Char _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_Const_Int _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_Const_Real _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_Const_String _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_Cont ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Decr ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Del ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_False ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Id _v ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_If ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Incr ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Lbrace ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Lparen ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Mul ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_New ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Not ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Null ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Return ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Semicolon ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Sub ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_True ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_For ->
            _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Rbrace ->
            _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState195)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr option))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.ast_stmt) =                                           (Sreturn _2) in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_test4_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (bytes option) -> 'ttv_return =
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
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Amp ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Const_Char _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
            | T_Const_Int _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
            | T_Const_Real _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
            | T_Const_String _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
            | T_Decr ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Del ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_False ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Id _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
            | T_Incr ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Lparen ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Mul ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_New ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Not ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Null ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Sub ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_True ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Semicolon ->
                _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181)
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

and _menhir_reduce79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bytes option) =     ( None ) in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run169 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (string)) = _v in
    let _v : (bytes option) =     ( Some x ) in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_parameter_list_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Symbol.pass_mode * Types.typ * bytes) list option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (_1 : (unit))), _), (_3 : (string))), _, (_5 : ((Symbol.pass_mode * Types.typ * bytes) list option))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _v : (Symbol.entry) =                                                               (
        let e= newFunction (id_make _3) true in 
                 may (List.iter (fun x-> ignore (newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) _5 ;  
                 endFunctionHeader e TYPE_none; 
                 e ) in
            _menhir_goto_function_declation1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (_1 : (unit))), _, (_2 : (Types.typ))), (_3 : (string))), _, (_5 : ((Symbol.pass_mode * Types.typ * bytes) list option))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _v : (Symbol.entry) =                                                             (
        let e= newFunction (id_make _3) true in  
                may (List.iter (fun x-> ignore(newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) _5 ;  
                endFunctionHeader e _2 ; 
                e) in
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
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_Char ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_Double ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | T_Int ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136

and _menhir_reduce13 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (string))) = _menhir_stack in
    let _v : (Ast.ast_expr) =                  (Eid _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Rparen ->
        _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EUnAdd _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Colon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState35 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | T_Amp ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | T_Const_Char _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | T_Const_Int _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | T_Const_Real _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | T_Const_String _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | T_Decr ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | T_Del ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | T_False ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | T_Id _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | T_Incr ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | T_Lparen ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | T_Mul ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | T_New ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | T_Not ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | T_Null ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | T_Sub ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | T_True ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eminus (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EPlusEq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EModEq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                           (EMinusEq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (EAssignEq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EDotEq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EDivEq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Eor (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eneq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eand (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Emult (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Elte (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Elt (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Egt (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Egte (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                        (Eeq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eplus (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Emod (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Rbracket ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState76
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
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Ediv (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                                           (EQuestT (_1,_3,_5)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Colon | T_Decr | T_Incr | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EAmber _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Colon | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                            (EMinusMinus (_2,PRE)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Colon | T_Decr | T_Incr | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EDel _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState181 | MenhirState183 | MenhirState185 | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState91 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Amp ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Const_Char _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | T_Const_Int _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | T_Const_Real _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | T_Const_String _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | T_Decr ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Del ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_False ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Id _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | T_Incr ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Lparen ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Mul ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_New ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Not ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Null ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Sub ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_True ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Rparen | T_Semicolon ->
            _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Rparen | T_Semicolon ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (check_expr _2 ;_2) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Comma ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState94 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Add ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | T_Amp ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | T_Const_Char _v ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                | T_Const_Int _v ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                | T_Const_Real _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                | T_Const_String _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                | T_Decr ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | T_Del ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | T_False ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | T_Id _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                | T_Incr ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | T_Lparen ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | T_Mul ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | T_New ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | T_Not ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | T_Null ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | T_Sub ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | T_True ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
            | T_Rparen | T_Semicolon ->
                _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Colon | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                            (EPlusPlus (_2,PRE)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))), _, (_4 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                              (ECast (_2,_4)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState103 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                         (_2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EPointer _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Rbracket ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (Enot _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EUnMinus _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
            let _v : (Ast.ast_expr) =                                (_1) in
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
                let _v : (Ast.ast_expr) =                                                 (_2) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (x : (Ast.ast_expr)) = _v in
                let _v : (Ast.ast_expr option) =     ( Some x ) in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ast_expr))) = _menhir_stack in
            let _v : (Ast.ast_expr option) =     ( Some x ) in
            _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159)
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState164 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Amp ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Break ->
                _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Const_Char _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | T_Const_Int _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | T_Const_Real _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | T_Const_String _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | T_Cont ->
                _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Decr ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Del ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_False ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Id _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | T_If ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Incr ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Lbrace ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Lparen ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Mul ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_New ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Not ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Null ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Return ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Semicolon ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Sub ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_True ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_For ->
                _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165)
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164)
    | MenhirState154 | MenhirState161 | MenhirState195 | MenhirState165 | MenhirState177 | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_And ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Comma ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Decr ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Div_eq ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Dot_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Equal ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Geq ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Gr ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Incr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Le ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Leq ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Minus_eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Mod ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Mod_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Neq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Or ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_PlusEq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Quest ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState191 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_stmt) =                                  (SExpr (Some _1)) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191)
    | _ ->
        _menhir_fail ()

and _menhir_reduce64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_T_Mul_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run106 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Mul ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Id _ | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_stmt list) =     ( [] ) in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bytes option) =     ( None ) in
    _menhir_goto_option_test4_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run155 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_stmt) =                        (SExpr None) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run156 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState156 in
        let _v : (Ast.ast_expr option) =     ( None ) in
        _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156

and _menhir_run160 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState160

and _menhir_run162 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Amp ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Const_Char _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Const_Int _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Const_Real _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Const_String _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Decr ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Del ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_False ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Id _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Incr ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Lparen ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Mul ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_New ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Not ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Null ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Sub ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_True ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run166 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Colon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (bytes) =                     (_1) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (bytes)) = _v in
        let _v : (bytes option) =     ( Some x ) in
        _menhir_goto_option_test4_ _menhir_env _menhir_stack _menhir_s _v
    | T_Lparen ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Semicolon | T_Sub ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run168 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
    | T_Semicolon ->
        _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168

and _menhir_run172 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
    | T_Semicolon ->
        _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Symbol.pass_mode * Types.typ * bytes) list option) =     ( None ) in
    _menhir_goto_option_parameter_list_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) =                 (Ebool true) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) =                 (ENull) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Bool ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | T_Char ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | T_Double ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | T_Int ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Bool ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Char ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Double ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Int ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Lparen ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) =                  (Ebool false) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (string)) = _v in
    let _v : (Ast.ast_expr) =                         (Estring _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (float)) = _v in
    let _v : (Ast.ast_expr) =                       (Ereal _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (int)) = _v in
    let _v : (Ast.ast_expr) =                      (Eint _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (string)) = _v in
    let _v : (Ast.ast_expr) =                       (Estring _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Amp ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Const_Char _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | T_Const_Int _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | T_Const_Real _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | T_Const_String _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | T_Decr ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Del ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_False ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | T_Incr ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Lparen ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Mul ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_New ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Not ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Null ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Sub ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_True ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_goto_basic_type : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Mul ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Id _ | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState204 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState200 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | MenhirState156 ->
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
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) =           (ignore(openScope();)) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | T_Amp ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | T_Const_Char _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | T_Const_Int _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | T_Const_Real _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | T_Const_String _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | T_Decr ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | T_Del ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | T_False ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | T_Id _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | T_Incr ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | T_Lparen ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | T_Mul ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | T_New ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | T_Not ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | T_Null ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | T_Sub ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | T_True ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Amp ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Const_Char _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | T_Const_Int _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | T_Const_Real _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | T_Const_String _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | T_Decr ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Del ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_False ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Id _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | T_Incr ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Lparen ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Mul ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_New ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Not ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Null ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Sub ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_True ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState208 | MenhirState204 | MenhirState153 | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Char ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Double ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Int ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Void ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState127 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
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
                        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | T_Rparen ->
                        _menhir_reduce85 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | T_Bool | T_Char | T_Double | T_Int ->
                        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Amp ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Break ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Const_Char _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Const_Int _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Const_Real _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Const_String _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Cont ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Decr ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Del ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_False ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Id _v ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_If ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Incr ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Lbrace ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Lparen ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Mul ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_New ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Not ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Null ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Return ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Semicolon ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Sub ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_True ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_For ->
            _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Rbrace ->
            _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
    | _ ->
        _menhir_fail ()

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) =                    (TYPE_int) in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) =                    (TYPE_double) in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) =                  (TYPE_char) in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) =                  (TYPE_bool) in
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

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit) =
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
    let _v : (unit) =                 (ignore(initSymbolTable 256); ignore (openScope ());) in
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Bool ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | T_Char ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | T_Double ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | T_Int ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | T_Void ->
        _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
  

