
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
  | T_Const_Char of (char)
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
  | MenhirState207
  | MenhirState203
  | MenhirState199
  | MenhirState196
  | MenhirState194
  | MenhirState190
  | MenhirState187
  | MenhirState184
  | MenhirState182
  | MenhirState180
  | MenhirState176
  | MenhirState171
  | MenhirState167
  | MenhirState164
  | MenhirState163
  | MenhirState162
  | MenhirState160
  | MenhirState159
  | MenhirState158
  | MenhirState155
  | MenhirState153
  | MenhirState152
  | MenhirState151
  | MenhirState145
  | MenhirState138
  | MenhirState135
  | MenhirState133
  | MenhirState132
  | MenhirState129
  | MenhirState126
  | MenhirState123
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState108
  | MenhirState107
  | MenhirState104
  | MenhirState103
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState97
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
  | MenhirState77
  | MenhirState76
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
  | MenhirState30
  | MenhirState29
  | MenhirState24
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState16
  | MenhirState15
  | MenhirState13
  | MenhirState12
  | MenhirState10
  | MenhirState8
  | MenhirState6
  | MenhirState0
  
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
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Amp ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Break ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Const_Char _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | T_Const_Int _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | T_Const_Real _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | T_Const_String _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | T_Cont ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Del ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_False ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Id _v ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | T_If ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Lbrace ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Lparen ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Mul ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_New ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Not ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Null ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Return ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Semicolon ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Sub ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_True ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_For ->
            _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Rbrace ->
            _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | MenhirState203 ->
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
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Eof ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_program))) = _menhir_stack in
            let _2 = () in
            let _v : (unit) =                           (ignore(initSymbolTable 256 ); ignore(openScope()); ignore(is_main()); ast_tree := _1;check (Some _1);) in
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
    | MenhirState207 ->
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
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Symbol.pass_mode * Types.typ * bytes) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Symbol.pass_mode * Types.typ * bytes))) = _menhir_stack in
        let _v : ((Symbol.pass_mode * Types.typ * bytes) list) =     ( x :: xs ) in
        _menhir_goto_list_test2_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState132 ->
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

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_decl list) =     ( [] ) in
    _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_expression_list_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr list option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
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
    | MenhirState180 ->
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
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_Id _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_New ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_Not ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_Null ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_Sub ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_True ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | T_Semicolon ->
                _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState182 ->
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
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_Id _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_New ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_Not ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_Null ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_Sub ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_True ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | T_Rparen ->
                _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState184 ->
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
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Break ->
                _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | T_Cont ->
                _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Id _v ->
                _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | T_If ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Lbrace ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_New ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Not ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Null ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Return ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Semicolon ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Sub ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_True ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_For ->
                _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
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
    | MenhirState6 ->
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
    | MenhirState123 ->
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
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.ast_expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.ast_expr))) = _menhir_stack in
        let _v : (Ast.ast_expr list) =     ( x :: xs ) in
        _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState89 ->
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
    | MenhirState203 | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState203
        | T_Char ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState203
        | T_Double ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState203
        | T_Int ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState203
        | T_Void ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState203
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState203
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203)
    | MenhirState207 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Char ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Double ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Int ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Void ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Eof ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ast_decl))) = _menhir_stack in
            let _v : (Ast.ast_program) =     ( [ x ] ) in
            _menhir_goto_nonempty_list_declation_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState207)
    | _ ->
        _menhir_fail ()

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Symbol.pass_mode * Types.typ * bytes) list) =     ( [] ) in
    _menhir_goto_list_test2_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run133 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Byref ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | T_Bool | T_Char | T_Double | T_Int ->
        _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133

and _menhir_goto_option_test8_ : _menhir_env -> 'ttv_tail -> (Ast.ast_expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_3 : (Ast.ast_expr option)) = _v in
    let ((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))) = _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) =                               (ENew (_2,_3)) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | T_Amp ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | T_Const_Char _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | T_Const_Int _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | T_Const_Real _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | T_Const_String _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | T_Del ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | T_False ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | T_Id _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | T_Lparen ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | T_Mul ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | T_New ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | T_Not ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | T_Null ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | T_Sub ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | T_True ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
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
    | MenhirState167 ->
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
    | MenhirState171 ->
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
        let _menhir_s = MenhirState151 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Char ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Double ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Int ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Void ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
    | T_Semicolon ->
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151

and _menhir_reduce82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
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

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr list) =     ( [] ) in
    _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) =           (ignore(closeScope();)) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState74 ->
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
    | MenhirState108 ->
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
    | MenhirState196 ->
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
    | MenhirState199 ->
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
    | MenhirState151 ->
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

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                            (EPlusPlus (_1,AFTER)) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                            (EMinusMinus (_1,AFTER)) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState60
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
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState62
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
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState64
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
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44
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
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
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
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_goto_list_T_Mul_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (unit list)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let x = () in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_T_Mul_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (unit list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Types.typ))) = _menhir_stack in
        let _v : (Types.typ) =                           (if _2 = [] then _1 else List.fold_left (fun x->fun y-> TYPE_pointer x ) _1 _2) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState207 | MenhirState203 | MenhirState152 | MenhirState0 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Id _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
        | MenhirState13 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Lbracket ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState15
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
        | MenhirState18 ->
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
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | T_Amp ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | T_Const_Char _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | T_Const_Int _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | T_Const_Real _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | T_Const_String _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | T_Decr ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | T_Del ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | T_False ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | T_Id _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | T_Incr ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | T_Lparen ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | T_Mul ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | T_New ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | T_Not ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | T_Null ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | T_Sub ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | T_True ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState135 ->
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
                | MenhirState145 | MenhirState129 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | T_Comma ->
                        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState132
                    | T_Rparen ->
                        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState132
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
                | MenhirState133 ->
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
                        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                    | T_Rparen ->
                        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
                | _ ->
                    _menhir_fail ())
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState126 ->
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
                        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                    | T_Rparen ->
                        _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                    | T_Bool | T_Char | T_Double | T_Int ->
                        _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
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
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_stmt))), _, (xs : (Ast.ast_stmt list))) = _menhir_stack in
        let _v : (Ast.ast_stmt list) =     ( x :: xs ) in
        _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState196
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState199
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_stmt) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState164 ->
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
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Break ->
                _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | T_Cont ->
                _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Id _v ->
                _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | T_If ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Lbrace ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_New ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Not ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Null ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Return ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Semicolon ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_Sub ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_True ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | T_For ->
                _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176)
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
    | MenhirState176 ->
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
    | MenhirState187 ->
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
    | MenhirState153 | MenhirState194 | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Amp ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Break ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Const_Char _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
        | T_Const_Int _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
        | T_Const_Real _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
        | T_Const_String _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
        | T_Cont ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Del ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_False ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Id _v ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
        | T_If ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Lbrace ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Lparen ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Mul ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_New ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Not ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Null ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Return ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Semicolon ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Sub ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_True ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_For ->
            _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | T_Rbrace ->
            _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState194
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194)
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
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_Id _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_New ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_Not ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_Null ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_Sub ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_True ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | T_Semicolon ->
                _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState180
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180)
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

and _menhir_reduce78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bytes option) =     ( None ) in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run168 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
    | MenhirState129 ->
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
    | MenhirState145 ->
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
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | T_Char ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | T_Double ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | T_Int ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135

and _menhir_reduce13 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (string))) = _menhir_stack in
    let _v : (Ast.ast_expr) =                  (Eid _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Rparen ->
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EUnAdd _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Colon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState33 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | T_Id _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | T_New ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | T_Not ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | T_Null ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | T_Sub ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | T_True ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eminus (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EPlusEq (_1,_3)) in
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
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EModEq (_1,_3)) in
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
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                           (EMinusEq (_1,_3)) in
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
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (EAssignEq (_1,_3)) in
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
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EDotEq (_1,_3)) in
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
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EDivEq (_1,_3)) in
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
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Eor (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eneq (_1,_3)) in
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
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eand (_1,_3)) in
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
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Emult (_1,_3)) in
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
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Elte (_1,_3)) in
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Elt (_1,_3)) in
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Egt (_1,_3)) in
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Egte (_1,_3)) in
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                        (Eeq (_1,_3)) in
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eplus (_1,_3)) in
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
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Emod (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Rbracket ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Ediv (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState82
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Colon | T_Decr | T_Incr | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EAmber _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Colon | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                            (EMinusMinus (_2,PRE)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Colon | T_Decr | T_Incr | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EDel _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState180 | MenhirState182 | MenhirState184 | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState89 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | T_Id _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | T_New ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | T_Not ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | T_Null ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | T_Sub ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | T_True ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Rparen | T_Semicolon ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Rparen | T_Semicolon ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState94
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
                let _menhir_s = MenhirState92 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Add ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | T_Amp ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | T_Const_Char _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | T_Const_Int _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | T_Const_Real _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | T_Const_String _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | T_Decr ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | T_Del ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | T_False ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | T_Id _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | T_Incr ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | T_Lparen ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | T_Mul ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | T_New ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | T_Not ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | T_Null ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | T_Sub ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | T_True ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
            | T_Rparen | T_Semicolon ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Colon | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                            (EPlusPlus (_2,PRE)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState100
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState101 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                         (_2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EPointer _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Rbracket ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (Enot _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EUnMinus _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState115
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ast_expr))) = _menhir_stack in
            let _v : (Ast.ast_expr option) =     ( Some x ) in
            _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState163 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Break ->
                _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | T_Cont ->
                _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Id _v ->
                _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | T_If ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Lbrace ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_New ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Not ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Null ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Return ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Semicolon ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_Sub ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_True ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | T_For ->
                _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164)
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
    | MenhirState153 | MenhirState160 | MenhirState194 | MenhirState164 | MenhirState176 | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_And ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Comma ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Decr ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Div ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Div_eq ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Dot_eq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Eq ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Equal ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Geq ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Gr ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Incr ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Lbracket ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Le ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Leq ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Minus_eq ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Mod_eq ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Mul ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Neq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Or ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_PlusEq ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Quest ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState190 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_stmt) =                                  (SExpr (Some _1)) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190)
    | _ ->
        _menhir_fail ()

and _menhir_reduce63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_T_Mul_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run104 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Mul ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Id _ | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_stmt list) =     ( [] ) in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bytes option) =     ( None ) in
    _menhir_goto_option_test4_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run154 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_stmt) =                        (SExpr None) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run155 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState155
    | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState155 in
        let _v : (Ast.ast_expr option) =     ( None ) in
        _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155

and _menhir_run159 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState159

and _menhir_run161 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Amp ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Const_Char _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
        | T_Const_Int _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
        | T_Const_Real _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
        | T_Const_String _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Del ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_False ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Id _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Lparen ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Mul ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_New ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Not ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Null ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Sub ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_True ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run165 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Semicolon | T_Sub ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run167 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
    | T_Semicolon ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState167

and _menhir_run171 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
    | T_Semicolon ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171

and _menhir_reduce76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Symbol.pass_mode * Types.typ * bytes) list option) =     ( None ) in
    _menhir_goto_option_parameter_list_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run130 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) =                 (Ebool true) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) =                 (ENull) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Bool ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | T_Char ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | T_Double ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | T_Int ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Bool ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Char ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Double ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Int ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Lparen ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) =                  (Ebool false) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (string)) = _v in
    let _v : (Ast.ast_expr) =                         (Estring _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (float)) = _v in
    let _v : (Ast.ast_expr) =                       (Ereal _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (int)) = _v in
    let _v : (Ast.ast_expr) =                      (Eint _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (char) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (char)) = _v in
    let _v : (Ast.ast_expr) =                       (Echar _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_New ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_Not ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_Null ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_Sub ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_True ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_goto_basic_type : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Mul ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Id _ | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) =           (ignore(openScope();)) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_Amp ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_Const_Char _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | T_Const_Int _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | T_Const_Real _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | T_Const_String _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_Del ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_False ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_Id _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_Lparen ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_Mul ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_New ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_Not ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_Null ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_Sub ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | T_True ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Amp ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Const_Char _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | T_Const_Int _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | T_Const_Real _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | T_Const_String _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Del ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_False ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Id _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Lparen ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Mul ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_New ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Not ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Null ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Sub ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_True ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState207 | MenhirState203 | MenhirState152 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Char ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Double ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Int ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Void ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState126 in
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
                        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                    | T_Rparen ->
                        _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                    | T_Bool | T_Char | T_Double | T_Int ->
                        _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Amp ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Break ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Const_Char _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
        | T_Const_Int _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
        | T_Const_Real _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
        | T_Const_String _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
        | T_Cont ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Del ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_False ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Id _v ->
            _menhir_run165 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
        | T_If ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Lbrace ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Lparen ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Mul ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_New ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Not ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Null ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Return ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Semicolon ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Sub ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_True ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_For ->
            _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | T_Rbrace ->
            _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
    | _ ->
        _menhir_fail ()

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) =                    (TYPE_int) in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) =                    (TYPE_double) in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) =                  (TYPE_char) in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Bool ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | T_Char ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | T_Double ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | T_Int ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | T_Void ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

