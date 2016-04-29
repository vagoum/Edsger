
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
  | T_Const_Real of (string)
  | T_Const_Int of (string)
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
  | MenhirState169
  | MenhirState165
  | MenhirState157
  | MenhirState153
  | MenhirState151
  | MenhirState149
  | MenhirState147
  | MenhirState145
  | MenhirState141
  | MenhirState136
  | MenhirState132
  | MenhirState129
  | MenhirState128
  | MenhirState127
  | MenhirState125
  | MenhirState124
  | MenhirState121
  | MenhirState119
  | MenhirState118
  | MenhirState112
  | MenhirState109
  | MenhirState107
  | MenhirState106
  | MenhirState103
  | MenhirState99
  | MenhirState91
  | MenhirState87
  | MenhirState84
  | MenhirState83
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState47
  | MenhirState46
  | MenhirState36
  | MenhirState35
  | MenhirState33
  | MenhirState32
  | MenhirState31
  | MenhirState23
  | MenhirState21
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState9
  | MenhirState7
  | MenhirState0
  
        open Lexing

let rec _menhir_goto_list_test9_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit) =                                    () in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_expression_list_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_binary_assig : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
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
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Not ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Null ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Sub ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_True ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_goto_binary_operator : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
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
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Not ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Null ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Sub ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_True ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_goto_option_test3_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _6 = _v in
    let ((((_menhir_stack, _menhir_s), _, _3), _), _, _5) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : (unit) =                                                                                                                 () in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_T_Id_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (bytes option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                    () in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
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
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                    () in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_expression_list_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
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
        let ((_menhir_stack, _menhir_s, _1), _, _3) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (unit) =                                                   () in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                           () in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
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
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_Id _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_New ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_Not ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_Null ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_Sub ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_True ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | T_Semicolon ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState147 ->
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
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_Id _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_New ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_Not ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_Null ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_Sub ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_True ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | T_Rparen ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState149 ->
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
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Break ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | T_Cont ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Id _v ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | T_If ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Lbrace ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_New ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Not ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Null ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Return ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Semicolon ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Sub ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_True ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_For ->
                _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_New ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Not ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Null ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Sub ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_True ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_binary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =               () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                        () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_binary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                      () in
    _menhir_goto_binary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =               () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
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
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Not ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Null ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Sub ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_True ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =               () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                  () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_binary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_binary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_binary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                  () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_declator_plus : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit) =                                          () in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (unit) =                               () in
        _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _2 = () in
        let _v : (unit) =                                          () in
        _menhir_goto_declator_plus _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_statement_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rbrace ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                         () in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rbrace ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, _1), _2), _, _4), _, _7), _, _8) = _menhir_stack in
            let _9 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (unit) =                                                                                                           () in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            let _v : (unit) =                        () in
            _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState129 ->
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
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Break ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | T_Cont ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Id _v ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | T_If ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Lbrace ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_New ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Not ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Null ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Return ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Semicolon ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_Sub ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_True ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | T_For ->
                _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit option) =     ( None ) in
            _menhir_goto_option_test3_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _2) = _menhir_stack in
        let _1 = () in
        let _v : (unit) =                        () in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_test3_ _menhir_env _menhir_stack _v
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, _1), _, _4), _, _6), _, _8), _, _10) = _menhir_stack in
        let _9 = () in
        let _7 = () in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _v : (unit) =                                                                                                                 () in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState119 | MenhirState157 | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Amp ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Break ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Const_Char _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
        | T_Const_Int _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
        | T_Const_Real _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
        | T_Const_String _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
        | T_Cont ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Del ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_False ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Id _v ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
        | T_If ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Lbrace ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Lparen ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Mul ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_New ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Not ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Null ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Return ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Semicolon ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Sub ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_True ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_For ->
            _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Rbrace ->
            _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)
    | _ ->
        _menhir_fail ()

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_test4_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
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
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_Id _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_New ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_Not ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_Null ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_Sub ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_True ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | T_Semicolon ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
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

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bytes option) =     ( None ) in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run133 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (bytes option) =     ( Some x ) in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_test2_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_test2_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit) =                                  () in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_parameter_list_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce32 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
    let _v : (unit) =                  () in
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
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Not ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Null ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Sub ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_True ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | T_Rparen ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState21 in
        let _v : (unit option) =     ( None ) in
        _menhir_goto_option_expression_list_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_goto_unary_assig : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState119 | MenhirState157 | MenhirState125 | MenhirState151 | MenhirState149 | MenhirState147 | MenhirState145 | MenhirState141 | MenhirState129 | MenhirState127 | MenhirState121 | MenhirState9 | MenhirState16 | MenhirState79 | MenhirState18 | MenhirState75 | MenhirState72 | MenhirState21 | MenhirState23 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState46 | MenhirState35 | MenhirState32 | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | T_Not ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | T_Null ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | T_Sub ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | T_True ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | MenhirState128 | MenhirState153 | MenhirState124 | MenhirState91 | MenhirState87 | MenhirState81 | MenhirState80 | MenhirState71 | MenhirState73 | MenhirState67 | MenhirState66 | MenhirState33 | MenhirState36 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
        let _v : (unit) =                                 () in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | T_Colon | T_Rbrace | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
            let _v : (unit) =                                 () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Colon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState36 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
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
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | T_Not ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | T_Null ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | T_Sub ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | T_True ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | T_Rbrace ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState47 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _1), _), _, _3) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (unit) =                                                  () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Colon | T_Rbrace | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : (unit) =                                                () in
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
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Colon | T_Rbrace | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : (unit) =                                             () in
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
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Rbrace | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, _1), _), _, _3), _), _, _5) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (unit) =                                                           () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Colon | T_Rbrace | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
            let _v : (unit) =                                    () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Mod | T_Mul | T_Rbrace | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                           () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState71 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | T_Id _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | T_New ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | T_Not ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | T_Null ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | T_Sub ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | T_True ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Rparen ->
            _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState75 | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                           () in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Comma ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState74 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Add ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | T_Amp ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | T_Const_Char _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
                | T_Const_Int _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
                | T_Const_Real _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
                | T_Const_String _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
                | T_Decr ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | T_Del ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | T_False ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | T_Id _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
                | T_Incr ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | T_Lparen ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | T_Mul ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | T_New ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | T_Not ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | T_Null ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | T_Sub ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | T_True ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
            | T_Rparen ->
                _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Colon | T_Rbrace | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, _2), _, _4) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                              () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState81 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                         () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState87 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                          () in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let x = _v in
            let _v : (unit option) =     ( Some x ) in
            _menhir_goto_option_test8_ _menhir_env _menhir_stack _v
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (unit) =                                () in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Rbracket ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _2) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (unit) =                                                 () in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = _v in
                let _v : (unit option) =     ( Some x ) in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState145 | MenhirState147 | MenhirState149 | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (unit option) =     ( Some x ) in
            _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState128 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Amp ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Break ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Const_Char _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | T_Const_Int _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | T_Const_Real _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | T_Const_String _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | T_Cont ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Decr ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Del ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_False ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Id _v ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | T_If ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Incr ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Lbrace ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Lparen ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Mul ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_New ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Not ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Null ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Return ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Semicolon ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_Sub ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_True ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | T_For ->
                _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState119 | MenhirState125 | MenhirState157 | MenhirState129 | MenhirState141 | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_And ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Comma ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Div ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Div_eq ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Dot_eq ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Eq ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Equal ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Geq ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Gr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Lbrace ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Le ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Leq ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Minus_eq ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Mod ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Mod_eq ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Mul ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Neq ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Or ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_PlusEq ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Quest ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState153 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _2 = () in
            let _v : (unit) =                                  () in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | _ ->
        _menhir_fail ()

and _menhir_goto_unary_operator : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_New ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Not ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Null ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_Sub ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | T_True ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_goto_option_test_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _2 = _v in
    let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
    let _v : (unit) =                             () in
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
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _2 = () in
        let _v : (unit) =                                     () in
        _menhir_goto_declator_plus _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_test4_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run120 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                        () in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run121 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_New ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_Not ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_Null ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_Sub ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_True ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | T_Semicolon ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Amp ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Break ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Const_Char _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | T_Const_Int _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | T_Const_Real _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | T_Const_String _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | T_Cont ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Decr ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Del ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Id _v ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | T_If ->
        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Lbrace ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_New ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Not ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Null ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Return ->
        _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Semicolon ->
        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Sub ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_True ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_For ->
        _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | T_Rbrace ->
        _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125

and _menhir_run126 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Amp ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Const_Char _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | T_Const_Int _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | T_Const_Real _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | T_Const_String _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Del ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_False ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Id _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Lparen ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Mul ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_New ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Not ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Null ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Sub ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_True ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run130 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Colon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _2 = () in
        let _v : (unit) =                     () in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_test4_ _menhir_env _menhir_stack _menhir_s _v
    | T_Lparen ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbrace | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Semicolon | T_Sub ->
        _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run132 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | T_Semicolon ->
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132

and _menhir_run136 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | T_Semicolon ->
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136

and _menhir_reduce61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_test2_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Byref ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | T_Bool | T_Char | T_Double | T_Int ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_goto_option_test8_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _3 = _v in
    let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
    let _1 = () in
    let _v : (unit) =                               () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_unary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_unary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Bool ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_Char ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_Double ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | T_Int ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_unary_operator _menhir_env _menhir_stack _menhir_s _v

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
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Char ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
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
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_False ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Id _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | T_Incr ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Int ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Lparen ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_New ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Not ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Null ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_Sub ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | T_True ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                     () in
    _menhir_goto_unary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Lparen ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbrace | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbrace | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
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
    let _v : (unit) =                  () in
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
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Not ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Null ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_Sub ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | T_True ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_unary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit) =                         () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit) =                       () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit) =                      () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit) =                       () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                       () in
    _menhir_goto_unary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_unary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | T_Amp ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | T_Const_Char _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | T_Const_Int _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | T_Const_Real _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | T_Const_String _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | T_Del ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | T_False ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | T_Id _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | T_Lparen ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | T_Mul ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | T_New ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | T_Not ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | T_Null ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | T_Sub ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | T_True ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
    | T_Comma | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (unit option) =     ( None ) in
        _menhir_goto_option_test_ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_list_declation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Amp ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Break ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Const_Char _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
        | T_Const_Int _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
        | T_Const_Real _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
        | T_Const_String _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
        | T_Cont ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Decr ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Del ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_False ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Id _v ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
        | T_If ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Incr ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Lbrace ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Lparen ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Mul ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_New ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Not ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Null ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Return ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Semicolon ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Sub ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_True ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_For ->
            _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Rbrace ->
            _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_T_Mul_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let x = () in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_T_Mul_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit) =                           () in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState169 | MenhirState165 | MenhirState118 | MenhirState0 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Id _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
        | MenhirState14 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Lbracket ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
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
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                | T_Not ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                | T_Null ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                | T_Sub ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                | T_True ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
            | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbrace | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbrace | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (unit option) =     ( None ) in
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
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | T_Amp ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | T_Const_Char _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                | T_Const_Int _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                | T_Const_Real _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                | T_Const_String _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                | T_Decr ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | T_Del ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | T_False ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | T_Id _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                | T_Incr ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | T_Lparen ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | T_Mul ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | T_New ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | T_Not ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | T_Null ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | T_Sub ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | T_True ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState109 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Id _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _3 = _v in
                let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
                let _v : (unit) =                                 () in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                (match _menhir_s with
                | MenhirState103 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | T_Comma ->
                        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                    | T_Rparen ->
                        _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
                | MenhirState107 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
                    let _1 = () in
                    let _v : (unit) =                          () in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | T_Comma ->
                        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                    | T_Rparen ->
                        _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
                | _ ->
                    _menhir_fail ())
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

and _menhir_goto_declation : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState165 | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Char ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Double ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Int ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Void ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165)
    | MenhirState169 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | T_Char ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | T_Double ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | T_Int ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | T_Void ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169)
    | _ ->
        _menhir_fail ()

and _menhir_reduce57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_T_Byref_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Bool ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_Char ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_Double ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | T_Int ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_reduce55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_T_Mul_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Mul ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Id _ | T_Incr | T_Lbrace | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbrace | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce55 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_reduce67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_parameter_list_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Rparen ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Lbrace ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Bool ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_Char ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_Double ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_Int ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_Void ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
                _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _1), _2), _, _4) = _menhir_stack in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (unit) =                                                                                     () in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            let _v : (unit) =                              () in
            _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_run104 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_basic_type : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Mul ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Id _ | T_Incr | T_Lbrace | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbrace | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce55 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
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
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
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
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
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
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                  () in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
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
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | T_Rparen ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState103 in
                let _v : (unit option) =     ( None ) in
                _menhir_goto_option_parameter_list_ _menhir_env _menhir_stack _menhir_s _v
            | T_Bool | T_Char | T_Double | T_Int ->
                _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                  () in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                  () in
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
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | T_Char ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | T_Double ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | T_Int ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | T_Void ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

