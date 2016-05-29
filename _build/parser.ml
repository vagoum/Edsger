
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
  | MenhirState178
  | MenhirState172
  | MenhirState169
  | MenhirState161
  | MenhirState160
  | MenhirState158
  | MenhirState150
  | MenhirState146
  | MenhirState144
  | MenhirState142
  | MenhirState140
  | MenhirState138
  | MenhirState134
  | MenhirState129
  | MenhirState125
  | MenhirState122
  | MenhirState121
  | MenhirState120
  | MenhirState118
  | MenhirState117
  | MenhirState111
  | MenhirState109
  | MenhirState108
  | MenhirState107
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState95
  | MenhirState94
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState75
  | MenhirState74
  | MenhirState64
  | MenhirState63
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState51
  | MenhirState49
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState39
  | MenhirState35
  | MenhirState34
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState20
  | MenhirState17
  | MenhirState16
  | MenhirState9
  | MenhirState7
  | MenhirState6
  | MenhirState3
  | MenhirState0
  
        open Lexing

let rec _menhir_goto_function_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit) =                        () in
    _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_T_Id_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (bytes option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState125 ->
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
    | MenhirState129 ->
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
    match _menhir_s with
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState138 ->
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
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_Amp ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_Const_Char _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
            | T_Const_Int _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
            | T_Const_Real _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
            | T_Const_String _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
            | T_Decr ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_Del ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_False ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_Id _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
            | T_Incr ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_Lparen ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_Mul ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_New ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_Not ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_Null ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_Sub ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_True ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | T_Semicolon ->
                _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_test3_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _6 = _v in
    let ((((_menhir_stack, _menhir_s), _, _3), _), _, _5) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : (unit) =                                                                                                                      () in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_test9_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState99 ->
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
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Amp ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Const_Char _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | T_Const_Int _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | T_Const_Real _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | T_Const_String _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | T_Decr ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Del ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_False ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Id _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | T_Incr ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Mul ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_New ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Not ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Null ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Sub ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_True ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_goto_binary_operator : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Amp ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Const_Char _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | T_Const_Int _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | T_Const_Real _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | T_Const_String _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | T_Decr ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Del ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_False ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Id _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | T_Incr ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Mul ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_New ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Not ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Null ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Sub ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_True ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_goto_list_statement_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState118 ->
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
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rbrace ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, _1), _, _2), _, _4), _, _7), _, _8) = _menhir_stack in
            let _9 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (unit) =                                                                                                () in
            _menhir_goto_function_def _menhir_env _menhir_stack _menhir_s _v
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
        | T_Rbrace ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _2), _, _4), _, _7), _, _8) = _menhir_stack in
            let _9 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                                                                                 () in
            _menhir_goto_function_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_Amp ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_Const_Char _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | T_Const_Int _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | T_Const_Real _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | T_Const_String _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | T_Decr ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_Del ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_False ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_Id _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | T_Incr ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_Lparen ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_Mul ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_New ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_Not ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_Null ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_Sub ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_True ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | T_Semicolon ->
                _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
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

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bytes option) =     ( None ) in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run126 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (bytes option) =     ( Some x ) in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_expression_list_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState122 ->
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
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Amp ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Break ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Const_Char _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | T_Const_Int _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | T_Const_Real _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | T_Const_String _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | T_Cont ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Decr ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Del ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_False ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Id _v ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | T_If ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Incr ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Lbrace ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Lparen ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Mul ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_New ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Not ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Null ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Return ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Semicolon ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_Sub ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_True ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | T_For ->
                _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
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
    | MenhirState134 ->
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
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, _1), _, _4), _, _6), _, _8), _, _10) = _menhir_stack in
        let _9 = () in
        let _7 = () in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _v : (unit) =                                                                                                                      () in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState172 | MenhirState35 | MenhirState150 | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Amp ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Break ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Const_Char _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | T_Const_Int _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | T_Const_Real _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | T_Const_String _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | T_Cont ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Del ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_False ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Id _v ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | T_If ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Lbrace ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Lparen ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Mul ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_New ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Not ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Null ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Return ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Semicolon ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Sub ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_True ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_For ->
            _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | T_Rbrace ->
            _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState39 ->
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
    | MenhirState140 ->
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
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_Amp ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_Const_Char _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | T_Const_Int _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | T_Const_Real _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | T_Const_String _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | T_Decr ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_Del ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_False ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_Id _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | T_Incr ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_Lparen ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_Mul ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_New ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_Not ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_Null ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_Sub ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_True ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | T_Rparen ->
                _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 ->
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
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Amp ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Break ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Const_Char _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
            | T_Const_Int _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
            | T_Const_Real _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
            | T_Const_String _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
            | T_Cont ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Decr ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Del ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_False ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Id _v ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
            | T_If ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Incr ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Lbrace ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Lparen ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Mul ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_New ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Not ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Null ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Return ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Semicolon ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_Sub ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_True ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | T_For ->
                _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Amp ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Const_Char _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_Const_Int _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_Const_Real _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_Const_String _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_Decr ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Del ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_False ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Id _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_Incr ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Mul ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_New ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Not ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Null ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Sub ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_True ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_binary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =               () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                        () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_binary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                      () in
    _menhir_goto_binary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =               () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (unit) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Amp ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Const_Char _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | T_Const_Int _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | T_Const_Real _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | T_Const_String _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | T_Decr ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Del ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_False ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Id _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | T_Incr ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Mul ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_New ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Not ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Null ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Sub ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_True ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =               () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                  () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_binary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_binary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_binary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                  () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                () in
    _menhir_goto_binary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_declator_plus : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState28 ->
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
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _2 = () in
        let _v : (unit) =                                          () in
        _menhir_goto_declator_plus _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_declation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
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
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _2 = () in
            let _v : (unit) =                           () in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_declation_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_test4_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                        () in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_Amp ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_Const_Char _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | T_Const_Int _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | T_Const_Real _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | T_Const_String _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | T_Decr ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_Del ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_False ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_Id _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | T_Incr ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_Mul ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_New ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_Not ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_Null ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_Sub ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_True ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | T_Semicolon ->
        _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run118 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Amp ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Break ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Const_Char _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | T_Const_Int _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | T_Const_Real _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | T_Const_String _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | T_Cont ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Decr ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Del ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_False ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Id _v ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | T_If ->
        _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Incr ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Lbrace ->
        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Mul ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_New ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Not ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Null ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Return ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Semicolon ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Sub ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_True ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_For ->
        _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | T_Rbrace ->
        _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118

and _menhir_run119 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Amp ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Const_Char _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | T_Const_Int _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | T_Const_Real _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | T_Const_String _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Del ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_False ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Id _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Lparen ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Mul ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_New ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Not ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Null ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Sub ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_True ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run123 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Semicolon | T_Sub ->
        _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | T_Semicolon ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125

and _menhir_run129 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
    | T_Semicolon ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129

and _menhir_reduce32 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
    let _v : (unit) =                  () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Amp ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Const_Char _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | T_Const_Int _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | T_Const_Real _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | T_Const_String _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | T_Decr ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Del ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_False ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Id _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | T_Incr ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Mul ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_New ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Not ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Null ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Sub ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_True ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Rparen ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_goto_unary_assig : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState172 | MenhirState160 | MenhirState35 | MenhirState150 | MenhirState118 | MenhirState144 | MenhirState142 | MenhirState140 | MenhirState138 | MenhirState134 | MenhirState122 | MenhirState120 | MenhirState39 | MenhirState44 | MenhirState107 | MenhirState46 | MenhirState103 | MenhirState100 | MenhirState49 | MenhirState51 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState74 | MenhirState63 | MenhirState60 | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Amp ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Const_Char _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | T_Const_Int _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | T_Const_Real _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | T_Const_String _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Del ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_False ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Id _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Lparen ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Mul ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_New ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Not ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Null ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Sub ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_True ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState161 | MenhirState121 | MenhirState146 | MenhirState117 | MenhirState111 | MenhirState109 | MenhirState108 | MenhirState99 | MenhirState101 | MenhirState95 | MenhirState94 | MenhirState61 | MenhirState64 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState75 ->
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
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
            let _v : (unit) =                                 () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Colon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState64 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Amp ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Const_Char _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | T_Const_Int _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | T_Const_Real _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | T_Const_String _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | T_Decr ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Del ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_False ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Id _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | T_Incr ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Lparen ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Mul ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_New ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Not ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Null ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_Sub ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | T_True ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState75 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _1), _), _, _3) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (unit) =                                                      () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : (unit) =                                                () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _1), _, _2), _, _3) = _menhir_stack in
            let _v : (unit) =                                             () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, _1), _), _, _3), _), _, _5) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (unit) =                                                           () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1), _, _2) = _menhir_stack in
            let _v : (unit) =                                    () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Colon | T_Decr | T_Incr | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                           () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState138 | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState99 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | T_Amp ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | T_Const_Char _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | T_Const_Int _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | T_Const_Real _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | T_Const_String _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | T_Decr ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | T_Del ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | T_False ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | T_Id _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | T_Incr ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | T_Lparen ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | T_Mul ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | T_New ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | T_Not ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | T_Null ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | T_Sub ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | T_True ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Rparen | T_Semicolon ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | MenhirState103 | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Rparen | T_Semicolon ->
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
                let _menhir_s = MenhirState102 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Add ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | T_Amp ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | T_Const_Char _v ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
                | T_Const_Int _v ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
                | T_Const_Real _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
                | T_Const_String _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
                | T_Decr ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | T_Del ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | T_False ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | T_Id _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
                | T_Incr ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | T_Lparen ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | T_Mul ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | T_New ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | T_Not ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | T_Null ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | T_Sub ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | T_True ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
            | T_Rparen | T_Semicolon ->
                _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, _2), _, _4) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                              () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState109 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) =                                         () in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState111 in
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
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
    | MenhirState140 | MenhirState142 | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (unit option) =     ( Some x ) in
            _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState121 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Amp ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Break ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Const_Char _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | T_Const_Int _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | T_Const_Real _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | T_Const_String _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | T_Cont ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Decr ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Del ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_False ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Id _v ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | T_If ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Incr ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Lbrace ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Lparen ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Mul ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_New ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Not ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Null ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Return ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Semicolon ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_Sub ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_True ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | T_For ->
                _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
    | MenhirState172 | MenhirState35 | MenhirState118 | MenhirState150 | MenhirState122 | MenhirState134 | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState146 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _2 = () in
            let _v : (unit) =                                  () in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146)
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_And ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Comma ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Div ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Div_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Dot_eq ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Equal ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Geq ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Gr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Lbracket ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Le ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Leq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Minus_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Mod ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Mod_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Mul ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Neq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Or ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_PlusEq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Quest ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState161
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
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
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Amp ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Const_Char _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Const_Int _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Const_Real _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Const_String _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Decr ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Del ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_False ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Id _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Incr ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Mul ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_New ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Not ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Null ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Sub ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_True ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

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
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState169 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Lbracket ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack)
            | T_Comma | T_Semicolon ->
                _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169)
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

and _menhir_goto_list_test2_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_test2_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState6 ->
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

and _menhir_goto_declation : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 | MenhirState158 | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Char ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Double ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Int ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Void ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
    | MenhirState178 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Char ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Double ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Int ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Void ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | T_Eof ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (unit list) =     ( [ x ] ) in
            _menhir_goto_nonempty_list_declation_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_declation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Amp ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Break ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Const_Char _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | T_Const_Int _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | T_Const_Real _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | T_Const_String _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | T_Cont ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Del ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_False ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Id _v ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | T_If ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Lbrace ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Lparen ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Mul ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_New ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Not ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Null ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Return ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Semicolon ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Sub ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_True ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_For ->
            _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | T_Rbrace ->
            _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Amp ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Break ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Const_Char _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
        | T_Const_Int _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
        | T_Const_Real _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
        | T_Const_String _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
        | T_Cont ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Decr ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Del ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_False ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Id _v ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
        | T_If ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Incr ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Lbrace ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Lparen ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Mul ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_New ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Not ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Null ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Return ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Semicolon ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Sub ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_True ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_For ->
            _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | T_Rbrace ->
            _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_test8_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _3 = _v in
    let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
    let _1 = () in
    let _v : (unit) =                               () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_unary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_unary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Bool ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Char ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Double ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Int ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_unary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Amp ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Bool ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Char ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Const_Char _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_Int _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_Real _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_String _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Decr ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Del ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Double ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_False ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Id _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Incr ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Int ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Mul ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_New ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Not ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Null ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Sub ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_True ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                     () in
    _menhir_goto_unary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Lparen ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                  () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_Amp ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_Const_Char _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | T_Const_Int _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | T_Const_Real _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | T_Const_String _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | T_Decr ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_Del ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_False ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_Id _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | T_Incr ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_Mul ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_New ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_Not ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_Null ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_Sub ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | T_True ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_unary_assig _menhir_env _menhir_stack _menhir_s _v

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit) =                         () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit) =                       () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit) =                      () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit) =                       () in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                       () in
    _menhir_goto_unary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                 () in
    _menhir_goto_unary_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce79 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_test_ _menhir_env _menhir_stack _v

and _menhir_run160 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Amp ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Const_Char _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
    | T_Const_Int _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
    | T_Const_Real _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
    | T_Const_String _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
    | T_Decr ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Del ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_False ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Id _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
    | T_Incr ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Mul ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_New ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Not ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Null ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_Sub ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | T_True ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160

and _menhir_reduce63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_test2_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Byref ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | T_Bool | T_Char | T_Double | T_Int ->
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_function_declation : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (unit) =                              () in
    _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_T_Mul_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let x = () in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_T_Mul_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (unit) =                           () in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState9 ->
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
                | MenhirState30 | MenhirState3 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | T_Comma ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                    | T_Rparen ->
                        _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
                | MenhirState7 ->
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
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20
                    | T_Rparen ->
                        _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState20
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
                | _ ->
                    _menhir_fail ())
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState178 | MenhirState0 | MenhirState158 | MenhirState34 | MenhirState26 ->
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
                    _menhir_run160 _menhir_env (Obj.magic _menhir_stack)
                | T_Lparen ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | T_Byref ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                    | T_Rparen ->
                        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                    | T_Bool | T_Char | T_Double | T_Int ->
                        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
                | T_Comma | T_Semicolon ->
                    _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack)
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
        | MenhirState42 ->
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
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | T_Amp ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | T_Const_Char _v ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
                | T_Const_Int _v ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
                | T_Const_Real _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
                | T_Const_String _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
                | T_Decr ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | T_Del ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | T_False ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | T_Id _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
                | T_Incr ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | T_Lparen ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | T_Mul ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | T_New ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | T_Not ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | T_Null ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | T_Sub ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | T_True ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
            | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (unit option) =     ( None ) in
                _menhir_goto_option_test8_ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState46 ->
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
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | T_Amp ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | T_Const_Char _v ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                | T_Const_Int _v ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                | T_Const_Real _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                | T_Const_String _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                | T_Decr ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | T_Del ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | T_False ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | T_Id _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                | T_Incr ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | T_Lparen ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | T_Mul ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | T_New ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | T_Not ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | T_Null ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | T_Sub ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | T_True ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
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

and _menhir_goto_option_parameter_list_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
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
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Bool ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | T_Char ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | T_Double ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | T_Int ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | T_Void ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
                    _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
            | T_Semicolon ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _2), _, _4) = _menhir_stack in
                let _6 = () in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (unit) =                                                                    () in
                _menhir_goto_function_declation _menhir_env _menhir_stack _menhir_s _v
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
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Bool ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | T_Char ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | T_Double ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | T_Int ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | T_Void ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
                    _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
            | T_Semicolon ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _1), _, _2), _, _4) = _menhir_stack in
                let _6 = () in
                let _5 = () in
                let _3 = () in
                let _v : (unit) =                                                                   () in
                _menhir_goto_function_declation _menhir_env _menhir_stack _menhir_s _v
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
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | T_Char ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | T_Double ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | T_Int ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_reduce57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
    _menhir_goto_list_T_Mul_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Mul ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Id _ | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_parameter_list_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Id _ | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
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
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | T_Rparen ->
                _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | T_Bool | T_Char | T_Double | T_Int ->
                _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
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

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                    () in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =                  () in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | T_Char ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | T_Double ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | T_Int ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | T_Void ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

