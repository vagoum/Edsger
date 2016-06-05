
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
  | MenhirState209
  | MenhirState205
  | MenhirState198
  | MenhirState195
  | MenhirState193
  | MenhirState189
  | MenhirState186
  | MenhirState183
  | MenhirState181
  | MenhirState179
  | MenhirState175
  | MenhirState170
  | MenhirState166
  | MenhirState163
  | MenhirState162
  | MenhirState161
  | MenhirState159
  | MenhirState158
  | MenhirState157
  | MenhirState154
  | MenhirState152
  | MenhirState151
  | MenhirState148
  | MenhirState147
  | MenhirState144
  | MenhirState136
  | MenhirState135
  | MenhirState134
  | MenhirState129
  | MenhirState128
  | MenhirState126
  | MenhirState125
  | MenhirState124
  | MenhirState122
  | MenhirState119
  | MenhirState118
  | MenhirState117
  | MenhirState116
  | MenhirState115
  | MenhirState114
  | MenhirState110
  | MenhirState109
  | MenhirState108
  | MenhirState107
  | MenhirState106
  | MenhirState102
  | MenhirState101
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
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState77
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
  | MenhirState49
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
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

let rec _menhir_goto_option_expression_list_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr list option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState46 ->
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
    | MenhirState179 ->
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
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState181
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
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | T_Rparen ->
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
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Break ->
                _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
            | T_Cont ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Id _v ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
            | T_If ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Lbrace ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Return ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Semicolon ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | T_For ->
                _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_test9_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.ast_expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.ast_expr))) = _menhir_stack in
        let _v : (Ast.ast_expr list) =     ( x :: xs ) in
        _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState114 ->
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
    | MenhirState166 ->
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
    | MenhirState170 ->
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

and _menhir_reduce83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr list option) =     ( None ) in
    _menhir_goto_option_expression_list_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr list) =     ( [] ) in
    _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) =           (ignore(closeScope();)) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState99 ->
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
    | MenhirState129 ->
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
    | MenhirState195 ->
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
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Rbrace ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (_1 : (unit))), _, (_2 : (Symbol.entry))), _, (_4 : (Ast.ast_decl list))), _, (_5 : (Ast.ast_stmt list))), _, (_6 : (unit))) = _menhir_stack in
            let _7 = () in
            let _3 = () in
            let _v : (Ast.ast_fun_def) =                                                                                         (
        
        
        (_2,_4,_5)) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.ast_fun_def)) = _v in
            let _v : (Ast.ast_decl) =                         (FunDef _1) in
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

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState59
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

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState95
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
    _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                            (EPlusPlus (_1,AFTER)) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run101 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState101
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

and _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                            (EMinusMinus (_1,AFTER)) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState93
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

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75
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

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65
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

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83
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
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85
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
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState87
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
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState89
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

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState91
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

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState69
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

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState71
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

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73
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

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_goto_list_statement_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_stmt list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState193 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_stmt))), _, (xs : (Ast.ast_stmt list))) = _menhir_stack in
        let _v : (Ast.ast_stmt list) =     ( x :: xs ) in
        _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState195
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_stmt) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState163 ->
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
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Break ->
                _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
            | T_Cont ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Id _v ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
            | T_If ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Lbrace ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Return ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Semicolon ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | T_For ->
                _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175)
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
    | MenhirState175 ->
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
    | MenhirState186 ->
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
    | MenhirState152 | MenhirState193 | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Break ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
        | T_Cont ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Id _v ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
        | T_If ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Lbrace ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Return ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Semicolon ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_For ->
            _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Rbrace ->
            _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState193)
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
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Semicolon ->
                _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179)
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

and _menhir_run167 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (string)) = _v in
    let _v : (bytes option) =     ( Some x ) in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_declator_plus : _menhir_env -> 'ttv_tail -> _menhir_state -> ((bytes * Ast.ast_expr option) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : ((bytes * Ast.ast_expr option) list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Types.typ))) = _menhir_stack in
        let _v : (Ast.ast_var_decl) =                                                  (List.map (fun x -> 
                let typeA = if is_some (get_second2(x)) then TYPE_array (_1,0) else _1 in (* later 0-> lenth,doesnt needed for semantics yet*)
                newVariable (id_make (get_first2 x)) typeA true) _2) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.ast_var_decl)) = _v in
        let _v : (Ast.ast_decl) =                              (VarDecl _1) in
        _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : ((bytes * Ast.ast_expr option) list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (bytes * Ast.ast_expr option))) = _menhir_stack in
        let _2 = () in
        let _v : ((bytes * Ast.ast_expr option) list) =                                          (_1::_3) in
        _menhir_goto_declator_plus _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState209 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_decl))), _, (xs : (Ast.ast_program))) = _menhir_stack in
        let _v : (Ast.ast_program) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_declation_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_declation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Break ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
        | T_Cont ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Id _v ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
        | T_If ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Lbrace ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Return ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Semicolon ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_For ->
            _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | T_Rbrace ->
            _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_decl))), _, (xs : (Ast.ast_decl list))) = _menhir_stack in
        let _v : (Ast.ast_decl list) =     ( x :: xs ) in
        _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce13 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (string))) = _menhir_stack in
    let _v : (Ast.ast_expr) =                  (Eid _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | T_Rparen ->
        _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EUnAdd _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Colon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState58 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eminus (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EPlusEq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EModEq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                           (EMinusEq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (EAssignEq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EDotEq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EDivEq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Eor (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eneq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Mod | T_Mul | T_Neq | T_Or | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eand (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Emult (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Elte (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Elt (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Egt (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Egte (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                        (Eeq (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eplus (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Emod (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Rbracket ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState99
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
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Ediv (_1,_3)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState107
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Colon | T_Decr | T_Incr | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EAmber _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Colon | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                            (EMinusMinus (_2,PRE)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Colon | T_Decr | T_Incr | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EDel _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState179 | MenhirState181 | MenhirState183 | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState114 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState115
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | T_Rparen | T_Semicolon ->
            _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState114
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
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | T_Rparen | T_Semicolon ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState119
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
                let _menhir_s = MenhirState117 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Add ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | T_Amp ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | T_Const_Char _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                | T_Const_Int _v ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                | T_Const_Real _v ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                | T_Const_String _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                | T_Decr ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | T_Del ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | T_False ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | T_Id _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                | T_Incr ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | T_Lparen ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | T_Mul ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | T_New ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | T_Not ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | T_Null ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | T_Sub ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | T_True ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
            | T_Rparen | T_Semicolon ->
                _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | T_Colon | T_Lbracket | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                            (EPlusPlus (_2,PRE)) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState125
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState126 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                         (_2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EPointer _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Rbracket ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (Enot _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Add | T_Colon | T_Decr | T_Div | T_Incr | T_Lbracket | T_Mod | T_Mul | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EUnMinus _2) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState136
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ast_expr))) = _menhir_stack in
            let _v : (Ast.ast_expr option) =     ( Some x ) in
            _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState162 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Break ->
                _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | T_Cont ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Id _v ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | T_If ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Lbrace ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Return ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Semicolon ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | T_For ->
                _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162)
    | MenhirState152 | MenhirState159 | MenhirState193 | MenhirState163 | MenhirState175 | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_And ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Comma ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Decr ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Div ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Div_eq ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Dot_eq ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Eq ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Equal ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Geq ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Gr ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Incr ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Lbracket ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Le ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Leq ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Minus_eq ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Mod ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Mod_eq ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Mul ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Neq ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Or ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_PlusEq ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Quest ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState189 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_stmt) =                                  (SExpr (Some _1)) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState189
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189)
    | _ ->
        _menhir_fail ()

and _menhir_reduce68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_stmt list) =     ( [] ) in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bytes option) =     ( None ) in
    _menhir_goto_option_test4_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run153 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_stmt) =                        (SExpr None) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run154 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState154 in
        let _v : (Ast.ast_expr option) =     ( None ) in
        _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154

and _menhir_run158 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState158

and _menhir_run160 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Id _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run164 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Semicolon | T_Sub ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run166 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
    | T_Semicolon ->
        _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166

and _menhir_run170 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
    | T_Semicolon ->
        _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170

and _menhir_goto_option_test_ : _menhir_env -> 'ttv_tail -> (Ast.ast_expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_2 : (Ast.ast_expr option)) = _v in
    let (_menhir_stack, _menhir_s, (_1 : (string))) = _menhir_stack in
    let _v : (bytes * Ast.ast_expr option) =                              ( (_1,_2)) in
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
            let _menhir_s = MenhirState144 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Lbracket ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
            | T_Comma | T_Semicolon ->
                _menhir_reduce87 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144)
    | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (bytes * Ast.ast_expr option))) = _menhir_stack in
        let _2 = () in
        let _v : ((bytes * Ast.ast_expr option) list) =                                     ([_1]) in
        _menhir_goto_declator_plus _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_test2_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Symbol.pass_mode * Types.typ * bytes) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Symbol.pass_mode * Types.typ * bytes) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Symbol.pass_mode * Types.typ * bytes))) = _menhir_stack in
        let _v : ((Symbol.pass_mode * Types.typ * bytes) list) =     ( x :: xs ) in
        _menhir_goto_list_test2_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 ->
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

and _menhir_goto_declation : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState205 | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Char ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Double ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Int ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Void ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205)
    | MenhirState209 | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | T_Char ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | T_Double ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | T_Int ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | T_Void ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | T_Eof ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ast_decl))) = _menhir_stack in
            let _v : (Ast.ast_program) =     ( [ x ] ) in
            _menhir_goto_nonempty_list_declation_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState209)
    | _ ->
        _menhir_fail ()

and _menhir_reduce66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_decl list) =     ( [] ) in
    _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) =                 (Ebool true) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState35
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
    let _v : (Ast.ast_expr) =                 (ENull) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState37
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
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState42
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
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Bool ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Char ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_Double ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState43
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
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState44
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

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Lparen ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Colon | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
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
    let _v : (Ast.ast_expr) =                  (Ebool false) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (string)) = _v in
    let _v : (Ast.ast_expr) =                         (Estring _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (float)) = _v in
    let _v : (Ast.ast_expr) =                       (Ereal _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (int)) = _v in
    let _v : (Ast.ast_expr) =                      (Eint _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (string)) = _v in
    let _v : (Ast.ast_expr) =                       (Estring _1) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_goto_option_test8_ : _menhir_env -> 'ttv_tail -> (Ast.ast_expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_3 : (Ast.ast_expr option)) = _v in
    let ((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))) = _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) =                               (ENew (_2,_3)) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) =           (ignore(openScope();)) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState41
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
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Id _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | MenhirState209 | MenhirState205 | MenhirState151 | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_Char ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_Double ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_Int ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | T_Void ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Break ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
        | T_Cont ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Id _v ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
        | T_If ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Lbrace ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Return ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Semicolon ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_For ->
            _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Rbrace ->
            _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159)
    | _ ->
        _menhir_fail ()

and _menhir_reduce87 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _v : (Ast.ast_expr option) =     ( None ) in
    _menhir_goto_option_test_ _menhir_env _menhir_stack _v

and _menhir_run30 : _menhir_env -> ('ttv_tail * _menhir_state * (Types.typ)) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Byref ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_Rparen ->
        _menhir_reduce85 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | T_Bool | T_Char | T_Double | T_Int ->
        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run33 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState33
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

and _menhir_reduce70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Symbol.pass_mode * Types.typ * bytes) list) =     ( [] ) in
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
        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState9
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
    match _menhir_s with
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Lbrace ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Bool ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Char ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Double ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Int ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Void ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
                _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState151
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
    | MenhirState209 | MenhirState2 | MenhirState205 | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Symbol.entry))) = _menhir_stack in
            let _2 = () in
            let _v : (Symbol.entry) =                                                      (_1) in
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

and _menhir_goto_list_T_Mul_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (unit list)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let x = () in
        let _v : (unit list) =     ( x :: xs ) in
        _menhir_goto_list_T_Mul_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (unit list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Types.typ))) = _menhir_stack in
        let _v : (Types.typ) =                           (if _2 = [] then _1 else List.fold_left (fun x->fun y-> TYPE_pointer x ) _1 _2) in
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
                let (_3 : (string)) = _v in
                let ((_menhir_stack, _menhir_s, (_1 : (unit option))), _, (_2 : (Types.typ))) = _menhir_stack in
                let _v : (Symbol.pass_mode * Types.typ * bytes) =                                 (if is_some _1 then (PASS_BY_REFERENCE,_2,_3) else (PASS_BY_VALUE ,_2,_3)) in
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
                        _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack) MenhirState8
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
                | MenhirState9 ->
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
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | T_Rparen ->
                        _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack) MenhirState22
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
        | MenhirState209 | MenhirState205 | MenhirState151 | MenhirState2 ->
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
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
                | T_Comma | T_Semicolon ->
                    _menhir_reduce87 _menhir_env (Obj.magic _menhir_stack)
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
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState40
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
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | T_Amp ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | T_Const_Char _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | T_Const_Int _v ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | T_Const_Real _v ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | T_Const_String _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | T_Decr ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | T_Del ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | T_False ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | T_Id _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | T_Incr ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | T_Lparen ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | T_Mul ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | T_New ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | T_Not ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | T_Null ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | T_Sub ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | T_True ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
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
            | T_Id _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState148 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Lparen ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
        | _ ->
            _menhir_fail ())
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_parameter_list_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Symbol.pass_mode * Types.typ * bytes) list option) -> 'ttv_return =
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
            let (((_menhir_stack, _menhir_s), (_2 : (string))), _, (_4 : ((Symbol.pass_mode * Types.typ * bytes) list option))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Symbol.entry) =                                                         (
        let e= newFunction (id_make _2) true in 
                 may (List.iter (fun x-> ignore (newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) _4 ;  
                 endFunctionHeader e TYPE_none; 
                 e ) in
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
            let (((_menhir_stack, _menhir_s, (_1 : (Types.typ))), _, (_2 : (string))), _, (_4 : ((Symbol.pass_mode * Types.typ * bytes) list option))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : (Symbol.entry) =                                                      (
        let e= newFunction (id_make _2) true in  
                may (List.iter (fun x-> ignore(newParameter (id_make (get_third x)) (get_second x) (get_first x) e true  ))) _4 ;  
                endFunctionHeader e _1 ; 
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

and _menhir_reduce64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) =     ( [] ) in
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
        _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Symbol.pass_mode * Types.typ * bytes) list option) =     ( None ) in
    _menhir_goto_option_parameter_list_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
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
        _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState209 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState193 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState189 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
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
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
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
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
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
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
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
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
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
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
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
        raise _eRR
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
                _menhir_reduce85 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | T_Bool | T_Char | T_Double | T_Int ->
                _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState5
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
    let _v : (Types.typ) =                    (TYPE_int) in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) =                    (TYPE_double) in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.typ) =                  (TYPE_char) in
    _menhir_goto_basic_type _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
  

