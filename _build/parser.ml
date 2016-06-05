
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
  | MenhirState207
  | MenhirState205
  | MenhirState202
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
  | MenhirState150
  | MenhirState143
  | MenhirState135
  | MenhirState134
  | MenhirState133
  | MenhirState128
  | MenhirState127
  | MenhirState125
  | MenhirState124
  | MenhirState123
  | MenhirState121
  | MenhirState118
  | MenhirState117
  | MenhirState116
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState109
  | MenhirState108
  | MenhirState107
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
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
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
            let _v : (Ast.ast_expr) =                                                                  ( let k = if is_some _3 then get_some1 _3 else [] in check_function_call (lookupEntry (id_make _1) LOOKUP_ALL_SCOPES true) k ;Eid _1) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
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
            | T_Semicolon ->
                _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState183
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
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | T_Rparen ->
                _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState185
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
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Break ->
                _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | T_Cont ->
                _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Id _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | T_If ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Lbrace ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Return ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Semicolon ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | T_For ->
                _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState188
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

and _menhir_goto_list_test9_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.ast_expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.ast_expr))) = _menhir_stack in
        let _v : (Ast.ast_expr list) =     ( x :: xs ) in
        _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState113 ->
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
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_decl))), _, (xs : (Ast.ast_program))) = _menhir_stack in
        let _v : (Ast.ast_program) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_declation_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr list option) =     ( None ) in
    _menhir_goto_option_expression_list_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr list) =     ( [] ) in
    _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce43 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.ast_expr)) * _menhir_state) * _menhir_state * (Ast.ast_expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                                        (Ecomma (_1,_3)) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState82
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

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84
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

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run98 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run102 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState80
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

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58
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

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64
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

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                            (EPlusPlus (_1,AFTER)) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState78
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

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                            (EMinusMinus (_1,AFTER)) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

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
                _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState181
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

and _menhir_reduce80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_goto_list_declation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Break ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | T_Cont ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Id _v ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | T_If ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Lbrace ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState154
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
        | T_Return ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Semicolon ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_For ->
            _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T_Rbrace ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ast_decl))), _, (xs : (Ast.ast_decl list))) = _menhir_stack in
        let _v : (Ast.ast_decl list) =     ( x :: xs ) in
        _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

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
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : ((bytes * Ast.ast_expr option) list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (bytes * Ast.ast_expr option))) = _menhir_stack in
        let _2 = () in
        let _v : ((bytes * Ast.ast_expr option) list) =                                          (_1::_3) in
        _menhir_goto_declator_plus _menhir_env _menhir_stack _menhir_s _v
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
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Break ->
                _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | T_Cont ->
                _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Id _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | T_If ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Lbrace ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Return ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Semicolon ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | T_For ->
                _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState177
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
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Break ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_Cont ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Id _v ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_If ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Lbrace ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Lparen ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Mul ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_New ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Not ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Null ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Return ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Semicolon ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_For ->
            _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Rbrace ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState195)
    | _ ->
        _menhir_fail ()

and _menhir_goto_declation : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState205 | MenhirState153 ->
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
            _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205)
    | MenhirState207 | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Char ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Double ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Int ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | T_Void ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState207
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

and _menhir_reduce14 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (string))) = _menhir_stack in
    let _v : (Ast.ast_expr) =                   (Eid _1) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

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
        _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_goto_expression1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Ast.ast_expr)) = _v in
    let _v : (Ast.ast_expr) =                         (ignore(get_type _1);_1) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | T_Add | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EUnAdd _2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Emult (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState61 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                                       (EArray (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | T_Add | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eminus (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Emod (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Colon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState71 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState71
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
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EPlusEq (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Eor (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eneq (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Ediv (_1,_3)) in
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
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Add | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eplus (_1,_3)) in
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
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Elte (_1,_3)) in
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
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Elt (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Egt (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Egte (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (Eeq (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eand (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EModEq (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                           (EMinusEq (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (EAssignEq (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EDotEq (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EDivEq (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                                           (EQuestT (_1,_3,_5)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EAmber _2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                            (EMinusMinus (_2,PRE)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EDel _2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState181 | MenhirState183 | MenhirState185 | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState113 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | T_Rparen | T_Semicolon ->
            _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Rparen | T_Semicolon ->
            _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState118
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
                let _menhir_s = MenhirState116 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Add ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | T_Amp ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | T_Const_Char _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
                | T_Const_Int _v ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
                | T_Const_Real _v ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
                | T_Const_String _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
                | T_Decr ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | T_Del ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | T_False ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState117
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
            | T_Rparen | T_Semicolon ->
                _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                            (EPlusPlus (_2,PRE)) in
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
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | T_Add | T_And | T_Colon | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))), _, (_4 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                              (ECast (_2,_4)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState125 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                         (_2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EPointer _2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T_Rbracket ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (Enot _2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | T_Add | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Quest | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EUnMinus _2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState135
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState159
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
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState164 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Break ->
                _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | T_Cont ->
                _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Id _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | T_If ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Lbrace ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Return ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Semicolon ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | T_For ->
                _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165)
        | T_Sub ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState164
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
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_And ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Comma ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Decr ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Div ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Div_eq ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Dot_eq ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Eq ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Geq ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Gr ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Incr ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Lbracket ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Le ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Leq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Minus_eq ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Mod ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Mod_eq ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Mul ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Neq ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Or ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_PlusEq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | T_Quest ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState191
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
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191)
    | _ ->
        _menhir_fail ()

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_stmt list) =     ( [] ) in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce92 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState156
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState156
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
    _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState160

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
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Id _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState163
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
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState163
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
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
    | T_Add | T_And | T_Comma | T_Decr | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Incr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Quest | T_Semicolon | T_Sub ->
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
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
        _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState168
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
        _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172

and _menhir_reduce67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_decl list) =     ( [] ) in
    _menhir_goto_list_declation_ _menhir_env _menhir_stack _menhir_s _v

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
            let _menhir_s = MenhirState143 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Lbracket ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
            | T_Comma | T_Semicolon ->
                _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
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

and _menhir_reduce5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) =           (ignore(closeScope();)) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState128 ->
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
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Symbol.entry))), _, (_3 : (unit))) = _menhir_stack in
        let _2 = () in
        let _v : (Symbol.entry) =                                                             (_1) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Symbol.entry)) = _v in
        let _v : (Ast.ast_decl) =                             (FunDecl _1) in
        _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((_menhir_stack, _menhir_s, (_1 : (Symbol.entry))), _, (_3 : (unit))), _, (_4 : (Ast.ast_decl list))), _, (_5 : (Ast.ast_stmt list))), _, (_6 : (unit))), _, (_8 : (unit))) = _menhir_stack in
        let _7 = () in
        let _2 = () in
        let _v : (Ast.ast_fun_def) =                                                                                                (
        
        
        (_1,_4,_5)) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.ast_fun_def)) = _v in
        let _v : (Ast.ast_decl) =                         (FunDef _1) in
        _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_expr) =                 (Ebool true) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

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
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

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
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
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
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

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
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (float)) = _v in
    let _v : (Ast.ast_expr) =                       (Ereal _1) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (int)) = _v in
    let _v : (Ast.ast_expr) =                      (Eint _1) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (string)) = _v in
    let _v : (Ast.ast_expr) =                       (Estring _1) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

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
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Char ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Double ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Int ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Void ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Break ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_Cont ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Id _v ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | T_If ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Lbrace ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState161
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
        | T_Return ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Semicolon ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_For ->
            _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Rbrace ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
    | _ ->
        _menhir_fail ()

and _menhir_reduce88 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _v : (Ast.ast_expr option) =     ( None ) in
    _menhir_goto_option_test_ _menhir_env _menhir_stack _v

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

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState9
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
        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState150
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
                        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState8
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
                        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState22
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
        | MenhirState207 | MenhirState205 | MenhirState153 | MenhirState2 ->
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
                        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                    | T_Bool | T_Char | T_Double | T_Int ->
                        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
                | T_Comma | T_Semicolon ->
                    _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack)
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
                _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState40
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
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_Amp ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_Const_Char _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | T_Const_Int _v ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | T_Const_Real _v ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | T_Const_String _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                | T_Decr ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_Del ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | T_False ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState123
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
        let _ = openScope() in
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
        let _ = openScope() in
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

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_reduce78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState202 ->
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
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
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
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
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
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
    | MenhirState105 ->
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
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
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
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
                _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | T_Bool | T_Char | T_Double | T_Int ->
                _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState5
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
  

