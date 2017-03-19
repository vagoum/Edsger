
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
  | Special_Quest
  | NonElse
  | Incr_dcr_prefix
  | Incr_dcr_postfix
  | Fuction_Call
  | Cast_
  | Array_place
  | Adress_etc

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
  | MenhirState193
  | MenhirState190
  | MenhirState187
  | MenhirState185
  | MenhirState183
  | MenhirState179
  | MenhirState174
  | MenhirState170
  | MenhirState167
  | MenhirState166
  | MenhirState165
  | MenhirState163
  | MenhirState162
  | MenhirState161
  | MenhirState158
  | MenhirState156
  | MenhirState155
  | MenhirState154
  | MenhirState152
  | MenhirState145
  | MenhirState137
  | MenhirState136
  | MenhirState135
  | MenhirState130
  | MenhirState129
  | MenhirState127
  | MenhirState126
  | MenhirState125
  | MenhirState123
  | MenhirState120
  | MenhirState119
  | MenhirState118
  | MenhirState117
  | MenhirState116
  | MenhirState115
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
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
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
        open Lamda_lift


        let get_first (x,_,_) =x ;;
        let get_first2 (x,_) =x ;;
        let get_second (_,x,_)=x;;
        let get_second2 (_,x)=x;;
        let get_third (_,_,x)=x;;

let rec _menhir_goto_list_test9_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.ast_expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.ast_expr))) = _menhir_stack in
        let _v : (Ast.ast_expr list) =     ( x :: xs ) in
        _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState115 ->
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
            let _v : (Ast.ast_stmt) =                                    (if !nested_loops =0 then (error "No continue in Loop"; SCont _2) else SCont _2) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState174 ->
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

and _menhir_goto_option_expression_list_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr list option) -> 'ttv_return =
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
        let ((_menhir_stack, _menhir_s, (_1 : (string))), _, (_3 : (Ast.ast_expr list option))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (Ast.ast_expr) =                                                                          ( let k = if is_some _3 then get_some1 _3 else [] in check_function_call (lookupEntry (id_make _1) LOOKUP_ALL_SCOPES true) k ;ECall (_1,_3)) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.ast_expr)) = _v in
        let _v : (Ast.ast_expr) =                       (_1) in
        _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast_expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState158 ->
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
            let _v : (Ast.ast_stmt) =                                           (Sreturn _2) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
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
            | T_Semicolon ->
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
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | T_Rparen ->
                _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState187
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
    | MenhirState187 ->
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
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Break ->
                _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
            | T_Cont ->
                _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Id _v ->
                _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
            | T_If ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Lbrace ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Return ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Semicolon ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | T_For ->
                _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr list) =     ( [] ) in
    _menhir_goto_list_test9_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce44 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.ast_expr)) * _menhir_state) * _menhir_state * (Ast.ast_expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                                        (Ecomma (_1,_3)) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

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
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState97
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
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState99
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
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState103
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

and _menhir_run105 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState105
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

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                                                 (EPlusPlus (_1,AFTER)) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast_expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ast_expr) =                                                 (EMinusMinus (_1,AFTER)) in
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
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState197
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState200
    | _ ->
        _menhir_fail ()

and _menhir_reduce84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_expr option) =     ( None ) in
    _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v

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
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bytes option) =     ( None ) in
    _menhir_goto_option_T_Id_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run171 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Break ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
        | T_Cont ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Id _v ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
        | T_If ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Lbrace ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState156
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
        | T_Return ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Semicolon ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_For ->
            _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | T_Rbrace ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156)
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
                let typeA = if is_some (get_second2(x)) then (ignore(check_array_len (get_second2(x))); TYPE_array (_1,0) ) else _1 in (* later 0-> lenth,doesnt needed for semantics yet*)
                newVariable (id_make (get_first2 x)) typeA true) _2) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.ast_var_decl)) = _v in
        let _v : (Ast.ast_decl) =                              (VarDecl _1) in
        _menhir_goto_declation _menhir_env _menhir_stack _menhir_s _v
    | MenhirState145 ->
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
    | MenhirState167 ->
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
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Break ->
                _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | T_Cont ->
                _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Id _v ->
                _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | T_If ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Lbrace ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState179
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
            | T_Return ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Semicolon ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | T_For ->
                _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179)
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_stmt))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast_stmt) =                                                                      (Sif (_3,_5,None)) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_stmt))), _, (_7 : (Ast.ast_stmt))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.ast_stmt) =                                                                         (Sif (_3,_5,Some _7)) in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (unit) =           (ignore(nested_loops := !nested_loops -1)) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_12 : (unit)) = _v in
        let ((((((_menhir_stack, _menhir_s, (_1 : (bytes option))), _, (_4 : (Ast.ast_expr option))), _, (_6 : (Ast.ast_expr option))), _, (_8 : (Ast.ast_expr option))), (_10 : (unit))), _, (_11 : (Ast.ast_stmt))) = _menhir_stack in
        let _9 = () in
        let _7 = () in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ast.ast_stmt) =                                                                                                                                (Sfor (_1,_4,_6,_8,_11)) in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState156 | MenhirState195 | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Break ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_Cont ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Id _v ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | T_If ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Lbrace ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState195
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
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Semicolon ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_For ->
            _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | T_Rbrace ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState195
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
    | MenhirState205 | MenhirState155 ->
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
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState205
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

and _menhir_reduce16 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (string))) = _menhir_stack in
    let _v : (Ast.ast_expr) =               (Eid _1) in
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
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState46 in
        let _v : (Ast.ast_expr list option) =     ( None ) in
        _menhir_goto_option_expression_list_ _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                           (EUnAdd _2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Colon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState60 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState107
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
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
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | T_Add | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eminus (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Emult (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T_Rbracket ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState66 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                                                     (if (get_type _2)=TYPE_int then () else error "Not an int on array";_2) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (Ast.ast_expr)) = _v in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
            let _v : (Ast.ast_expr) =                                     (EArray (_1,_2)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EPlusEq (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Eor (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eneq (_1,_3)) in
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
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Emod (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EModEq (_1,_3)) in
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                           (EMinusEq (_1,_3)) in
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Elte (_1,_3)) in
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
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Ediv (_1,_3)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T_Add | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod_eq | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eplus (_1,_3)) in
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
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Elt (_1,_3)) in
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (Egt (_1,_3)) in
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Egte (_1,_3)) in
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (Eeq (_1,_3)) in
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                     (EAssignEq (_1,_3)) in
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EDotEq (_1,_3)) in
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | T_Colon | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                         (EDivEq (_1,_3)) in
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | T_And | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                      (Eand (_1,_3)) in
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | T_Colon | T_Div_eq | T_Dot_eq | T_Eq | T_Minus_eq | T_Mod_eq | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))), _), _, (_3 : (Ast.ast_expr))), _), _, (_5 : (Ast.ast_expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast_expr) =                                                                               (EQuestT (_1,_3,_5)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EAmber _2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                                  (EMinusMinus (_2,PRE)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                           (EDel _2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState115 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | T_Id _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | T_Rparen ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | T_Rparen ->
            _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T_Rparen ->
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
                let _menhir_s = MenhirState118 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | T_Add ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | T_Amp ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | T_Const_Char _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | T_Const_Int _v ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | T_Const_Real _v ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | T_Const_String _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | T_Decr ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | T_Del ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | T_False ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | T_Id _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | T_Incr ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | T_Lparen ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | T_Mul ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | T_New ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | T_Not ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | T_Null ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | T_Sub ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | T_True ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
            | T_Rparen ->
                _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Lbracket | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                                  (EPlusPlus (_2,PRE)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Types.typ))), _, (_4 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                                          (ECast (_2,_4)) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState127 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                         (_2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                           (EPointer _2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | T_Rbracket ->
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                           (Enot _2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | T_Add | T_And | T_Colon | T_Div | T_Div_eq | T_Dot_eq | T_Eq | T_Equal | T_Geq | T_Gr | T_Le | T_Leq | T_Minus_eq | T_Mod | T_Mod_eq | T_Mul | T_Neq | T_Or | T_PlusEq | T_Rbracket | T_Rparen | T_Semicolon | T_Sub ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ast_expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast_expr) =                                           (EUnMinus _2) in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState137
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
    | MenhirState183 | MenhirState185 | MenhirState187 | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | T_Rparen | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ast_expr))) = _menhir_stack in
            let _v : (Ast.ast_expr option) =     ( Some x ) in
            _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | T_Rparen ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState166 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Add ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Amp ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Break ->
                _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Const_Char _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
            | T_Const_Int _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
            | T_Const_Real _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
            | T_Const_String _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
            | T_Cont ->
                _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Decr ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Del ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_False ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Id _v ->
                _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
            | T_If ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Incr ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Lbrace ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Lparen ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Mul ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_New ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Not ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Null ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Return ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Semicolon ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_Sub ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_True ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | T_For ->
                _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState167)
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166)
    | MenhirState156 | MenhirState163 | MenhirState195 | MenhirState167 | MenhirState179 | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_And ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Comma ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Decr ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Div ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Div_eq ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Dot_eq ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Eq ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Equal ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Geq ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Gr ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Incr ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Lbracket ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Le ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Leq ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Minus_eq ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Mod ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Mod_eq ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Mul ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Neq ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Or ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_PlusEq ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Quest ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | T_Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState193 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ast_expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast_stmt) =                                  (SExpr (Some _1)) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | T_Sub ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState193
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState193)
    | _ ->
        _menhir_fail ()

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast_stmt list) =     ( [] ) in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce92 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bytes option) =     ( None ) in
    _menhir_goto_option_test4_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run157 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast_stmt) =                        (SExpr None) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run158 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Add ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Amp ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Const_Char _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | T_Const_Int _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | T_Const_Real _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | T_Const_String _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | T_Decr ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Del ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_False ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Id _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | T_Incr ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Lparen ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Mul ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_New ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Not ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Null ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Sub ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_True ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | T_Semicolon ->
        _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158

and _menhir_run162 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState162

and _menhir_run164 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_Id _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState165
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
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run168 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run170 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
    | T_Semicolon ->
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170

and _menhir_run174 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | T_Id _v ->
        _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
    | T_Semicolon ->
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            let _menhir_s = MenhirState145 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | T_Lbracket ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
            | T_Comma | T_Semicolon ->
                _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
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

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) =           (ignore(closeScope();)) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState130 ->
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
    | MenhirState152 ->
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
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState202
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
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
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
    let _v : (Ast.ast_expr) =                               (if Option.is_some _3 then ENew (_2,(Option.get _3)) else ENew (_2,Eint(1))) in
    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Bool ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | T_Char ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | T_Double ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | T_Int ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | T_Void ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | T_Add | T_Amp | T_Break | T_Const_Char _ | T_Const_Int _ | T_Const_Real _ | T_Const_String _ | T_Cont | T_Decr | T_Del | T_False | T_For | T_Id _ | T_If | T_Incr | T_Lbrace | T_Lparen | T_Mul | T_New | T_Not | T_Null | T_Rbrace | T_Return | T_Semicolon | T_Sub | T_True ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | T_Add ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Amp ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Break ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Const_Char _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Const_Int _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Const_Real _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Const_String _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_Cont ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Decr ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Del ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_False ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Id _v ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | T_If ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Incr ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Lbrace ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState163
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
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Semicolon ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Sub ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_True ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_For ->
            _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | T_Rbrace ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
    | _ ->
        _menhir_fail ()

and _menhir_reduce90 : _menhir_env -> 'ttv_tail -> 'ttv_return =
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

and _menhir_reduce73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState9
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
        _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | T_Semicolon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState152
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
                        _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState8
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
                        _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState22
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
        | MenhirState207 | MenhirState205 | MenhirState155 | MenhirState2 ->
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
                        _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                    | T_Bool | T_Char | T_Double | T_Int ->
                        _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
                | T_Comma | T_Semicolon ->
                    _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack)
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
                _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState40
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
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | T_Amp ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | T_Const_Char _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
                | T_Const_Int _v ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
                | T_Const_Real _v ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
                | T_Const_String _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
                | T_Decr ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | T_Del ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | T_False ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | T_Id _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
                | T_Incr ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | T_Lparen ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | T_Mul ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | T_New ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | T_Not ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | T_Null ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | T_Sub ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | T_True ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState125
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
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

and _menhir_reduce67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_reduce80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_T_Byref_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState18
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
    | MenhirState193 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
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
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
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
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
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
                _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | T_Bool | T_Char | T_Double | T_Int ->
                _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState5
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
  

