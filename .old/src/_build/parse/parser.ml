
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | VOID
    | VALUE
    | UINT8
    | UINT256
    | TRUE
    | THIS
    | THEN
    | SINGLE_EQ
    | SENDER
    | SEMICOLON
    | SELFDESTRUCT
    | RSQBR
    | RPAR
    | RETURN
    | REENTRANCE
    | RBRACE
    | RARROW
    | PLUS
    | NOW
    | NOT
    | NEQ
    | MULT
    | MSG
    | MINUS
    | LT
    | LSQBR
    | LPAR
    | LOG
    | LBRACE
    | LAND
    | INDEXED
    | IF
    | IDENT of (
# 2 "parse/parser.mly"
       (string)
# 43 "parse/parser.ml"
  )
    | GT
    | FALSE
    | EVENT
    | EQUALITY
    | EOF
    | ELSE
    | DOT
    | DEPLOY
    | DEFAULT
    | DECLIT8 of (
# 4 "parse/parser.mly"
       (Big_int.big_int)
# 57 "parse/parser.ml"
  )
    | DECLIT256 of (
# 3 "parse/parser.mly"
       (Big_int.big_int)
# 62 "parse/parser.ml"
  )
    | CONTRACT
    | COMMA
    | CASE
    | BYTES32
    | BOOL
    | BLOCK
    | BECOME
    | BALANCE
    | ALONG
    | ADDRESS
    | ABORT
  
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
  | MenhirState190
  | MenhirState185
  | MenhirState180
  | MenhirState177
  | MenhirState171
  | MenhirState168
  | MenhirState164
  | MenhirState157
  | MenhirState155
  | MenhirState150
  | MenhirState144
  | MenhirState141
  | MenhirState138
  | MenhirState129
  | MenhirState124
  | MenhirState120
  | MenhirState115
  | MenhirState112
  | MenhirState110
  | MenhirState108
  | MenhirState106
  | MenhirState104
  | MenhirState103
  | MenhirState100
  | MenhirState96
  | MenhirState94
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState83
  | MenhirState81
  | MenhirState77
  | MenhirState73
  | MenhirState71
  | MenhirState70
  | MenhirState55
  | MenhirState53
  | MenhirState52
  | MenhirState46
  | MenhirState42
  | MenhirState37
  | MenhirState34
  | MenhirState31
  | MenhirState26
  | MenhirState21
  | MenhirState11
  | MenhirState3
  | MenhirState0

let rec _menhir_goto_list_sentence_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Syntax.case_body) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (unit Syntax.sentence))), _, (xs : (unit Syntax.case_body))) = _menhir_stack in
        let _v : (unit Syntax.case_body) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 151 "parse/parser.ml"
         in
        _menhir_goto_list_sentence_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (scs : (unit Syntax.case_body))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit Syntax.case_body) = 
# 90 "parse/parser.mly"
                                          ( scs )
# 169 "parse/parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState115 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, (b : (unit Syntax.case_body))) = _menhir_stack in
                let _1 = () in
                let _v : (unit Syntax.case_body) = 
# 215 "parse/parser.mly"
                          ( b )
# 181 "parse/parser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (r : (unit Syntax.case_body)) = _v in
                let (_menhir_stack, _menhir_s, (v : (unit Syntax.exp option))) = _menhir_stack in
                let _v : (unit Syntax.message_info) = 
# 205 "parse/parser.mly"
                                        ( { Syntax.message_value_info = v;
                                            message_reentrance_info = r } )
# 191 "parse/parser.ml"
                 in
                (match _menhir_s with
                | MenhirState103 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (m : (unit Syntax.message_info)) = _v in
                    let (((_menhir_stack, _menhir_s, (contr : (unit Syntax.exp))), (mtd : (
# 2 "parse/parser.mly"
       (string)
# 201 "parse/parser.ml"
                    ))), _, (xs : (unit Syntax.exp list))) = _menhir_stack in
                    let _3 = () in
                    let _1 = () in
                    let _2 = () in
                    let _v : (unit Syntax.exp) = let lst =
                      let lst =
                        let xs =
                          let x = 
# 232 "<standard.mly>"
    ( xs )
# 212 "parse/parser.ml"
                           in
                          
# 200 "<standard.mly>"
    ( x )
# 217 "parse/parser.ml"
                          
                        in
                        
# 69 "parse/parser.mly"
                                                        (xs)
# 223 "parse/parser.ml"
                        
                      in
                      
# 202 "parse/parser.mly"
                    (lst)
# 229 "parse/parser.ml"
                      
                    in
                    
# 191 "parse/parser.mly"
    ( Syntax.SendExp { Syntax.send_head_contract = contr; send_head_method = Some mtd
                       ; send_args = (lst); send_msg_info = m }, () )
# 236 "parse/parser.ml"
                     in
                    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
                | MenhirState124 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (m : (unit Syntax.message_info)) = _v in
                    let (_menhir_stack, _menhir_s, (contr : (unit Syntax.exp))) = _menhir_stack in
                    let _5 = () in
                    let _4 = () in
                    let _3 = () in
                    let _2 = () in
                    let _v : (unit Syntax.exp) = 
# 188 "parse/parser.mly"
    ( Syntax.SendExp { Syntax.send_head_contract = contr; send_head_method = None
                       ; send_args = []; send_msg_info = m }, () )
# 252 "parse/parser.ml"
                     in
                    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
                | MenhirState129 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (m : (unit Syntax.message_info)) = _v in
                    let (((_menhir_stack, _menhir_s), (s : (
# 2 "parse/parser.mly"
       (string)
# 262 "parse/parser.ml"
                    ))), _, (xs : (unit Syntax.exp list))) = _menhir_stack in
                    let _3 = () in
                    let _1_inlined1 = () in
                    let _1 = () in
                    let _v : (unit Syntax.exp) = let lst =
                      let _1 = _1_inlined1 in
                      let lst =
                        let xs =
                          let x = 
# 232 "<standard.mly>"
    ( xs )
# 274 "parse/parser.ml"
                           in
                          
# 200 "<standard.mly>"
    ( x )
# 279 "parse/parser.ml"
                          
                        in
                        
# 69 "parse/parser.mly"
                                                        (xs)
# 285 "parse/parser.ml"
                        
                      in
                      
# 202 "parse/parser.mly"
                    (lst)
# 291 "parse/parser.ml"
                      
                    in
                    
# 185 "parse/parser.mly"
                                                    ( Syntax.NewExp { Syntax.new_head = s; new_args = lst; new_msg_info = m }, () )
# 297 "parse/parser.ml"
                     in
                    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    _menhir_fail ())
            | MenhirState168 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (cond : (unit Syntax.exp))), _, (s : (unit Syntax.sentence))), _, (b : (unit Syntax.case_body))) = _menhir_stack in
                let _6 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (unit Syntax.sentence) = let bodyF = 
# 130 "parse/parser.mly"
              (b)
# 313 "parse/parser.ml"
                 in
                let bodyT = 
# 129 "parse/parser.mly"
                 ([s])
# 318 "parse/parser.ml"
                 in
                
# 151 "parse/parser.mly"
                                                                 ( Syntax.IfThenElse (cond, bodyT, bodyF) )
# 323 "parse/parser.ml"
                 in
                _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
            | MenhirState157 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ABORT ->
                        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | ADDRESS ->
                        _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | BALANCE ->
                        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | BOOL ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | BYTES32 ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | DECLIT256 _v ->
                        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
                    | DECLIT8 _v ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
                    | DEPLOY ->
                        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | FALSE ->
                        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | IDENT _v ->
                        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
                    | IF ->
                        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | LBRACE ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | LOG ->
                        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | LPAR ->
                        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | NOT ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | NOW ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | RETURN ->
                        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | SELFDESTRUCT ->
                        _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | SENDER ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | THIS ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | TRUE ->
                        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | UINT256 ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | UINT8 ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | VALUE ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | VOID ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState177
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177)
                | ABORT | ADDRESS | BALANCE | BOOL | BYTES32 | DECLIT256 _ | DECLIT8 _ | DEPLOY | FALSE | IDENT _ | IF | LOG | LPAR | NOT | NOW | RBRACE | RETURN | SELFDESTRUCT | SENDER | THIS | TRUE | UINT256 | UINT8 | VALUE | VOID ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _, (cond : (unit Syntax.exp))), _, (b : (unit Syntax.case_body))) = _menhir_stack in
                    let _4 = () in
                    let _2 = () in
                    let _1 = () in
                    let _v : (unit Syntax.sentence) = let body = 
# 130 "parse/parser.mly"
              (b)
# 399 "parse/parser.ml"
                     in
                    
# 152 "parse/parser.mly"
                                            ( Syntax.IfThenOnly (cond, body) )
# 404 "parse/parser.ml"
                     in
                    _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState177 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (cond : (unit Syntax.exp))), _, (b : (unit Syntax.case_body))), _, (b_inlined1 : (unit Syntax.case_body))) = _menhir_stack in
                let _6 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (unit Syntax.sentence) = let bodyF =
                  let b = b_inlined1 in
                  
# 130 "parse/parser.mly"
              (b)
# 426 "parse/parser.ml"
                  
                in
                let bodyT = 
# 130 "parse/parser.mly"
              (b)
# 432 "parse/parser.ml"
                 in
                
# 151 "parse/parser.mly"
                                                                 ( Syntax.IfThenElse (cond, bodyT, bodyF) )
# 437 "parse/parser.ml"
                 in
                _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
            | MenhirState52 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (_1 : (Syntax.case_header))), _, (_2 : (unit Syntax.case_body))) = _menhir_stack in
                let _v : (unit Syntax.case) = 
# 85 "parse/parser.mly"
                                (   { Syntax.case_header    = _1
                                    ; Syntax.case_body      = _2    }    )
# 448 "parse/parser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CASE ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState185
                | DEFAULT ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState185
                | RBRACE ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState185
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185)
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit Syntax.exp option) = 
# 210 "parse/parser.mly"
                ( None )
# 481 "parse/parser.ml"
     in
    _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v

and _menhir_run104 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit Syntax.case_body) = 
# 211 "<standard.mly>"
    ( [] )
# 529 "parse/parser.ml"
     in
    _menhir_goto_list_sentence_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Syntax.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ALONG ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | REENTRANCE ->
                _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ALONG ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | REENTRANCE ->
                _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (s : (
# 2 "parse/parser.mly"
       (string)
# 597 "parse/parser.ml"
            ))), _, (xs : (unit Syntax.exp list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit Syntax.exp) = let lst =
              let lst =
                let xs =
                  let x = 
# 232 "<standard.mly>"
    ( xs )
# 607 "parse/parser.ml"
                   in
                  
# 200 "<standard.mly>"
    ( x )
# 612 "parse/parser.ml"
                  
                in
                
# 69 "parse/parser.mly"
                                                        (xs)
# 618 "parse/parser.ml"
                
              in
              
# 202 "parse/parser.mly"
                    (lst)
# 624 "parse/parser.ml"
              
            in
            
# 184 "parse/parser.mly"
                              ( Syntax.FunctionCallExp {Syntax.call_head = s; call_args = lst }, () )
# 630 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), (name : (
# 2 "parse/parser.mly"
       (string)
# 656 "parse/parser.ml"
                ))), _, (xs : (unit Syntax.exp list))) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1_inlined1 = () in
                let _1 = () in
                let _v : (unit Syntax.sentence) = let lst =
                  let _1 = _1_inlined1 in
                  let lst =
                    let xs =
                      let x = 
# 232 "<standard.mly>"
    ( xs )
# 669 "parse/parser.ml"
                       in
                      
# 200 "<standard.mly>"
    ( x )
# 674 "parse/parser.ml"
                      
                    in
                    
# 69 "parse/parser.mly"
                                                        (xs)
# 680 "parse/parser.ml"
                    
                  in
                  
# 202 "parse/parser.mly"
                    (lst)
# 686 "parse/parser.ml"
                  
                in
                
# 153 "parse/parser.mly"
                                                 ( Syntax.LogSentence (name, lst, None))
# 692 "parse/parser.ml"
                 in
                _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SINGLE_EQ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDRESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | BALANCE ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | DECLIT256 _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | DECLIT8 _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | DEPLOY ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | FALSE ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | IDENT _v ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | LPAR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | NOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | NOW ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | SENDER ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | THIS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | TRUE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | VALUE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run138 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138

and _menhir_run141 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | THEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState141 in
        let _v : (unit Syntax.exp option) = 
# 114 "<standard.mly>"
    ( None )
# 839 "parse/parser.ml"
         in
        _menhir_goto_option_exp_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141

and _menhir_run148 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | BALANCE ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | DECLIT256 _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | DECLIT8 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | DEPLOY ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | FALSE ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | IDENT _v ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | LPAR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NOT ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NOW ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | SENDER ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | THIS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | VALUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | RPAR ->
                _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
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

and _menhir_run154 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDRESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | BALANCE ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | DECLIT256 _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | DECLIT8 _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | DEPLOY ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | FALSE ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | IDENT _v ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | LPAR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | NOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | NOW ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | SENDER ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | THIS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | TRUE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | VALUE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run158 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 2 "parse/parser.mly"
       (string)
# 964 "parse/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
    | IDENT _ | RARROW ->
        _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack)
    | DOT | EQUALITY | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run159 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
    | IDENT _ | RARROW ->
        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run160 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (unit Syntax.sentence) = 
# 134 "parse/parser.mly"
                     ( Syntax.AbortSentence )
# 1017 "parse/parser.ml"
         in
        _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Syntax.exp option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | THEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BECOME ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | BALANCE ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | DECLIT256 _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
            | DECLIT8 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
            | DEPLOY ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | FALSE ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | IDENT _v ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
            | LPAR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | NOT ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | NOW ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | SENDER ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | THIS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | VALUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState144
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
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_sentence : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Syntax.sentence) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABORT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | ADDRESS ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | BALANCE ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | DECLIT256 _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
            | DECLIT8 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
            | DEPLOY ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | FALSE ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | IDENT _v ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
            | IF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | LBRACE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | LOG ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | LPAR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | NOT ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | NOW ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | RETURN ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | SELFDESTRUCT ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | SENDER ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | THIS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | VALUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | VOID ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
        | ABORT | ADDRESS | BALANCE | BOOL | BYTES32 | DECLIT256 _ | DECLIT8 _ | DEPLOY | FALSE | IDENT _ | IF | LOG | LPAR | NOT | NOW | RBRACE | RETURN | SELFDESTRUCT | SENDER | THIS | TRUE | UINT256 | UINT8 | VALUE | VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (cond : (unit Syntax.exp))), _, (s : (unit Syntax.sentence))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (unit Syntax.sentence) = let body = 
# 129 "parse/parser.mly"
                 ([s])
# 1166 "parse/parser.ml"
             in
            
# 152 "parse/parser.mly"
                                            ( Syntax.IfThenOnly (cond, body) )
# 1171 "parse/parser.ml"
             in
            _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (cond : (unit Syntax.exp))), _, (s : (unit Syntax.sentence))), _, (s_inlined1 : (unit Syntax.sentence))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (unit Syntax.sentence) = let bodyF =
          let s = s_inlined1 in
          
# 129 "parse/parser.mly"
                 ([s])
# 1193 "parse/parser.ml"
          
        in
        let bodyT = 
# 129 "parse/parser.mly"
                 ([s])
# 1199 "parse/parser.ml"
         in
        
# 151 "parse/parser.mly"
                                                                 ( Syntax.IfThenElse (cond, bodyT, bodyF) )
# 1204 "parse/parser.ml"
         in
        _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (cond : (unit Syntax.exp))), _, (b : (unit Syntax.case_body))), _, (s : (unit Syntax.sentence))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (unit Syntax.sentence) = let bodyF = 
# 129 "parse/parser.mly"
                 ([s])
# 1218 "parse/parser.ml"
         in
        let bodyT = 
# 130 "parse/parser.mly"
              (b)
# 1223 "parse/parser.ml"
         in
        
# 151 "parse/parser.mly"
                                                                 ( Syntax.IfThenElse (cond, bodyT, bodyF) )
# 1228 "parse/parser.ml"
         in
        _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
    | MenhirState180 | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABORT ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | ADDRESS ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | BALANCE ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | BOOL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | BYTES32 ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | DECLIT256 _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
        | DECLIT8 _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
        | DEPLOY ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | FALSE ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | IDENT _v ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
        | IF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | LOG ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | LPAR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | NOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | NOW ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | RETURN ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | SELFDESTRUCT ->
            _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | SENDER ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | THIS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | TRUE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | UINT256 ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | UINT8 ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | VALUE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | VOID ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | RBRACE ->
            _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Syntax.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState150 | MenhirState73 | MenhirState77 | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (unit Syntax.exp list)) = _v in
        let _v : (unit Syntax.exp list) = 
# 144 "<standard.mly>"
    ( x )
# 1303 "parse/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (unit Syntax.exp list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (unit Syntax.exp))) = _menhir_stack in
        let _2 = () in
        let _v : (unit Syntax.exp list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1315 "parse/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_value_info : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Syntax.exp option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REENTRANCE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce36 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Syntax.lexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (l : (unit Syntax.lexp))) = _menhir_stack in
    let _v : (unit Syntax.exp) = 
# 197 "parse/parser.mly"
    ( Syntax.ArrayAccessExp l, () )
# 1352 "parse/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run108 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run110 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run112 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_run98 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEFAULT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ALONG ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | REENTRANCE ->
                    _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState124
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
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | BALANCE ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | DECLIT256 _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | DECLIT8 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | DEPLOY ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | FALSE ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | IDENT _v ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | LPAR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | NOT ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | NOW ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | SENDER ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | THIS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | VALUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | RPAR ->
                _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
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

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | ADDRESS ->
        _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | BOOL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | BYTES32 ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IDENT _v ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LOG ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | RETURN ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | SELFDESTRUCT ->
        _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | VOID ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | RBRACE ->
        _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_goto_list_case_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Syntax.case list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (_2 : (
# 2 "parse/parser.mly"
       (string)
# 1873 "parse/parser.ml"
            ))), _, (xs : (Syntax.arg list))), _, (_5 : (unit Syntax.case list))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1_inlined1 = () in
            let _1 = () in
            let _v : (unit Syntax.toplevel) = let _3 =
              let _1 = _1_inlined1 in
              let xs =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 1886 "parse/parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 1891 "parse/parser.ml"
                
              in
              
# 69 "parse/parser.mly"
                                                        (xs)
# 1897 "parse/parser.ml"
              
            in
            
# 77 "parse/parser.mly"
                                ( Syntax.Contract(  { Syntax.contract_cases     = _5
                                                    ; contract_name             = _2
                                                    ; contract_arguments        = _3  }   ) )
# 1905 "parse/parser.ml"
             in
            _menhir_goto_contract _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (unit Syntax.case))), _, (xs : (unit Syntax.case list))) = _menhir_stack in
        let _v : (unit Syntax.case list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1921 "parse/parser.ml"
         in
        _menhir_goto_list_case_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce27 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 2 "parse/parser.mly"
       (string)
# 1930 "parse/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (s : (
# 2 "parse/parser.mly"
       (string)
# 1936 "parse/parser.ml"
    ))) = _menhir_stack in
    let _v : (unit Syntax.exp) = 
# 179 "parse/parser.mly"
    ( Syntax.IdentifierExp s, () )
# 1941 "parse/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 2 "parse/parser.mly"
       (string)
# 1948 "parse/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | RPAR ->
        _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_reduce49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit Syntax.exp list) = 
# 142 "<standard.mly>"
    ( [] )
# 1994 "parse/parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (unit Syntax.exp))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (unit Syntax.exp) = 
# 193 "parse/parser.mly"
                                 ( Syntax.AddressExp e, () )
# 2038 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQUALITY | GT | LAND | LT | MINUS | NEQ | PLUS | REENTRANCE | RPAR | RSQBR | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (unit Syntax.exp))), _, (rhs : (unit Syntax.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (unit Syntax.exp) = let o = 
# 158 "parse/parser.mly"
         (fun (l, r) -> Syntax.PlusExp(l, r))
# 2065 "parse/parser.ml"
             in
            
# 177 "parse/parser.mly"
                                 ( (o (lhs, rhs)), () )
# 2070 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQUALITY | GT | LAND | LT | MINUS | MULT | NEQ | PLUS | REENTRANCE | RPAR | RSQBR | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (unit Syntax.exp))), _, (rhs : (unit Syntax.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (unit Syntax.exp) = let o = 
# 160 "parse/parser.mly"
         (fun (l, r) -> Syntax.MultExp(l, r))
# 2095 "parse/parser.ml"
             in
            
# 177 "parse/parser.mly"
                                 ( (o (lhs, rhs)), () )
# 2100 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | RSQBR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (s : (unit Syntax.exp))), _, (idx : (unit Syntax.exp))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (unit Syntax.lexp) = 
# 223 "parse/parser.mly"
    ( Syntax.ArrayAccessLExp {
       Syntax.array_access_array = s; array_access_index = idx} )
# 2145 "parse/parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState171 | MenhirState164 | MenhirState155 | MenhirState150 | MenhirState141 | MenhirState144 | MenhirState138 | MenhirState55 | MenhirState70 | MenhirState71 | MenhirState73 | MenhirState77 | MenhirState81 | MenhirState120 | MenhirState100 | MenhirState112 | MenhirState110 | MenhirState108 | MenhirState106 | MenhirState104 | MenhirState96 | MenhirState94 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState83 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack)
            | MenhirState53 | MenhirState180 | MenhirState177 | MenhirState157 | MenhirState168 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SINGLE_EQ ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ADDRESS ->
                        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                    | BALANCE ->
                        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                    | DECLIT256 _v ->
                        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
                    | DECLIT8 _v ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
                    | DEPLOY ->
                        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                    | FALSE ->
                        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                    | IDENT _v ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
                    | LPAR ->
                        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                    | NOT ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                    | NOW ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                    | SENDER ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                    | THIS ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                    | TRUE ->
                        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                    | VALUE ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171)
                | DOT | EQUALITY | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS ->
                    _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQUALITY | GT | LAND | LT | NEQ | REENTRANCE | RPAR | RSQBR | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (unit Syntax.exp))), _, (rhs : (unit Syntax.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (unit Syntax.exp) = let o = 
# 163 "parse/parser.mly"
        (fun (l, r) -> Syntax.NeqExp(l, r))
# 2232 "parse/parser.ml"
             in
            
# 177 "parse/parser.mly"
                                 ( (o (lhs, rhs)), () )
# 2237 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQUALITY | GT | LAND | LT | MINUS | NEQ | PLUS | REENTRANCE | RPAR | RSQBR | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (unit Syntax.exp))), _, (rhs : (unit Syntax.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (unit Syntax.exp) = let o = 
# 159 "parse/parser.mly"
          (fun (l, r)  -> Syntax.MinusExp(l, r))
# 2264 "parse/parser.ml"
             in
            
# 177 "parse/parser.mly"
                                 ( (o (lhs, rhs)), () )
# 2269 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | REENTRANCE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (v : (unit Syntax.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (unit Syntax.exp option) = 
# 211 "parse/parser.mly"
                    ( Some v )
# 2310 "parse/parser.ml"
             in
            _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQUALITY | GT | LAND | LT | NEQ | REENTRANCE | RPAR | RSQBR | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (unit Syntax.exp))), _, (rhs : (unit Syntax.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (unit Syntax.exp) = let o = 
# 161 "parse/parser.mly"
       (fun (l, r) -> Syntax.LtExp(l, r))
# 2341 "parse/parser.ml"
             in
            
# 177 "parse/parser.mly"
                                 ( (o (lhs, rhs)), () )
# 2346 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | LAND | REENTRANCE | RPAR | RSQBR | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (unit Syntax.exp))), _, (rhs : (unit Syntax.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Syntax.exp) = 
# 168 "parse/parser.mly"
                               ( Syntax.LandExp (lhs, rhs), () )
# 2385 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQUALITY | GT | LAND | LT | NEQ | REENTRANCE | RPAR | RSQBR | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (unit Syntax.exp))), _, (rhs : (unit Syntax.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (unit Syntax.exp) = let o = 
# 162 "parse/parser.mly"
       (fun (l, r) -> Syntax.GtExp(l, r))
# 2416 "parse/parser.ml"
             in
            
# 177 "parse/parser.mly"
                                 ( (o (lhs, rhs)), () )
# 2421 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQUALITY | GT | LAND | LT | NEQ | REENTRANCE | RPAR | RSQBR | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (unit Syntax.exp))), _, (rhs : (unit Syntax.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (unit Syntax.exp) = let o = 
# 164 "parse/parser.mly"
             (fun (l, r) -> Syntax.EqualityExp(l, r))
# 2452 "parse/parser.ml"
             in
            
# 177 "parse/parser.mly"
                                 ( (o (lhs, rhs)), () )
# 2457 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState150 | MenhirState73 | MenhirState77 | MenhirState120 | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | BALANCE ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | DECLIT256 _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | DECLIT8 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | DEPLOY ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | FALSE ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | IDENT _v ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | LPAR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | NOT ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | NOW ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | SENDER ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | THIS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | VALUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (unit Syntax.exp))) = _menhir_stack in
            let _v : (unit Syntax.exp list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2534 "parse/parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (unit Syntax.exp))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (unit Syntax.exp) = 
# 175 "parse/parser.mly"
                                 ( Syntax.BalanceExp e, () )
# 2579 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (unit Syntax.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit Syntax.exp) = 
# 183 "parse/parser.mly"
    ( Syntax.ParenthExp e, () )
# 2623 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | REENTRANCE | RPAR | RSQBR | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (unit Syntax.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (unit Syntax.exp) = 
# 194 "parse/parser.mly"
                 ( Syntax.NotExp e, () )
# 2664 "parse/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (value : (unit Syntax.exp))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (unit Syntax.sentence) = 
# 150 "parse/parser.mly"
    ( Syntax.ExpSentence value )
# 2709 "parse/parser.ml"
             in
            _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
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
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (unit Syntax.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit Syntax.sentence) = 
# 154 "parse/parser.mly"
                                     ( Syntax.SelfdestructSentence e )
# 2753 "parse/parser.ml"
             in
            _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (value : (unit Syntax.exp option))), _, (cont : (unit Syntax.exp))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (unit Syntax.sentence) = 
# 136 "parse/parser.mly"
    ( Syntax.ReturnSentence { Syntax. return_exp = value; return_cont = cont} )
# 2799 "parse/parser.ml"
             in
            _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (unit Syntax.exp))) = _menhir_stack in
            let _v : (unit Syntax.exp option) = 
# 116 "<standard.mly>"
    ( Some x )
# 2839 "parse/parser.ml"
             in
            _menhir_goto_option_exp_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABORT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | ADDRESS ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | BALANCE ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | DECLIT256 _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
            | DECLIT8 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
            | DEPLOY ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | FALSE ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | IDENT _v ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
            | IF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | LBRACE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | LOG ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | LPAR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | NOT ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | NOW ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | RETURN ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | SELFDESTRUCT ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | SENDER ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | THIS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | VALUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | VOID ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Syntax.typ))), (name : (
# 2 "parse/parser.mly"
       (string)
# 2970 "parse/parser.ml"
            ))), _, (value : (unit Syntax.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : (unit Syntax.sentence) = 
# 143 "parse/parser.mly"
              ( Syntax.VariableInitSentence
                { Syntax.variable_init_type = t
                ; variable_init_name = name
                ; variable_init_value = value
                }
              )
# 2982 "parse/parser.ml"
             in
            _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
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
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (unit Syntax.lexp))), _, (rhs : (unit Syntax.exp))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (unit Syntax.sentence) = 
# 138 "parse/parser.mly"
    ( Syntax.AssignmentSentence (lhs, rhs) )
# 3026 "parse/parser.ml"
             in
            _menhir_goto_sentence _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 | MenhirState180 | MenhirState177 | MenhirState157 | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | EQUALITY ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_goto_separated_nonempty_list_COMMA_event_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.event_arg list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Syntax.event_arg list)) = _v in
        let _v : (Syntax.event_arg list) = 
# 144 "<standard.mly>"
    ( x )
# 3117 "parse/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_event_arg__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Syntax.event_arg list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Syntax.event_arg))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.event_arg list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3129 "parse/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_event_arg_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.arg list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState46 | MenhirState37 | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Syntax.arg list)) = _v in
        let _v : (Syntax.arg list) = 
# 144 "<standard.mly>"
    ( x )
# 3145 "parse/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Syntax.arg list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Syntax.arg))) = _menhir_stack in
        let _2 = () in
        let _v : (Syntax.arg list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3157 "parse/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_case_header : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.case_header) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit Syntax.case list) = 
# 211 "<standard.mly>"
    ( [] )
# 3182 "parse/parser.ml"
     in
    _menhir_goto_list_case_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Syntax.case_header) = 
# 94 "parse/parser.mly"
                                          ( Syntax.DefaultCaseHeader )
# 3194 "parse/parser.ml"
     in
    _menhir_goto_case_header _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDRESS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | BOOL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | BYTES32 ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | UINT256 ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | UINT8 ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState34 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ADDRESS ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                    | BOOL ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                    | BYTES32 ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                    | IDENT _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
                    | UINT256 ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                    | UINT8 ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                    | RPAR ->
                        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MSG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _v : (unit Syntax.exp) = 
# 173 "parse/parser.mly"
                        ( Syntax.ValueExp, () )
# 3308 "parse/parser.ml"
                 in
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit Syntax.exp) = 
# 169 "parse/parser.mly"
         ( Syntax.TrueExp, () )
# 3338 "parse/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit Syntax.exp) = 
# 195 "parse/parser.mly"
         ( Syntax.ThisExp, () )
# 3350 "parse/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MSG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _v : (unit Syntax.exp) = 
# 174 "parse/parser.mly"
                         ( Syntax.SenderExp, () )
# 3382 "parse/parser.ml"
                 in
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BLOCK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _v : (unit Syntax.exp) = 
# 176 "parse/parser.mly"
                        ( Syntax.NowExp, () )
# 3432 "parse/parser.ml"
                 in
                _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | BALANCE ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | DECLIT256 _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | DECLIT8 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | DEPLOY ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | FALSE ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NOT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NOW ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | SENDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | THIS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | VALUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 2 "parse/parser.mly"
       (string)
# 3535 "parse/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | DOT | EQUALITY | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | REENTRANCE | RPAR | RSQBR | SEMICOLON | THEN ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit Syntax.exp) = 
# 170 "parse/parser.mly"
          ( Syntax.FalseExp, () )
# 3561 "parse/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | BALANCE ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | DECLIT256 _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | DECLIT8 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | DEPLOY ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | FALSE ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | IDENT _v ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | LPAR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | NOT ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | NOW ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | SENDER ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | THIS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | VALUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | RPAR ->
                _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
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

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "parse/parser.mly"
       (Big_int.big_int)
# 3632 "parse/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (d : (
# 4 "parse/parser.mly"
       (Big_int.big_int)
# 3640 "parse/parser.ml"
    )) = _v in
    let _v : (unit Syntax.exp) = 
# 172 "parse/parser.mly"
                ( Syntax.DecLit8Exp d, ())
# 3645 "parse/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "parse/parser.mly"
       (Big_int.big_int)
# 3652 "parse/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (d : (
# 3 "parse/parser.mly"
       (Big_int.big_int)
# 3660 "parse/parser.ml"
    )) = _v in
    let _v : (unit Syntax.exp) = 
# 171 "parse/parser.mly"
                  ( Syntax.DecLit256Exp d, ())
# 3665 "parse/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDRESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BALANCE ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | DECLIT256 _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | DECLIT8 _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | DEPLOY ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | FALSE ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | IDENT _v ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | LPAR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NOW ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SENDER ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | THIS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | TRUE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | VALUE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run11 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | BOOL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | BYTES32 ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_goto_event_arg : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.event_arg) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDRESS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | BOOL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | BYTES32 ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | UINT256 ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | UINT8 ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | RPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (Syntax.event_arg))) = _menhir_stack in
        let _v : (Syntax.event_arg list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3790 "parse/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_event_arg_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.typ) -> (
# 2 "parse/parser.mly"
       (string)
# 3803 "parse/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 2 "parse/parser.mly"
       (string)
# 3811 "parse/parser.ml"
    )) = _v in
    let (_menhir_stack, _menhir_s, (t : (Syntax.typ))) = _menhir_stack in
    let _v : (Syntax.arg) = 
# 101 "parse/parser.mly"
                                        (   { Syntax.arg_typ        = t
                                            ; Syntax.arg_ident      = i
                                            ; Syntax.arg_location   = None    }   )
# 3819 "parse/parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (a : (Syntax.arg))) = _menhir_stack in
        let _v : (Syntax.event_arg) = 
# 107 "parse/parser.mly"
                                        ( Syntax.event_arg_of_arg a false )
# 3830 "parse/parser.ml"
         in
        _menhir_goto_event_arg _menhir_env _menhir_stack _menhir_s _v
    | MenhirState26 | MenhirState46 | MenhirState42 | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Syntax.arg))) = _menhir_stack in
            let _v : (Syntax.arg list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3865 "parse/parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_contract : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Syntax.toplevel) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONTRACT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | EVENT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | EOF ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190

and _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.arg list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CASE ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | DEFAULT ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | RBRACE ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
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
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _), (name : (
# 2 "parse/parser.mly"
       (string)
# 3958 "parse/parser.ml"
                ))), _, (xs : (Syntax.arg list))) = _menhir_stack in
                let _6 = () in
                let _3_inlined1 = () in
                let _1_inlined1 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Syntax.case_header) = let args =
                  let (_3, _1) = (_3_inlined1, _1_inlined1) in
                  let xs =
                    let x = 
# 232 "<standard.mly>"
    ( xs )
# 3972 "parse/parser.ml"
                     in
                    
# 200 "<standard.mly>"
    ( x )
# 3977 "parse/parser.ml"
                    
                  in
                  
# 69 "parse/parser.mly"
                                                        (xs)
# 3983 "parse/parser.ml"
                  
                in
                
# 98 "parse/parser.mly"
    ( Syntax.UsualCaseHeader { case_return_typ = []; Syntax.case_name = name; case_arguments = args } )
# 3989 "parse/parser.ml"
                 in
                _menhir_goto_case_header _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (return_typ : (Syntax.typ))), (name : (
# 2 "parse/parser.mly"
       (string)
# 4021 "parse/parser.ml"
                ))), _, (xs : (Syntax.arg list))) = _menhir_stack in
                let _6 = () in
                let _3 = () in
                let _1_inlined1 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Syntax.case_header) = let args =
                  let _1 = _1_inlined1 in
                  let xs =
                    let x = 
# 232 "<standard.mly>"
    ( xs )
# 4034 "parse/parser.ml"
                     in
                    
# 200 "<standard.mly>"
    ( x )
# 4039 "parse/parser.ml"
                    
                  in
                  
# 69 "parse/parser.mly"
                                                        (xs)
# 4045 "parse/parser.ml"
                  
                in
                
# 96 "parse/parser.mly"
    ( Syntax.UsualCaseHeader { case_return_typ = [return_typ] (* multi returns not supported *); Syntax.case_name = name; case_arguments = args } )
# 4051 "parse/parser.ml"
                 in
                _menhir_goto_case_header _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_reduce80 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 2 "parse/parser.mly"
       (string)
# 4072 "parse/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (s : (
# 2 "parse/parser.mly"
       (string)
# 4078 "parse/parser.ml"
    ))) = _menhir_stack in
    let _v : (Syntax.typ) = 
# 125 "parse/parser.mly"
                                        ( Syntax.ContractInstanceType s )
# 4083 "parse/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _v
        | INDEXED ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (i : (
# 2 "parse/parser.mly"
       (string)
# 4110 "parse/parser.ml"
                )) = _v in
                let (_menhir_stack, _menhir_s, (t : (Syntax.typ))) = _menhir_stack in
                let _2 = () in
                let _v : (Syntax.event_arg) = 
# 108 "parse/parser.mly"
                                        ( { Syntax.event_arg_body =
        { Syntax.arg_typ = t
        ; Syntax.arg_ident = i
        ; Syntax.arg_location = None
        }
      ; Syntax.event_arg_indexed = true
      }
    )
# 4124 "parse/parser.ml"
                 in
                _menhir_goto_event_arg _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | RARROW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | IDENT _ | INDEXED ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (key : (Syntax.typ))), _, (value : (Syntax.typ))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.typ) = 
# 124 "parse/parser.mly"
                                        ( Syntax.MappingType (key, value) )
# 4155 "parse/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 | MenhirState42 | MenhirState37 | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _v
        | RARROW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ADDRESS ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | BOOL ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | BYTES32 ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | IDENT _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
                | UINT256 ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | UINT8 ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | RPAR ->
                    _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | RARROW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState180 | MenhirState53 | MenhirState177 | MenhirState168 | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SINGLE_EQ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ADDRESS ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState164
                | BALANCE ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState164
                | DECLIT256 _v ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
                | DECLIT8 _v ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
                | DEPLOY ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState164
                | FALSE ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState164
                | IDENT _v ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
                | LPAR ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState164
                | NOT ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState164
                | NOW ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState164
                | SENDER ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState164
                | THIS ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState164
                | TRUE ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState164
                | VALUE ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState164
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | RARROW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce77 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _1 = () in
    let _v : (Syntax.typ) = 
# 122 "parse/parser.mly"
                                        ( Syntax.AddressType )
# 4299 "parse/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_contract_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Syntax.toplevel list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (cs : (unit Syntax.toplevel list))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 64 "parse/parser.mly"
       (unit Syntax.toplevel list)
# 4320 "parse/parser.ml"
            ) = 
# 72 "parse/parser.mly"
                                ( cs )
# 4324 "parse/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 64 "parse/parser.mly"
       (unit Syntax.toplevel list)
# 4331 "parse/parser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (unit Syntax.toplevel))), _, (xs : (unit Syntax.toplevel list))) = _menhir_stack in
        let _v : (unit Syntax.toplevel list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 4347 "parse/parser.ml"
         in
        _menhir_goto_list_contract_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_event_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.event_arg list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (_2 : (
# 2 "parse/parser.mly"
       (string)
# 4372 "parse/parser.ml"
            ))), _, (xs : (Syntax.event_arg list))) = _menhir_stack in
            let _4 = () in
            let _3 = () in
            let _1_inlined1 = () in
            let _1 = () in
            let _v : (unit Syntax.toplevel) = let _3 =
              let _1 = _1_inlined1 in
              let xs =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 4384 "parse/parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 4389 "parse/parser.ml"
                
              in
              
# 69 "parse/parser.mly"
                                                        (xs)
# 4395 "parse/parser.ml"
              
            in
            
# 81 "parse/parser.mly"
                                ( Syntax.Event      { Syntax.event_arguments    = _3
                                                    ; event_name                = _2  }   )
# 4402 "parse/parser.ml"
             in
            _menhir_goto_contract _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Syntax.arg list) = 
# 142 "<standard.mly>"
    ( [] )
# 4423 "parse/parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Syntax.typ) = 
# 120 "parse/parser.mly"
                                        ( Syntax.Uint8Type )
# 4435 "parse/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Syntax.typ) = 
# 119 "parse/parser.mly"
                                        ( Syntax.Uint256Type )
# 4447 "parse/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 2 "parse/parser.mly"
       (string)
# 4454 "parse/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Syntax.typ) = 
# 121 "parse/parser.mly"
                                        ( Syntax.Bytes32Type )
# 4469 "parse/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Syntax.typ) = 
# 123 "parse/parser.mly"
                                        ( Syntax.BoolType )
# 4481 "parse/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
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
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
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

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit Syntax.toplevel list) = 
# 211 "<standard.mly>"
    ( [] )
# 4690 "parse/parser.ml"
     in
    _menhir_goto_list_contract_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState3 in
                let _v : (Syntax.event_arg list) = 
# 142 "<standard.mly>"
    ( [] )
# 4729 "parse/parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_COMMA_event_arg__ _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDRESS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | RPAR ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
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

and file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 64 "parse/parser.mly"
       (unit Syntax.toplevel list)
# 4812 "parse/parser.ml"
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
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONTRACT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EVENT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 4840 "parse/parser.ml"
