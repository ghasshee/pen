
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | VALUE
    | UNIT
    | UINT8
    | UINT256
    | TRUE
    | THIS
    | THEN
    | SENDER
    | SEMI
    | SELFDESTRUCT
    | RSQBR
    | RPAR
    | RETURN
    | REENTRANCE
    | REC
    | RBRACE
    | PLUS
    | NOW
    | NOT
    | NEW
    | NEQ
    | MULT
    | MSG
    | MINUS
    | METHOD
    | LT
    | LSQBR
    | LPAR
    | LOG
    | LET
    | LBRACE
    | LARROW
    | LAND
    | LAM
    | KECCAK
    | ISZERO
    | INDEXED
    | IN
    | IF
    | ID of (
# 11 "parser.mly"
       (string)
# 51 "parser.ml"
  )
    | GT
    | FIX
    | FALSE
    | EVENT
    | EUINT8 of (
# 13 "parser.mly"
       (Big_int.big_int)
# 60 "parser.ml"
  )
    | EUINT256 of (
# 12 "parser.mly"
       (Big_int.big_int)
# 65 "parser.ml"
  )
    | EQEQ
    | EQ
    | EOF
    | ELSE
    | ECDSARECOVER
    | DOT
    | DEFAULT
    | DARROW
    | CONTRACT
    | COMMA
    | COLON
    | CALL
    | BYTES32
    | BOOL
    | BLOCK
    | BECOME
    | BALANCE
    | ARROW
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
  | MenhirState218
  | MenhirState211
  | MenhirState206
  | MenhirState203
  | MenhirState198
  | MenhirState194
  | MenhirState188
  | MenhirState185
  | MenhirState183
  | MenhirState179
  | MenhirState178
  | MenhirState173
  | MenhirState170
  | MenhirState166
  | MenhirState161
  | MenhirState159
  | MenhirState155
  | MenhirState149
  | MenhirState147
  | MenhirState142
  | MenhirState136
  | MenhirState133
  | MenhirState131
  | MenhirState129
  | MenhirState127
  | MenhirState123
  | MenhirState121
  | MenhirState120
  | MenhirState119
  | MenhirState116
  | MenhirState114
  | MenhirState111
  | MenhirState109
  | MenhirState107
  | MenhirState103
  | MenhirState101
  | MenhirState98
  | MenhirState93
  | MenhirState91
  | MenhirState89
  | MenhirState88
  | MenhirState86
  | MenhirState83
  | MenhirState81
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState70
  | MenhirState68
  | MenhirState63
  | MenhirState62
  | MenhirState51
  | MenhirState50
  | MenhirState46
  | MenhirState42
  | MenhirState38
  | MenhirState34
  | MenhirState33
  | MenhirState29
  | MenhirState24
  | MenhirState15
  | MenhirState13
  | MenhirState6
  | MenhirState3
  | MenhirState0

# 1 "parser.mly"
  
    open Misc
    open Syntax 
    open Context
    module BS = BatString
    
let reserved x                  =   if BS.starts_with x "pre_" then err "Names 'pre_..' are reserved." 

# 179 "parser.ml"

let rec _menhir_run192 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv831 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
    ((let (_menhir_stack, _menhir_s, (_1 : 'tv_tm)) = _menhir_stack in
    let _v : 'tv_stmt = 
# 94 "parser.mly"
                                                    ( SmExpr (_1 [])                                            )
# 190 "parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv832)

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_stmt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv819 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv813 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | ADDRESS ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | BALANCE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | BYTES32 ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | ECDSARECOVER ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | EUINT256 _v ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
            | EUINT8 _v ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
            | FALSE ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | ID _v ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
            | IF ->
                _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | ISZERO ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | KECCAK ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | LAM ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | LBRACE ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState194
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194) : 'freshtv814)
        | ABORT | ADDRESS | BALANCE | BOOL | BYTES32 | ECDSARECOVER | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IF | ISZERO | KECCAK | LAM | LET | LOG | LPAR | NEW | NOT | NOW | RBRACE | RETURN | SELFDESTRUCT | SENDER | THIS | TRUE | UINT256 | UINT8 | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv815 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_1_inlined1 : 'tv_stmt)) = _menhir_stack in
            let _v : 'tv_stmt = let _4 =
              let _1 = _1_inlined1 in
              
# 86 "parser.mly"
                                                    ( [_1]                                                      )
# 281 "parser.ml"
              
            in
            
# 93 "parser.mly"
                                                    ( SmIf(_2 [],_4,[])                                         )
# 287 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv816)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv817 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv818)) : 'freshtv820)
    | MenhirState194 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv823 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv821 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_1_inlined1 : 'tv_stmt)), _, (_1_inlined2 : 'tv_stmt)) = _menhir_stack in
        let _v : 'tv_stmt = let _6 =
          let _1 = _1_inlined2 in
          
# 86 "parser.mly"
                                                    ( [_1]                                                      )
# 308 "parser.ml"
          
        in
        let _4 =
          let _1 = _1_inlined1 in
          
# 86 "parser.mly"
                                                    ( [_1]                                                      )
# 316 "parser.ml"
          
        in
        
# 92 "parser.mly"
                                                    ( SmIf(_2 [],_4,_6)                                         )
# 322 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv822)) : 'freshtv824)
    | MenhirState203 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv827 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv825 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_1_inlined1 : 'tv_block)), _, (_1_inlined2 : 'tv_stmt)) = _menhir_stack in
        let _v : 'tv_stmt = let _6 =
          let _1 = _1_inlined2 in
          
# 86 "parser.mly"
                                                    ( [_1]                                                      )
# 336 "parser.ml"
          
        in
        let _4 =
          let _1 = _1_inlined1 in
          
# 87 "parser.mly"
                                                    ( _1                                                        )
# 344 "parser.ml"
          
        in
        
# 92 "parser.mly"
                                                    ( SmIf(_2 [],_4,_6)                                         )
# 350 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv826)) : 'freshtv828)
    | MenhirState206 | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv829 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ABORT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | ADDRESS ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | BALANCE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | BOOL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | BYTES32 ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | ECDSARECOVER ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | EUINT256 _v ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _v
        | EUINT8 _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _v
        | FALSE ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | ID _v ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _v
        | IF ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | ISZERO ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | KECCAK ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | LAM ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | LET ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | LOG ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | NEW ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | NOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | NOW ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | RETURN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | SELFDESTRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | SENDER ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | THIS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | TRUE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | UINT256 ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | UINT8 ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | VALUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | RBRACE ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206) : 'freshtv830)
    | _ ->
        _menhir_fail ()

and _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv803 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv801 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_args)) = _menhir_stack in
        let _v : 'tv_args = 
# 163 "parser.mly"
                                                    ( fun ctx -> _1 ctx :: _3 ctx                                           )
# 437 "parser.ml"
         in
        _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv802)) : 'freshtv804)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv811 * _menhir_state) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv807 * _menhir_state) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv805 * _menhir_state) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_args)) = _menhir_stack in
            let _v : 'tv_arg_list = 
# 160 "parser.mly"
                                                    ( fun ctx -> _2 ctx                                                     )
# 456 "parser.ml"
             in
            _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv806)) : 'freshtv808)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv809 * _menhir_state) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv810)) : 'freshtv812)
    | _ ->
        _menhir_fail ()

and _menhir_run149 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149

and _menhir_reduce72 : _menhir_env -> (('ttv_tail * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (_1 : 'tv_lexpr)), _, (_3 : 'tv_tm)) = _menhir_stack in
    let _v : 'tv_tm = 
# 120 "parser.mly"
                                                    ( fun ctx -> TmAssign(_1 ctx, _3 ctx)                               ,() )
# 533 "parser.ml"
     in
    _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_value_info : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_value_info -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv799) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_value_info) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv797) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_value_info) : 'tv_value_info) = _v in
    ((let _v : 'tv_msg = 
# 169 "parser.mly"
                                                    ( _1                                                                    )
# 550 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv795) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_msg) = _v in
    ((match _menhir_s with
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv785 * _menhir_state * 'tv_tm)) * (
# 11 "parser.mly"
       (string)
# 562 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv783 * _menhir_state * 'tv_tm)) * (
# 11 "parser.mly"
       (string)
# 570 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_5 : 'tv_msg) : 'tv_msg) = _v in
        ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), (_3 : (
# 11 "parser.mly"
       (string)
# 577 "parser.ml"
        ))), _, (_4 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_tm = 
# 124 "parser.mly"
                                                    ( fun ctx -> EpSend{cn=_1 ctx; mthd=Some _3;args=_4 ctx; msg=_5 ctx},() )
# 582 "parser.ml"
         in
        _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv784)) : 'freshtv786)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv789 * _menhir_state * 'tv_tm))))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv787 * _menhir_state * 'tv_tm))))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_6 : 'tv_msg) : 'tv_msg) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_tm)) = _menhir_stack in
        let _v : 'tv_tm = 
# 123 "parser.mly"
                                                    ( fun ctx -> EpSend{cn=_1 ctx; mthd=None   ;args=[]    ; msg=_6 ctx},() )
# 598 "parser.ml"
         in
        _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv788)) : 'freshtv790)
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv793 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 606 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv791 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 614 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : 'tv_msg) : 'tv_msg) = _v in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 11 "parser.mly"
       (string)
# 621 "parser.ml"
        ))), _, (_3 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_aTm = 
# 155 "parser.mly"
                                       ( reserved _2; fun ctx -> EpNew {new_id=_2;new_args=_3 ctx; new_msg=_4 ctx}      ,() )
# 626 "parser.ml"
         in
        _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv792)) : 'freshtv794)
    | _ ->
        _menhir_fail ()) : 'freshtv796)) : 'freshtv798)) : 'freshtv800)

and _menhir_run107 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run114 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_run109 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_run116 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116

and _menhir_run123 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123

and _menhir_run111 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run129 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129

and _menhir_run131 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131

and _menhir_run136 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136

and _menhir_run118 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEFAULT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv777 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv773 * _menhir_state * 'tv_tm))) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv769 * _menhir_state * 'tv_tm)))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | WITH ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | ABORT | ADDRESS | BALANCE | COMMA | DOT | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
                    _menhir_reduce94 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142) : 'freshtv770)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv771 * _menhir_state * 'tv_tm)))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv772)) : 'freshtv774)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv775 * _menhir_state * 'tv_tm))) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv776)) : 'freshtv778)
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv779 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 1205 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119) : 'freshtv780)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv781 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv782)

and _menhir_goto_list_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_stmt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState206 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv733 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv731 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_stmt)), _, (xs : 'tv_list_stmt_)) = _menhir_stack in
        let _v : 'tv_list_stmt_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1238 "parser.ml"
         in
        _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv732)) : 'freshtv734)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv767 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv763 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv761 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_list_stmt_)) = _menhir_stack in
            let _v : 'tv_block = 
# 61 "parser.mly"
                                                    ( _2                                                        )
# 1257 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv759) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_block) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState194 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv737 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv735 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_1_inlined1 : 'tv_stmt)), _, (_1_inlined2 : 'tv_block)) = _menhir_stack in
                let _v : 'tv_stmt = let _6 =
                  let _1 = _1_inlined2 in
                  
# 87 "parser.mly"
                                                    ( _1                                                        )
# 1276 "parser.ml"
                  
                in
                let _4 =
                  let _1 = _1_inlined1 in
                  
# 86 "parser.mly"
                                                    ( [_1]                                                      )
# 1284 "parser.ml"
                  
                in
                
# 92 "parser.mly"
                                                    ( SmIf(_2 [],_4,_6)                                         )
# 1290 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv736)) : 'freshtv738)
            | MenhirState185 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv745 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ELSE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv739 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ABORT ->
                        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | ADDRESS ->
                        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | BALANCE ->
                        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | BOOL ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | BYTES32 ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | ECDSARECOVER ->
                        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | EUINT256 _v ->
                        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
                    | EUINT8 _v ->
                        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
                    | FALSE ->
                        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | ID _v ->
                        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
                    | IF ->
                        _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | ISZERO ->
                        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | KECCAK ->
                        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | LAM ->
                        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | LBRACE ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | LET ->
                        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | LOG ->
                        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | LPAR ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | NEW ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | NOT ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | NOW ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | RETURN ->
                        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | SELFDESTRUCT ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | SENDER ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | THIS ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | TRUE ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | UINT256 ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | UINT8 ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | VALUE ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState203
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203) : 'freshtv740)
                | ABORT | ADDRESS | BALANCE | BOOL | BYTES32 | ECDSARECOVER | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IF | ISZERO | KECCAK | LAM | LET | LOG | LPAR | NEW | NOT | NOW | RBRACE | RETURN | SELFDESTRUCT | SENDER | THIS | TRUE | UINT256 | UINT8 | VALUE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv741 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_1_inlined1 : 'tv_block)) = _menhir_stack in
                    let _v : 'tv_stmt = let _4 =
                      let _1 = _1_inlined1 in
                      
# 87 "parser.mly"
                                                    ( _1                                                        )
# 1376 "parser.ml"
                      
                    in
                    
# 93 "parser.mly"
                                                    ( SmIf(_2 [],_4,[])                                         )
# 1382 "parser.ml"
                     in
                    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv742)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv743 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv744)) : 'freshtv746)
            | MenhirState203 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv749 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv747 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_1_inlined1 : 'tv_block)), _, (_1_inlined2 : 'tv_block)) = _menhir_stack in
                let _v : 'tv_stmt = let _6 =
                  let _1 = _1_inlined2 in
                  
# 87 "parser.mly"
                                                    ( _1                                                        )
# 1403 "parser.ml"
                  
                in
                let _4 =
                  let _1 = _1_inlined1 in
                  
# 87 "parser.mly"
                                                    ( _1                                                        )
# 1411 "parser.ml"
                  
                in
                
# 92 "parser.mly"
                                                    ( SmIf(_2 [],_4,_6)                                         )
# 1417 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv748)) : 'freshtv750)
            | MenhirState50 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv757 * _menhir_state * 'tv_mthd_head) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv755 * _menhir_state * 'tv_mthd_head) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_mthd_head)), _, (_2 : 'tv_block)) = _menhir_stack in
                let _v : 'tv_mthd = 
# 58 "parser.mly"
                                                    ( TmMthd(_1,_2)                                             )
# 1429 "parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv753) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_mthd) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv751 * _menhir_state * 'tv_mthd) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | DEFAULT ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState211
                | METHOD ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState211
                | RBRACE ->
                    _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState211
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState211) : 'freshtv752)) : 'freshtv754)) : 'freshtv756)) : 'freshtv758)
            | _ ->
                _menhir_fail ()) : 'freshtv760)) : 'freshtv762)) : 'freshtv764)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv765 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv766)) : 'freshtv768)
    | _ ->
        _menhir_fail ()

and _menhir_reduce94 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_value_info = 
# 172 "parser.mly"
                                                    ( fun ctx -> TmZero, ()                                                 )
# 1468 "parser.ml"
     in
    _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v

and _menhir_run121 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121

and _menhir_goto_tm : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv533 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv529 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv527 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_aTm = 
# 149 "parser.mly"
                                                    ( fun ctx -> EpAddr (_3 ctx)                                        ,() )
# 1571 "parser.ml"
             in
            _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv528)) : 'freshtv530)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv531 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv532)) : 'freshtv534)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv539 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | MINUS | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv535 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 107 "parser.mly"
                                                    ( fun l r -> EpPlus(l,r)                                    )
# 1600 "parser.ml"
             in
            
# 126 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 1605 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv536)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv537 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv538)) : 'freshtv540)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv545 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv541 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 108 "parser.mly"
                                                    ( fun l r -> TmMul(l,r)                                    )
# 1632 "parser.ml"
             in
            
# 126 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 1637 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv542)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv543 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv544)) : 'freshtv546)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv571 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | RSQBR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv567 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT | ADDRESS | BALANCE | COMMA | DOT | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv547 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
                let _v : 'tv_tm = 
# 125 "parser.mly"
                                                    ( fun ctx -> EpArray{aid=_1 ctx;aidx=_3 ctx}                        ,() )
# 1686 "parser.ml"
                 in
                _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv548)
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv563 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
                let _v : 'tv_lexpr = 
# 176 "parser.mly"
                                                    ( fun ctx -> EpArray{aid=_1 ctx; aidx=_3 ctx}                           )
# 1696 "parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv561) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_lexpr) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                match _menhir_s with
                | MenhirState183 | MenhirState198 | MenhirState188 | MenhirState62 | MenhirState63 | MenhirState71 | MenhirState166 | MenhirState73 | MenhirState159 | MenhirState161 | MenhirState83 | MenhirState155 | MenhirState88 | MenhirState93 | MenhirState147 | MenhirState149 | MenhirState101 | MenhirState103 | MenhirState107 | MenhirState109 | MenhirState111 | MenhirState114 | MenhirState116 | MenhirState121 | MenhirState127 | MenhirState129 | MenhirState136 | MenhirState131 | MenhirState123 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv553 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                    ((assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | EQ ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv549 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | ABORT ->
                            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | ADDRESS ->
                            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | BALANCE ->
                            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | ECDSARECOVER ->
                            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | EUINT256 _v ->
                            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
                        | EUINT8 _v ->
                            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
                        | FALSE ->
                            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | ID _v ->
                            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
                        | IF ->
                            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | ISZERO ->
                            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | KECCAK ->
                            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | LAM ->
                            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | LET ->
                            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | LOG ->
                            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | LPAR ->
                            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | NEW ->
                            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | NOT ->
                            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | NOW ->
                            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | RETURN ->
                            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | SELFDESTRUCT ->
                            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | SENDER ->
                            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | THIS ->
                            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | TRUE ->
                            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | VALUE ->
                            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv550)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv551 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv552)) : 'freshtv554)
                | MenhirState51 | MenhirState206 | MenhirState203 | MenhirState185 | MenhirState194 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv559 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                    ((assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | EQ ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv555 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | ABORT ->
                            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | ADDRESS ->
                            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | BALANCE ->
                            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | ECDSARECOVER ->
                            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | EUINT256 _v ->
                            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
                        | EUINT8 _v ->
                            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
                        | FALSE ->
                            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | ID _v ->
                            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
                        | IF ->
                            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | ISZERO ->
                            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | KECCAK ->
                            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | LAM ->
                            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | LET ->
                            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | LOG ->
                            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | LPAR ->
                            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | NEW ->
                            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | NOT ->
                            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | NOW ->
                            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | RETURN ->
                            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | SELFDESTRUCT ->
                            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | SENDER ->
                            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | THIS ->
                            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | TRUE ->
                            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | VALUE ->
                            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState198
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198) : 'freshtv556)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv557 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv558)) : 'freshtv560)
                | _ ->
                    _menhir_fail ()) : 'freshtv562)) : 'freshtv564)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv565 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv566)) : 'freshtv568)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv569 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv570)) : 'freshtv572)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv577 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | NEQ | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv573 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 105 "parser.mly"
                                                    ( fun l r -> EpNEq(l,r)                                     )
# 1885 "parser.ml"
             in
            
# 126 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 1890 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv574)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv575 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv576)) : 'freshtv578)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv583 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | MINUS | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv579 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 109 "parser.mly"
                                                    ( fun l r -> TmMinus(l,r)                                   )
# 1919 "parser.ml"
             in
            
# 126 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 1924 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv580)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv581 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv582)) : 'freshtv584)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv589 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv585 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_value_info = 
# 173 "parser.mly"
                                                    ( _2                                                                    )
# 1967 "parser.ml"
             in
            _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v) : 'freshtv586)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv587 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv588)) : 'freshtv590)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv595 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | NEQ | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv591 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 102 "parser.mly"
                                                    ( fun l r -> EpLT(l,r)                                      )
# 2000 "parser.ml"
             in
            
# 126 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 2005 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv592)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv593 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv594)) : 'freshtv596)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv599 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv597 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv598)) : 'freshtv600)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv605 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LAND | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv601 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 106 "parser.mly"
                                                    ( fun l r -> EpLAnd(l,r)                                    )
# 2081 "parser.ml"
             in
            
# 126 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 2086 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv602)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv603 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv604)) : 'freshtv606)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv611 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | NEQ | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv607 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 103 "parser.mly"
                                                    ( fun l r -> EpGT(l,r)                                      )
# 2119 "parser.ml"
             in
            
# 126 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 2124 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv608)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv609 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv610)) : 'freshtv612)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv617 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | NEQ | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv613 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 104 "parser.mly"
                                                    ( fun l r -> TmEq(l,r)                                      )
# 2157 "parser.ml"
             in
            
# 126 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 2162 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv614)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv615 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv616)) : 'freshtv618)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv625 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv621 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv619 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_aTm = 
# 146 "parser.mly"
                                                    ( fun ctx -> EpBalance (_3 ctx)                                     ,() )
# 2208 "parser.ml"
             in
            _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv620)) : 'freshtv622)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv623 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv624)) : 'freshtv626)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv631 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv627 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | ADDRESS ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | BALANCE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | ECDSARECOVER ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | EUINT256 _v ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | EUINT8 _v ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | FALSE ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | ID _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | IF ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | ISZERO ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | KECCAK ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | LAM ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147) : 'freshtv628)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv629 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv630)) : 'freshtv632)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv635 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv633 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv634)) : 'freshtv636)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv641 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv637 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_4 : 'tv_tm)), _, (_6 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 119 "parser.mly"
                                                    ( fun ctx -> TmIf(_2 ctx, _4 ctx, _6 ctx)                           ,() )
# 2377 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv638)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv639 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv640)) : 'freshtv642)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv647 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2392 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv643 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2422 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (_2 : (
# 11 "parser.mly"
       (string)
# 2427 "parser.ml"
            ))), _, (_4 : 'tv_ty)), _, (_6 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 118 "parser.mly"
                                                    ( fun ctx -> TmAbs(_2, _4, _6(add_bruijn_idx ctx _2))               ,() )
# 2432 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv644)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv645 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2442 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv646)) : 'freshtv648)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv653 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2451 "parser.ml"
        )) * (
# 11 "parser.mly"
       (string)
# 2455 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv649 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2471 "parser.ml"
            )) * (
# 11 "parser.mly"
       (string)
# 2475 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | ADDRESS ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | BALANCE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | ECDSARECOVER ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | EUINT256 _v ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | EUINT8 _v ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | FALSE ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | ID _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | IF ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | ISZERO ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | KECCAK ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | LAM ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155) : 'freshtv650)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv651 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2553 "parser.ml"
            )) * (
# 11 "parser.mly"
       (string)
# 2557 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv652)) : 'freshtv654)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv659 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2566 "parser.ml"
        )) * (
# 11 "parser.mly"
       (string)
# 2570 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv655 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2600 "parser.ml"
            )) * (
# 11 "parser.mly"
       (string)
# 2604 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (((((((_menhir_stack, _menhir_s), _), (_3 : (
# 11 "parser.mly"
       (string)
# 2609 "parser.ml"
            ))), (_4 : (
# 11 "parser.mly"
       (string)
# 2613 "parser.ml"
            ))), _, (_6 : 'tv_ty)), _, (_8 : 'tv_tm)), _, (_10 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 114 "parser.mly"
                                                    ( fun ctx -> let ctx' = add_rec_idx ctx (_3^"'") in 
                                                                 let ctx''= add_struct_idx ctx' _4 in 
                                                                 let ctx  = add_bruijn_idx ctx (_3) in 
                                                                 TmApp((TmAbs((_3),_6,_10 ctx),()),(TmFix((_3^"'"),_4,_6,_8 ctx''),())), ())
# 2621 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv656)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv657 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2631 "parser.ml"
            )) * (
# 11 "parser.mly"
       (string)
# 2635 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv658)) : 'freshtv660)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv665 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 2644 "parser.ml"
        ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv661 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 2660 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | ADDRESS ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | BALANCE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | ECDSARECOVER ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | EUINT256 _v ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
            | EUINT8 _v ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
            | FALSE ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | ID _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
            | IF ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | ISZERO ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | KECCAK ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | LAM ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161) : 'freshtv662)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv663 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 2738 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv664)) : 'freshtv666)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv671 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 2747 "parser.ml"
        ))) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv667 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 2777 "parser.ml"
            ))) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), _, (_2 : 'tv_ty)), (_3 : (
# 11 "parser.mly"
       (string)
# 2782 "parser.ml"
            ))), _, (_5 : 'tv_tm)), _, (_7 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 113 "parser.mly"
                                                    ( fun ctx -> TmApp((TmAbs(_3,_2,_7(add_bruijn_idx ctx _3)),()),_5 ctx)   ,() )
# 2787 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv668)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv669 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 2797 "parser.ml"
            ))) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv670)) : 'freshtv672)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv679 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv675 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv673 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_aTm = 
# 137 "parser.mly"
                                                    ( _2 )
# 2837 "parser.ml"
             in
            _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv674)) : 'freshtv676)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv677 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv678)) : 'freshtv680)
    | MenhirState166 | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv687 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv681 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | ADDRESS ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | BALANCE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | ECDSARECOVER ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | EUINT256 _v ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | EUINT8 _v ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | FALSE ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | ID _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | IF ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | ISZERO ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | KECCAK ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | LAM ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166) : 'freshtv682)
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv683 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_args = 
# 162 "parser.mly"
                                                    ( fun ctx -> [_1 ctx]                                                   )
# 2938 "parser.ml"
             in
            _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv684)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv685 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv686)) : 'freshtv688)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv693 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv689 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_ret = 
# 99 "parser.mly"
                                                    ( _1                                                        )
# 2981 "parser.ml"
             in
            _menhir_goto_ret _menhir_env _menhir_stack _menhir_s _v) : 'freshtv690)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv691 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv692)) : 'freshtv694)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv699 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv695 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 122 "parser.mly"
                                                    ( fun ctx -> TmSlfDstrct(_2 ctx)                                    ,() )
# 3024 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv696)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv697 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv698)) : 'freshtv700)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv705 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv701 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | ADDRESS ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | BALANCE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | BYTES32 ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | ECDSARECOVER ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | EUINT256 _v ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | EUINT8 _v ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | FALSE ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | ID _v ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
            | IF ->
                _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | ISZERO ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | KECCAK ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | LAM ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | LBRACE ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState185
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185) : 'freshtv702)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv703 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv704)) : 'freshtv706)
    | MenhirState188 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv713 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 3140 "parser.ml"
        ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv709 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 3170 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv707 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 3177 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), (_2 : (
# 11 "parser.mly"
       (string)
# 3182 "parser.ml"
            ))), _, (_4 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 91 "parser.mly"
                                       ( reserved _2; SmDecl(_1,_2,_4 [])                                       )
# 3187 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv708)) : 'freshtv710)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv711 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 3197 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv712)) : 'freshtv714)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv717 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv715 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv716)) : 'freshtv718)
    | MenhirState206 | MenhirState51 | MenhirState203 | MenhirState194 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv721 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv719 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv720)) : 'freshtv722)
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv729 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv725 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv723 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_lexpr)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 90 "parser.mly"
                                                    ( SmAssign(_1 [],_3 [])                                     )
# 3309 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv724)) : 'freshtv726)
        | ELSE ->
            _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv727 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv728)) : 'freshtv730)
    | _ ->
        _menhir_fail ()

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_stmt_ = 
# 211 "<standard.mly>"
    ( [] )
# 3329 "parser.ml"
     in
    _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run183 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183

and _menhir_goto_arg_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_arg_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv495 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3401 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv493 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3407 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 11 "parser.mly"
       (string)
# 3412 "parser.ml"
        ))), _, (_3 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_tm = 
# 121 "parser.mly"
                                                    ( fun ctx -> TmLog(_2,_3 ctx,None)                                  ,() )
# 3417 "parser.ml"
         in
        _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)) : 'freshtv496)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv499 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv497 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_appTm = 
# 132 "parser.mly"
                                                    ( fun ctx -> EpCall{call_id="keccak256";call_args=_2 ctx}           ,() )
# 3429 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv498)) : 'freshtv500)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv503 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv501 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_appTm = 
# 130 "parser.mly"
                                                    ( fun ctx -> EpCall{call_id="iszero";call_args=_2 ctx}              ,() )
# 3441 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv502)) : 'freshtv504)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv507 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv505 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_appTm = 
# 131 "parser.mly"
                                                    ( fun ctx -> EpCall{call_id="pre_ecdsarecover";call_args=_2 ctx}    ,() )
# 3453 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv509 * _menhir_state * 'tv_tm)) * (
# 11 "parser.mly"
       (string)
# 3461 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | WITH ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | ABORT | ADDRESS | BALANCE | COMMA | DOT | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            _menhir_reduce94 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv510)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv511 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3479 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | WITH ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | ABORT | ADDRESS | BALANCE | COMMA | DOT | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            _menhir_reduce94 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170) : 'freshtv512)
    | MenhirState179 | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv525 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 3497 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv523 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 3503 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 11 "parser.mly"
       (string)
# 3508 "parser.ml"
        ))), _, (_2 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_call = 
# 157 "parser.mly"
                                                    ( fun ctx -> EpCall{call_id=_1;call_args=_2 ctx}                    ,() )
# 3513 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv521) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_call) = _v in
        ((match _menhir_s with
        | MenhirState178 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv515 * _menhir_state) * _menhir_state * 'tv_ret))) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_call) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv513 * _menhir_state) * _menhir_state * 'tv_ret))) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((_5 : 'tv_call) : 'tv_call) = _v in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_ret)) = _menhir_stack in
            let _v : 'tv_aTm = 
# 138 "parser.mly"
                                                    ( fun ctx -> TmReturn(_2 ctx,_5 ctx)                                ,() )
# 3533 "parser.ml"
             in
            _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv514)) : 'freshtv516)
        | MenhirState63 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv519) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_call) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv517) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_call) : 'tv_call) = _v in
            ((let _v : 'tv_ret = 
# 98 "parser.mly"
                                                    ( _1                                                        )
# 3548 "parser.ml"
             in
            _menhir_goto_ret _menhir_env _menhir_stack _menhir_s _v) : 'freshtv518)) : 'freshtv520)
        | _ ->
            _menhir_fail ()) : 'freshtv522)) : 'freshtv524)) : 'freshtv526)
    | _ ->
        _menhir_fail ()

and _menhir_goto_appTm : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_appTm -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv491 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | COMMA | DOT | ELSE | EQEQ | GT | IN | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv489 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_appTm)) = _menhir_stack in
        let _v : 'tv_tm = 
# 112 "parser.mly"
                                                    ( _1                                                                    )
# 3601 "parser.ml"
         in
        _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv490)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133) : 'freshtv492)

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | ADDRESS ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BOOL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BYTES32 ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | ID _v ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | RBRACE ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_goto_list_mthd_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_mthd_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState211 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv479 * _menhir_state * 'tv_mthd) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv477 * _menhir_state * 'tv_mthd) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_mthd)), _, (xs : 'tv_list_mthd_)) = _menhir_stack in
        let _v : 'tv_list_mthd_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 3691 "parser.ml"
         in
        _menhir_goto_list_mthd_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv478)) : 'freshtv480)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv487 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3699 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv483 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3709 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv481 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3716 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (_2 : (
# 11 "parser.mly"
       (string)
# 3721 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)), _, (_5 : 'tv_list_mthd_)) = _menhir_stack in
            let _v : 'tv_cntrct = let _3 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 3728 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 3733 "parser.ml"
                
              in
              
# 48 "parser.mly"
                                                    ( _1                                                        )
# 3739 "parser.ml"
              
            in
            
# 54 "parser.mly"
                                                    ( reserved _2; Cntrct{id=_2; mthds=_5; fields=_3}           )
# 3745 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv482)) : 'freshtv484)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv485 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3755 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv486)) : 'freshtv488)
    | _ ->
        _menhir_fail ()

and _menhir_goto_ret : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ret -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv475 * _menhir_state) * _menhir_state * 'tv_ret) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv471 * _menhir_state) * _menhir_state * 'tv_ret) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BECOME ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv467 * _menhir_state) * _menhir_state * 'tv_ret)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv465) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState178 in
                let (_v : (
# 11 "parser.mly"
       (string)
# 3789 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAR ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState179
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179) : 'freshtv466)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178) : 'freshtv468)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv469 * _menhir_state) * _menhir_state * 'tv_ret)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv470)) : 'freshtv472)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv473 * _menhir_state) * _menhir_state * 'tv_ret) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv474)) : 'freshtv476)

and _menhir_reduce14 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 11 "parser.mly"
       (string)
# 3823 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 11 "parser.mly"
       (string)
# 3829 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_aTm = 
# 150 "parser.mly"
                                       ( reserved _1; fun ctx -> prBds ctx;pe _1; begin 
                                                                 try TmIdx(lookup_bruijn_idx _1 ctx,len ctx),() with _ -> 
                                                                 try TmIdxRec(lookup_rec_idx (_1^"'") ctx)        ,() with _ -> 
                                                                 try TmIdxStrct(lookup_struct_idx _1 ctx)   ,() with _ -> 
                                                                     TmId _1,() end )
# 3838 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv463 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState71 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv461 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_arg_list = 
# 159 "parser.mly"
                                                    ( fun ctx -> []                                                         )
# 3898 "parser.ml"
         in
        _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv462)) : 'freshtv464)
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_goto_aTm : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_aTm -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv459) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_aTm) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv457) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_aTm) : 'tv_aTm) = _v in
    ((let _v : 'tv_pathTm = 
# 135 "parser.mly"
                                                    ( _1                                                                    )
# 3987 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv455) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_pathTm) = _v in
    ((match _menhir_s with
    | MenhirState51 | MenhirState206 | MenhirState183 | MenhirState203 | MenhirState185 | MenhirState198 | MenhirState194 | MenhirState188 | MenhirState62 | MenhirState63 | MenhirState71 | MenhirState166 | MenhirState73 | MenhirState159 | MenhirState161 | MenhirState83 | MenhirState155 | MenhirState88 | MenhirState93 | MenhirState147 | MenhirState149 | MenhirState101 | MenhirState103 | MenhirState107 | MenhirState109 | MenhirState111 | MenhirState114 | MenhirState116 | MenhirState121 | MenhirState127 | MenhirState129 | MenhirState136 | MenhirState131 | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv445) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_pathTm) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv443) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_pathTm) : 'tv_pathTm) = _v in
        ((let _v : 'tv_appTm = 
# 128 "parser.mly"
                                                    ( _1                                                                    )
# 4006 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv444)) : 'freshtv446)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv449 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_pathTm) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv447 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_pathTm) : 'tv_pathTm) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_appTm)) = _menhir_stack in
        let _v : 'tv_appTm = 
# 133 "parser.mly"
                                                    ( fun ctx -> TmApp(_1 ctx,_2 ctx)                                   ,() )
# 4022 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv448)) : 'freshtv450)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv453 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_pathTm) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv451 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_pathTm) : 'tv_pathTm) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_appTm = 
# 129 "parser.mly"
                                                    ( fun ctx -> EpNot (_2 ctx)                                         ,() )
# 4038 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv452)) : 'freshtv454)
    | _ ->
        _menhir_fail ()) : 'freshtv456)) : 'freshtv458)) : 'freshtv460)

and _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_evnt_arg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv437) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv435) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_evnt_arg_) : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ = 
# 144 "<standard.mly>"
    ( x )
# 4059 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv436)) : 'freshtv438)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv441 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv439 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_evnt_arg_) : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_evnt_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_evnt_arg_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4075 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv440)) : 'freshtv442)
    | _ ->
        _menhir_fail ()

and _menhir_goto_mthd_head : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_mthd_head -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv433 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv434)

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_mthd_ = 
# 211 "<standard.mly>"
    ( [] )
# 4101 "parser.ml"
     in
    _menhir_goto_list_mthd_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | BOOL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | BYTES32 ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv431 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState34 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv427 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv423 * _menhir_state) * _menhir_state)) = Obj.magic _menhir_stack in
                let (_v : (
# 11 "parser.mly"
       (string)
# 4139 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv419 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 4150 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ADDRESS ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                    | BOOL ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                    | BYTES32 ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                    | ID _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
                    | UINT256 ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                    | UINT8 ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                    | RPAR ->
                        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv420)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv421 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 4180 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv422)) : 'freshtv424)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv425 * _menhir_state) * _menhir_state)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv426)) : 'freshtv428)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv429 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv430)) : 'freshtv432)
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv417) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_mthd_head = 
# 64 "parser.mly"
                                                    ( TyDefault                                                 )
# 4216 "parser.ml"
     in
    _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv418)

and _menhir_run151 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 4223 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARROW | DARROW | ID _ ->
        _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack)
    | ABORT | ADDRESS | BALANCE | COMMA | DOT | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv415 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 4241 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)

and _menhir_run152 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
    | ARROW | DARROW | ID _ ->
        _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv413 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv414)

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv409 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MSG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv405 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv401 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv399 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_aTm = 
# 144 "parser.mly"
                                                    ( fun ctx -> EpValue                                                ,() )
# 4292 "parser.ml"
                 in
                _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv400)) : 'freshtv402)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv403 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv404)) : 'freshtv406)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv407 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv408)) : 'freshtv410)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv411 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv412)

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv397) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_aTm = 
# 140 "parser.mly"
                                                    ( fun ctx -> EpTrue                                                 ,() )
# 4326 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv395) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_aTm = 
# 148 "parser.mly"
                                                    ( fun ctx -> EpThis                                                 ,() )
# 4339 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv396)

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv391 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MSG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv387 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv383 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv381 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_aTm = 
# 145 "parser.mly"
                                                    ( fun ctx -> EpSender                                               ,() )
# 4371 "parser.ml"
                 in
                _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv382)) : 'freshtv384)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv385 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv386)) : 'freshtv388)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv389 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv390)) : 'freshtv392)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv393 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv394)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv377) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState63 in
        let (_v : (
# 11 "parser.mly"
       (string)
# 4482 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | ABORT | ADDRESS | BALANCE | DOT | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | LAND | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | SENDER | THEN | THIS | TRUE | VALUE ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173) : 'freshtv378)
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv379) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState63 in
        ((let _v : 'tv_ret = 
# 97 "parser.mly"
                                                    ( fun ctx -> TmUnit                                  ,()    )
# 4535 "parser.ml"
         in
        _menhir_goto_ret _menhir_env _menhir_stack _menhir_s _v) : 'freshtv380)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv373 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BLOCK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv369 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv365 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv363 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_aTm = 
# 147 "parser.mly"
                                                    ( fun ctx -> EpNow                                                  ,() )
# 4571 "parser.ml"
                 in
                _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv364)) : 'freshtv366)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv367 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)) : 'freshtv370)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv371 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv372)) : 'freshtv374)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv375 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv359 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 4649 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv360)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv361 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv362)

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv355 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 4740 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv356)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv357 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv358)

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | BOOL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | BYTES32 ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | REC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState77 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv349 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 4788 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv345 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 4799 "parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_v : (
# 11 "parser.mly"
       (string)
# 4804 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | COLON ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv341 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 4815 "parser.ml"
                    )) * (
# 11 "parser.mly"
       (string)
# 4819 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ADDRESS ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                    | BOOL ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                    | BYTES32 ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                    | ID _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
                    | UINT256 ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                    | UINT8 ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv342)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv343 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 4847 "parser.ml"
                    )) * (
# 11 "parser.mly"
       (string)
# 4851 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv344)) : 'freshtv346)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv347 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 4862 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)) : 'freshtv350)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv351 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv352)) : 'freshtv354)
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv337 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 4894 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv333 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 4905 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | BYTES32 ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv334)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv335 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 4933 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv339 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)

and _menhir_run89 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run93 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | ADDRESS ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | BALANCE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | ECDSARECOVER ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | EUINT256 _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | EUINT8 _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | FALSE ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | ID _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | IF ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | ISZERO ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | KECCAK ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | LAM ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run94 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 5033 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv331) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_aTm = 
# 141 "parser.mly"
                                                    ( fun ctx -> EpFalse                                                ,() )
# 5049 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv332)

and _menhir_run96 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "parser.mly"
       (Big_int.big_int)
# 5056 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv329) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 13 "parser.mly"
       (Big_int.big_int)
# 5066 "parser.ml"
    )) : (
# 13 "parser.mly"
       (Big_int.big_int)
# 5070 "parser.ml"
    )) = _v in
    ((let _v : 'tv_aTm = 
# 143 "parser.mly"
                                                    ( fun ctx -> TmUint _1                                           ,() )
# 5075 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv330)

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (Big_int.big_int)
# 5082 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv327) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 12 "parser.mly"
       (Big_int.big_int)
# 5092 "parser.ml"
    )) : (
# 12 "parser.mly"
       (Big_int.big_int)
# 5096 "parser.ml"
    )) = _v in
    ((let _v : 'tv_aTm = 
# 142 "parser.mly"
                                                    ( fun ctx -> TmUint _1                                           ,() )
# 5101 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)

and _menhir_run98 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_run100 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ABORT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ADDRESS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | BALANCE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ECDSARECOVER ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EUINT256 _v ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | EUINT8 _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | FALSE ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ID _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | IF ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ISZERO ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | KECCAK ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LAM ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LET ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LOG ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NEW ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NOW ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | RETURN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SELFDESTRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SENDER ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | THIS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TRUE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | VALUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv324)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv326)

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv322)

and _menhir_run104 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv319) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_aTm = 
# 139 "parser.mly"
                                                    ( fun ctx -> TmAbort                                                ,() )
# 5215 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)

and _menhir_goto_separated_nonempty_list_COMMA_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_arg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState46 | MenhirState38 | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv313) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv311) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_arg_) : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 144 "<standard.mly>"
    ( x )
# 5234 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)) : 'freshtv314)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv317 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv315 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_arg_) : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_arg_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 5250 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv316)) : 'freshtv318)
    | _ ->
        _menhir_fail ()

and _menhir_goto_evnt_arg : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_evnt_arg -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv309 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv303 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | BOOL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | BYTES32 ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | ID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
        | INDEXED ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | UINT256 ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | UINT8 ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv304)
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_evnt_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_evnt_arg_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 5295 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv306)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)) : 'freshtv310)

and _menhir_run13 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | BOOL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | BYTES32 ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | BOOL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | BYTES32 ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_cntrct : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_cntrct -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv301 * _menhir_state * 'tv_cntrct) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONTRACT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState218
    | EVENT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState218
    | EOF ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState218
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState218) : 'freshtv302)

and _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv283 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5383 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv279 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5393 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv275 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5403 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | DEFAULT ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | METHOD ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | RBRACE ->
                    _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv276)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv277 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5425 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)) : 'freshtv280)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv281 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5436 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv291 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 5445 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv287 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 5455 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv285 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 5462 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _), (_4 : (
# 11 "parser.mly"
       (string)
# 5467 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
            let _v : 'tv_mthd_head = let _5 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 5474 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 5479 "parser.ml"
                
              in
              
# 48 "parser.mly"
                                                    ( _1                                                        )
# 5485 "parser.ml"
              
            in
            
# 66 "parser.mly"
                                                    ( TyMthd(_4,_5,TyTuple[])                                   )
# 5491 "parser.ml"
             in
            _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv286)) : 'freshtv288)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv289 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 5501 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)) : 'freshtv292)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv299 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5510 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv295 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5520 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv293 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5527 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_ty)), (_3 : (
# 11 "parser.mly"
       (string)
# 5532 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
            let _v : 'tv_mthd_head = let _4 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 5539 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 5544 "parser.ml"
                
              in
              
# 48 "parser.mly"
                                                    ( _1                                                        )
# 5550 "parser.ml"
              
            in
            
# 65 "parser.mly"
                                                    ( TyMthd(_3,_4,_2)                                          )
# 5556 "parser.ml"
             in
            _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)) : 'freshtv296)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv297 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5566 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv298)) : 'freshtv300)
    | _ ->
        _menhir_fail ()

and _menhir_reduce93 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 11 "parser.mly"
       (string)
# 5576 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 11 "parser.mly"
       (string)
# 5582 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_ty = 
# 83 "parser.mly"
                                                    ( TyInstnc _1                                              )
# 5587 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv197 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv193 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 5611 "parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv191 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let ((_3 : (
# 11 "parser.mly"
       (string)
# 5619 "parser.ml"
            )) : (
# 11 "parser.mly"
       (string)
# 5623 "parser.ml"
            )) = _v in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_evnt_arg = 
# 73 "parser.mly"
                                                    ( TyEvVar(_3,_2,true)                                       )
# 5629 "parser.ml"
             in
            _menhir_goto_evnt_arg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv192)) : 'freshtv194)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv195 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)) : 'freshtv198)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv203 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | EQ | ID _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv199 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), _, (_3 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_ty = 
# 81 "parser.mly"
                                                    ( TyMap(_1,_3)                                              )
# 5656 "parser.ml"
             in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv200)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv201 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)) : 'freshtv204)
    | MenhirState88 | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv209 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | EQ | ID _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv205 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), _, (_3 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_ty = 
# 82 "parser.mly"
                                                    ( TyAbs(_1,_3)                                              )
# 5683 "parser.ml"
             in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv206)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv207 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)) : 'freshtv210)
    | MenhirState46 | MenhirState42 | MenhirState38 | MenhirState29 | MenhirState24 | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv231 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv227 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 5709 "parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv225 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let ((_2 : (
# 11 "parser.mly"
       (string)
# 5717 "parser.ml"
            )) : (
# 11 "parser.mly"
       (string)
# 5721 "parser.ml"
            )) = _v in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_arg = 
# 69 "parser.mly"
                                                    ( reserved _2; TyVar(_2,_1)                                 )
# 5727 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv223) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_arg) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState3 | MenhirState24 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv213 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv211 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_arg)) = _menhir_stack in
                let _v : 'tv_evnt_arg = 
# 72 "parser.mly"
                                                    ( let TyVar(id,ty)=_1 in TyEvVar(id,ty,false)               )
# 5744 "parser.ml"
                 in
                _menhir_goto_evnt_arg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv212)) : 'freshtv214)
            | MenhirState29 | MenhirState46 | MenhirState42 | MenhirState38 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv221 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | COMMA ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv215 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ADDRESS ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
                    | BOOL ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
                    | BYTES32 ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
                    | ID _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
                    | UINT256 ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
                    | UINT8 ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv216)
                | RPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv217 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, (x : 'tv_arg)) = _menhir_stack in
                    let _v : 'tv_separated_nonempty_list_COMMA_arg_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 5782 "parser.ml"
                     in
                    _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv218)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv219 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)) : 'freshtv222)
            | _ ->
                _menhir_fail ()) : 'freshtv224)) : 'freshtv226)) : 'freshtv228)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv229 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)) : 'freshtv232)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv241 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv237 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 5817 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv233 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5828 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | BOOL ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | BYTES32 ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | ID _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
                | UINT256 ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | UINT8 ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | RPAR ->
                    _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46) : 'freshtv234)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv235 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5858 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv236)) : 'freshtv238)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv239 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)) : 'freshtv242)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv247 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5874 "parser.ml"
        )) * (
# 11 "parser.mly"
       (string)
# 5878 "parser.ml"
        ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv243 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5892 "parser.ml"
            )) * (
# 11 "parser.mly"
       (string)
# 5896 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | ADDRESS ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | BALANCE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | ECDSARECOVER ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | EUINT256 _v ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | EUINT8 _v ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | FALSE ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | ID _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | IF ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | ISZERO ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | KECCAK ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LAM ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv245 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5960 "parser.ml"
            )) * (
# 11 "parser.mly"
       (string)
# 5964 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv253 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5973 "parser.ml"
        ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv249 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5983 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | ADDRESS ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | BALANCE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | BYTES32 ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | ECDSARECOVER ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | EUINT256 _v ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | EUINT8 _v ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | FALSE ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | ID _v ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | IF ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | ISZERO ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | KECCAK ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | LAM ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv250)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv251 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6057 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)) : 'freshtv254)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv259 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 6077 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv255 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6088 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ABORT ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | ADDRESS ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | BALANCE ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | ECDSARECOVER ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | EUINT256 _v ->
                    _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | EUINT8 _v ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | FALSE ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | ID _v ->
                    _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | IF ->
                    _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | ISZERO ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | KECCAK ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | LAM ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | LET ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | LOG ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | LPAR ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | NEW ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | NOT ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | NOW ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | RETURN ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | SELFDESTRUCT ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | SENDER ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | THIS ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | TRUE ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | VALUE ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159) : 'freshtv256)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv257 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6152 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)) : 'freshtv260)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv261 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)) : 'freshtv264)
    | MenhirState206 | MenhirState51 | MenhirState203 | MenhirState194 | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv273 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv269 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 6179 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv265 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6190 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ABORT ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | ADDRESS ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | BALANCE ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | ECDSARECOVER ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | EUINT256 _v ->
                    _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
                | EUINT8 _v ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
                | FALSE ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | ID _v ->
                    _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
                | IF ->
                    _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | ISZERO ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | KECCAK ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | LAM ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | LET ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | LOG ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | LPAR ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | NEW ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | NOT ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | NOW ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | RETURN ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | SELFDESTRUCT ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | SENDER ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | THIS ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | TRUE ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | VALUE ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188) : 'freshtv266)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv267 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6254 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)) : 'freshtv270)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv271 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)) : 'freshtv274)
    | _ ->
        _menhir_fail ()

and _menhir_reduce89 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : 'tv_ty = 
# 79 "parser.mly"
                                                    ( TyAddr                                                    )
# 6274 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_cntrct_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_cntrct_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv181 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv179 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_list_cntrct_)) = _menhir_stack in
            let _v : (
# 44 "parser.mly"
       (unit Syntax.toplevel list)
# 6297 "parser.ml"
            ) = 
# 51 "parser.mly"
                                                    ( _1                                                        )
# 6301 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv177) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 44 "parser.mly"
       (unit Syntax.toplevel list)
# 6309 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv175) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 44 "parser.mly"
       (unit Syntax.toplevel list)
# 6317 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv173) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 44 "parser.mly"
       (unit Syntax.toplevel list)
# 6325 "parser.ml"
            )) : (
# 44 "parser.mly"
       (unit Syntax.toplevel list)
# 6329 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv174)) : 'freshtv176)) : 'freshtv178)) : 'freshtv180)) : 'freshtv182)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv183 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)) : 'freshtv186)
    | MenhirState218 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state * 'tv_cntrct) * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv187 * _menhir_state * 'tv_cntrct) * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_cntrct)), _, (xs : 'tv_list_cntrct_)) = _menhir_stack in
        let _v : 'tv_list_cntrct_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 6348 "parser.ml"
         in
        _menhir_goto_list_cntrct_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv188)) : 'freshtv190)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv171 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6361 "parser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv167 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6371 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv163 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6381 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv161 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6388 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (_2 : (
# 11 "parser.mly"
       (string)
# 6393 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = _menhir_stack in
            let _v : 'tv_cntrct = let _3 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 6400 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 6405 "parser.ml"
                
              in
              
# 48 "parser.mly"
                                                    ( _1                                                        )
# 6411 "parser.ml"
              
            in
            
# 55 "parser.mly"
                                                    ( Event (TyEv(_2,_3))                                       )
# 6417 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv165 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6427 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv169 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6438 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)) : 'freshtv172)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | BOOL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | BYTES32 ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 142 "<standard.mly>"
    ( [] )
# 6471 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv159) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 77 "parser.mly"
                                                    ( TyU8                                                   )
# 6484 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv160)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv157) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 76 "parser.mly"
                                                    ( TyU256                                                 )
# 6497 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 6504 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv155) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 78 "parser.mly"
                                                    ( TyBytes32                                                 )
# 6520 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv153) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 80 "parser.mly"
                                                    ( TyBool                                                    )
# 6533 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv154)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState218 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_cntrct) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState211 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_mthd) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState206 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState203 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv27 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_lexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState194 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv31 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState188 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv33 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6581 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv35 * _menhir_state) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 6600 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv41 * _menhir_state) * _menhir_state * 'tv_ret))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 6614 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv45 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6623 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv49 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6637 "parser.ml"
        ))) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv51 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6646 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv53 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6655 "parser.ml"
        )) * (
# 11 "parser.mly"
       (string)
# 6659 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv55 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv57 * _menhir_state) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv59 * _menhir_state * 'tv_tm))))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * 'tv_lexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv75 * _menhir_state * 'tv_tm)) * (
# 11 "parser.mly"
       (string)
# 6718 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv77 * _menhir_state * 'tv_tm)) * (
# 11 "parser.mly"
       (string)
# 6727 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv101 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6791 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv103 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6800 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv105 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6809 "parser.ml"
        )) * (
# 11 "parser.mly"
       (string)
# 6813 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv107 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6822 "parser.ml"
        )) * (
# 11 "parser.mly"
       (string)
# 6826 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6840 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6859 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv129 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6893 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv131 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv133 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 6907 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv137 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6921 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv139 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6930 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv141 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv145 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv149 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6959 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv152)

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_cntrct_ = 
# 211 "<standard.mly>"
    ( [] )
# 6973 "parser.ml"
     in
    _menhir_goto_list_cntrct_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 6989 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 7000 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | BYTES32 ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | INDEXED ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState3 in
                ((let _v : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ = 
# 142 "<standard.mly>"
    ( [] )
# 7026 "parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv12)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv14)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv15 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 7040 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)) : 'freshtv18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 7064 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv3 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 7075 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | BYTES32 ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | RPAR ->
                _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv5 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 7105 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)) : 'freshtv8)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)

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
# 44 "parser.mly"
       (unit Syntax.toplevel list)
# 7132 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONTRACT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EVENT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 7160 "parser.ml"
