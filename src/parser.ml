
module MenhirBasics = struct
  
  exception Error
  
  type token = 
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
    | LBRACE
    | LAND
    | INDEXED
    | IF
    | ID of (
# 7 "parser.mly"
       (string)
# 43 "parser.ml"
  )
    | GT
    | FALSE
    | EVENT
    | EUINT8 of (
# 9 "parser.mly"
       (Big_int.big_int)
# 51 "parser.ml"
  )
    | EUINT256 of (
# 8 "parser.mly"
       (Big_int.big_int)
# 56 "parser.ml"
  )
    | EQEQ
    | EQ
    | EOF
    | ELSE
    | DOT
    | DEFAULT
    | DARROW
    | CONTRACT
    | COMMA
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
  | MenhirState183
  | MenhirState178
  | MenhirState175
  | MenhirState170
  | MenhirState166
  | MenhirState163
  | MenhirState159
  | MenhirState152
  | MenhirState150
  | MenhirState146
  | MenhirState141
  | MenhirState139
  | MenhirState135
  | MenhirState132
  | MenhirState127
  | MenhirState118
  | MenhirState114
  | MenhirState109
  | MenhirState107
  | MenhirState105
  | MenhirState103
  | MenhirState101
  | MenhirState100
  | MenhirState97
  | MenhirState93
  | MenhirState91
  | MenhirState88
  | MenhirState86
  | MenhirState84
  | MenhirState81
  | MenhirState79
  | MenhirState74
  | MenhirState72
  | MenhirState71
  | MenhirState68
  | MenhirState63
  | MenhirState52
  | MenhirState51
  | MenhirState46
  | MenhirState42
  | MenhirState37
  | MenhirState33
  | MenhirState31
  | MenhirState26
  | MenhirState21
  | MenhirState14
  | MenhirState3
  | MenhirState0

# 1 "parser.mly"
  
    open Misc
    open Syntax 

# 146 "parser.ml"

let rec _menhir_goto_list_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_stmt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv683 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv681 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_stmt)), _, (xs : 'tv_list_stmt_)) = _menhir_stack in
        let _v : 'tv_list_stmt_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 161 "parser.ml"
         in
        _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv682)) : 'freshtv684)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv717 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv713 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv711 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_list_stmt_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_block = 
# 54 "parser.mly"
                                                    ( _2                                                        )
# 182 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv709) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_block) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState163 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv687 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv685 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_stmt)), _, (_1_inlined2 : 'tv_block)) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_stmt = let _6 =
                  let _1 = _1_inlined2 in
                  
# 79 "parser.mly"
                                                    ( _1                                                        )
# 204 "parser.ml"
                  
                in
                let _4 =
                  let _1 = _1_inlined1 in
                  
# 78 "parser.mly"
                                                    ( [_1]                                                      )
# 212 "parser.ml"
                  
                in
                
# 87 "parser.mly"
                                                    ( SmIf(_2,_4,_6)                                            )
# 218 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv686)) : 'freshtv688)
            | MenhirState152 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv695 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ELSE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv689 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ABORT ->
                        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | ADDRESS ->
                        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | BALANCE ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | BOOL ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | BYTES32 ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | EUINT256 _v ->
                        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
                    | EUINT8 _v ->
                        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
                    | FALSE ->
                        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | ID _v ->
                        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
                    | IF ->
                        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | LBRACE ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | LOG ->
                        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | LPAR ->
                        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | NEW ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | NOT ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | NOW ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | RETURN ->
                        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | SELFDESTRUCT ->
                        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | SENDER ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | THIS ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | TRUE ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | UINT256 ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | UINT8 ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | VALUE ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175) : 'freshtv690)
                | ABORT | ADDRESS | BALANCE | BOOL | BYTES32 | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IF | LOG | LPAR | NEW | NOT | NOW | RBRACE | RETURN | SELFDESTRUCT | SENDER | THIS | TRUE | UINT256 | UINT8 | VALUE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv691 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_block)) = _menhir_stack in
                    let _3 = () in
                    let _1 = () in
                    let _v : 'tv_stmt = let _4 =
                      let _1 = _1_inlined1 in
                      
# 79 "parser.mly"
                                                    ( _1                                                        )
# 296 "parser.ml"
                      
                    in
                    
# 88 "parser.mly"
                                                    ( SmIfThen (_2, _4)                                         )
# 302 "parser.ml"
                     in
                    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv692)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv693 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv694)) : 'freshtv696)
            | MenhirState175 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv699 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv697 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_block)), _, (_1_inlined2 : 'tv_block)) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_stmt = let _6 =
                  let _1 = _1_inlined2 in
                  
# 79 "parser.mly"
                                                    ( _1                                                        )
# 326 "parser.ml"
                  
                in
                let _4 =
                  let _1 = _1_inlined1 in
                  
# 79 "parser.mly"
                                                    ( _1                                                        )
# 334 "parser.ml"
                  
                in
                
# 87 "parser.mly"
                                                    ( SmIf(_2,_4,_6)                                            )
# 340 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv698)) : 'freshtv700)
            | MenhirState51 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv707 * _menhir_state * 'tv_mthd_head) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv705 * _menhir_state * 'tv_mthd_head) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_mthd_head)), _, (_2 : 'tv_block)) = _menhir_stack in
                let _v : 'tv_mthd = 
# 51 "parser.mly"
                                                    ( {mthd_head=_1; mthd_body=_2}                              )
# 352 "parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv703) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_mthd) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv701 * _menhir_state * 'tv_mthd) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | DEFAULT ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                | METHOD ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                | RBRACE ->
                    _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183) : 'freshtv702)) : 'freshtv704)) : 'freshtv706)) : 'freshtv708)
            | _ ->
                _menhir_fail ()) : 'freshtv710)) : 'freshtv712)) : 'freshtv714)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv715 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv716)) : 'freshtv718)
    | _ ->
        _menhir_fail ()

and _menhir_reduce80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_value_info = 
# 132 "parser.mly"
                                                    ( EpFalse,()                                                            )
# 391 "parser.ml"
     in
    _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v

and _menhir_run101 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_stmt_ = 
# 211 "<standard.mly>"
    ( [] )
# 439 "parser.ml"
     in
    _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv653 * _menhir_state * 'tv_expr)) * (
# 7 "parser.mly"
       (string)
# 452 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv649 * _menhir_state * 'tv_expr)) * (
# 7 "parser.mly"
       (string)
# 462 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ALONG ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | COMMA | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
                _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv650)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv651 * _menhir_state * 'tv_expr)) * (
# 7 "parser.mly"
       (string)
# 482 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv652)) : 'freshtv654)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv661 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 491 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv657 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 501 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv655 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 508 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 7 "parser.mly"
       (string)
# 513 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _3 = () in
            let _1_inlined1 = () in
            let _v : 'tv_expr = let _2 =
              let _1 = _1_inlined1 in
              let _1 =
                let _1 =
                  let x = 
# 232 "<standard.mly>"
    ( xs )
# 524 "parser.ml"
                   in
                  
# 200 "<standard.mly>"
    ( x )
# 529 "parser.ml"
                  
                in
                
# 41 "parser.mly"
                                                    ( _1                                                        )
# 535 "parser.ml"
                
              in
              
# 126 "parser.mly"
                                                    ( _1                                                                    )
# 541 "parser.ml"
              
            in
            
# 118 "parser.mly"
                                                    ( Printf.printf "\n%s\n" _1; EpCall{call_id=_1;call_args=_2},       ()  )
# 547 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv656)) : 'freshtv658)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv659 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 557 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv660)) : 'freshtv662)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv667 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 566 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv663 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 576 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ALONG ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | COMMA | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
                _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv664)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv665 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 596 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv666)) : 'freshtv668)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv679 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 605 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv675 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 615 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv671 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 625 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv669 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 632 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s), (_2 : (
# 7 "parser.mly"
       (string)
# 637 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1_inlined1 = () in
                let _1 = () in
                let _v : 'tv_stmt = let _3 =
                  let _1 = _1_inlined1 in
                  let _1 =
                    let _1 =
                      let x = 
# 232 "<standard.mly>"
    ( xs )
# 650 "parser.ml"
                       in
                      
# 200 "<standard.mly>"
    ( x )
# 655 "parser.ml"
                      
                    in
                    
# 41 "parser.mly"
                                                    ( _1                                                        )
# 661 "parser.ml"
                    
                  in
                  
# 126 "parser.mly"
                                                    ( _1                                                                    )
# 667 "parser.ml"
                  
                in
                
# 89 "parser.mly"
                                                    ( SmLog(_2,_3,None)                                         )
# 673 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv670)) : 'freshtv672)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv673 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 683 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv674)) : 'freshtv676)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv677 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 694 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv678)) : 'freshtv680)
    | _ ->
        _menhir_fail ()

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run132 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv647) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState132 in
        ((let _v : 'tv_option_expr_ = 
# 114 "<standard.mly>"
    ( None )
# 781 "parser.ml"
         in
        _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv648)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv645 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState139 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv641 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | EUINT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | EUINT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | ID _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | SENDER ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | THIS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | VALUE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141) : 'freshtv642)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv643 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv644)) : 'freshtv646)
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139

and _menhir_run144 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv637 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 7 "parser.mly"
       (string)
# 893 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv633 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 904 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | EUINT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | EUINT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | ID _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | SENDER ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | THIS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | VALUE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | RPAR ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146) : 'freshtv634)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv635 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 950 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv636)) : 'freshtv638)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv639 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv640)

and _menhir_run150 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState150
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState150
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState150
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState150
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState150
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState150
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState150
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState150
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState150
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState150
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState150
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150

and _menhir_run153 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 1004 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
    | DARROW | ID _ ->
        _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack)
    | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS ->
        _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv631 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 1024 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv632)

and _menhir_run154 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
    | DARROW | ID _ ->
        _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv629 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv630)

and _menhir_run155 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv625 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv623 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_stmt = 
# 82 "parser.mly"
                                                    ( SmAbort                                                   )
# 1065 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv624)) : 'freshtv626)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv627 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv628)

and _menhir_goto_option_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv621 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv617 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BECOME ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv613 * _menhir_state) * _menhir_state * 'tv_option_expr_)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | EUINT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | EUINT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | ID _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | SENDER ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | THIS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | VALUE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135) : 'freshtv614)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv615 * _menhir_state) * _menhir_state * 'tv_option_expr_)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv616)) : 'freshtv618)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv619 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv620)) : 'freshtv622)

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_stmt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv601 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv595 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | ADDRESS ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | EUINT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | EUINT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | ID _v ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | IF ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | LBRACE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | LOG ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | LPAR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | RETURN ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | SELFDESTRUCT ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | SENDER ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | THIS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | VALUE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163) : 'freshtv596)
        | ABORT | ADDRESS | BALANCE | BOOL | BYTES32 | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IF | LOG | LPAR | NEW | NOT | NOW | RBRACE | RETURN | SELFDESTRUCT | SENDER | THIS | TRUE | UINT256 | UINT8 | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv597 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_stmt)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_stmt = let _4 =
              let _1 = _1_inlined1 in
              
# 78 "parser.mly"
                                                    ( [_1]                                                      )
# 1222 "parser.ml"
              
            in
            
# 88 "parser.mly"
                                                    ( SmIfThen (_2, _4)                                         )
# 1228 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv598)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv599 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv600)) : 'freshtv602)
    | MenhirState163 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv605 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv603 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_stmt)), _, (_1_inlined2 : 'tv_stmt)) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_stmt = let _6 =
          let _1 = _1_inlined2 in
          
# 78 "parser.mly"
                                                    ( [_1]                                                      )
# 1252 "parser.ml"
          
        in
        let _4 =
          let _1 = _1_inlined1 in
          
# 78 "parser.mly"
                                                    ( [_1]                                                      )
# 1260 "parser.ml"
          
        in
        
# 87 "parser.mly"
                                                    ( SmIf(_2,_4,_6)                                            )
# 1266 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv604)) : 'freshtv606)
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv609 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv607 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_block)), _, (_1_inlined2 : 'tv_stmt)) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_stmt = let _6 =
          let _1 = _1_inlined2 in
          
# 78 "parser.mly"
                                                    ( [_1]                                                      )
# 1283 "parser.ml"
          
        in
        let _4 =
          let _1 = _1_inlined1 in
          
# 79 "parser.mly"
                                                    ( _1                                                        )
# 1291 "parser.ml"
          
        in
        
# 87 "parser.mly"
                                                    ( SmIf(_2,_4,_6)                                            )
# 1297 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv608)) : 'freshtv610)
    | MenhirState178 | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv611 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ABORT ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | ADDRESS ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | BALANCE ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | BOOL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | BYTES32 ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | EUINT256 _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
        | EUINT8 _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
        | FALSE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | ID _v ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
        | IF ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | LOG ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | LPAR ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | NEW ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | NOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | NOW ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | RETURN ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | SELFDESTRUCT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | SENDER ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | THIS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | TRUE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | UINT256 ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | UINT8 ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | VALUE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | RBRACE ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178) : 'freshtv612)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState146 | MenhirState71 | MenhirState74 | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv589) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv587) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 144 "<standard.mly>"
    ( x )
# 1376 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv588)) : 'freshtv590)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv593 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv591 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1393 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv592)) : 'freshtv594)
    | _ ->
        _menhir_fail ()

and _menhir_goto_value_info : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_value_info -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv585) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_value_info) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv583) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_value_info) : 'tv_value_info) = _v in
    ((let _v : 'tv_msg = 
# 129 "parser.mly"
                                                    ( _1                                                                    )
# 1412 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv581) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_msg) = _v in
    ((match _menhir_s with
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv571 * _menhir_state * 'tv_expr)) * (
# 7 "parser.mly"
       (string)
# 1424 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv569 * _menhir_state * 'tv_expr)) * (
# 7 "parser.mly"
       (string)
# 1432 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_5 : 'tv_msg) : 'tv_msg) = _v in
        ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), (_3 : (
# 7 "parser.mly"
       (string)
# 1439 "parser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
        let _3_inlined1 = () in
        let _1_inlined1 = () in
        let _2 = () in
        let _v : 'tv_expr = let _4 =
          let (_3, _1) = (_3_inlined1, _1_inlined1) in
          let _1 =
            let _1 =
              let x = 
# 232 "<standard.mly>"
    ( xs )
# 1451 "parser.ml"
               in
              
# 200 "<standard.mly>"
    ( x )
# 1456 "parser.ml"
              
            in
            
# 41 "parser.mly"
                                                    ( _1                                                        )
# 1462 "parser.ml"
            
          in
          
# 126 "parser.mly"
                                                    ( _1                                                                    )
# 1468 "parser.ml"
          
        in
        
# 121 "parser.mly"
                                                    ( EpSend{sd_cn=_1;sd_mthd=Some _3;sd_args=_4;sd_msg=_5},            ()  )
# 1474 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv570)) : 'freshtv572)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv575 * _menhir_state * 'tv_expr))))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv573 * _menhir_state * 'tv_expr))))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_6 : 'tv_msg) : 'tv_msg) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_expr)) = _menhir_stack in
        let _5 = () in
        let _4 = () in
        let _3 = () in
        let _2 = () in
        let _v : 'tv_expr = 
# 120 "parser.mly"
                                                    ( EpSend{sd_cn=_1;sd_mthd=None   ;sd_args=[];sd_msg=_6},            ()  )
# 1494 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv574)) : 'freshtv576)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv579 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1502 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv577 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 1510 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : 'tv_msg) : 'tv_msg) = _v in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 7 "parser.mly"
       (string)
# 1517 "parser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
        let _3 = () in
        let _1_inlined1 = () in
        let _1 = () in
        let _v : 'tv_expr = let _3 =
          let _1 = _1_inlined1 in
          let _1 =
            let _1 =
              let x = 
# 232 "<standard.mly>"
    ( xs )
# 1529 "parser.ml"
               in
              
# 200 "<standard.mly>"
    ( x )
# 1534 "parser.ml"
              
            in
            
# 41 "parser.mly"
                                                    ( _1                                                        )
# 1540 "parser.ml"
            
          in
          
# 126 "parser.mly"
                                                    ( _1                                                                    )
# 1546 "parser.ml"
          
        in
        
# 119 "parser.mly"
                                                    ( EpNew {new_id=_2;new_args=_3; new_msg=_4},                        ()  )
# 1552 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv578)) : 'freshtv580)
    | _ ->
        _menhir_fail ()) : 'freshtv582)) : 'freshtv584)) : 'freshtv586)

and _menhir_reduce32 : _menhir_env -> ((('ttv_tail * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _v : 'tv_expr = 
# 123 "parser.mly"
                                                    ( EpArray{arrId=_1;arrIndex=_3},                                    ()  )
# 1566 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run105 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run107 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run109 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEFAULT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv559 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv555 * _menhir_state * 'tv_expr))) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv551 * _menhir_state * 'tv_expr)))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ALONG ->
                    _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | COMMA | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
                    _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118) : 'freshtv552)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv553 * _menhir_state * 'tv_expr)))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv554)) : 'freshtv556)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv557 * _menhir_state * 'tv_expr))) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv558)) : 'freshtv560)
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv565 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        let (_v : (
# 7 "parser.mly"
       (string)
# 1963 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv561 * _menhir_state * 'tv_expr)) * (
# 7 "parser.mly"
       (string)
# 1974 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | EUINT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | EUINT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | ID _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | SENDER ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | THIS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | VALUE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | RPAR ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv562)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv563 * _menhir_state * 'tv_expr)) * (
# 7 "parser.mly"
       (string)
# 2020 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv564)) : 'freshtv566)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv567 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv568)

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | ADDRESS ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | BOOL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | BYTES32 ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | ID _v ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | IF ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LOG ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LPAR ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | RETURN ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SELFDESTRUCT ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | RBRACE ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_goto_list_mthd_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_mthd_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv541 * _menhir_state * 'tv_mthd) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv539 * _menhir_state * 'tv_mthd) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_mthd)), _, (xs : 'tv_list_mthd_)) = _menhir_stack in
        let _v : 'tv_list_mthd_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2104 "parser.ml"
         in
        _menhir_goto_list_mthd_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv540)) : 'freshtv542)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv549 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 2112 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv545 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 2122 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv543 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 2129 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (_2 : (
# 7 "parser.mly"
       (string)
# 2134 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)), _, (_5 : 'tv_list_mthd_)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1_inlined1 = () in
            let _1 = () in
            let _v : 'tv_cntrct = let _3 =
              let _1 = _1_inlined1 in
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 2147 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 2152 "parser.ml"
                
              in
              
# 41 "parser.mly"
                                                    ( _1                                                        )
# 2158 "parser.ml"
              
            in
            
# 47 "parser.mly"
                                                    ( Cntrct{mthds=_5; cntrct_id=_2; cntrct_args=_3}            )
# 2164 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv544)) : 'freshtv546)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv547 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 2174 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv548)) : 'freshtv550)
    | _ ->
        _menhir_fail ()

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 142 "<standard.mly>"
    ( [] )
# 2186 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce26 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 7 "parser.mly"
       (string)
# 2193 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 7 "parser.mly"
       (string)
# 2199 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_expr = 
# 117 "parser.mly"
                                                    ( EpIdent _1,                                                       ()  )
# 2204 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 7 "parser.mly"
       (string)
# 2211 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | RPAR ->
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv371 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv367 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv365 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 116 "parser.mly"
                                                    ( EpAddr _3,                                                        ()  )
# 2295 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv366)) : 'freshtv368)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv369 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv370)) : 'freshtv372)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv377 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | MINUS | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv373 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : 'tv_expr = let _2 =
              let _1 = _1_inlined1 in
              
# 93 "parser.mly"
                                                    ( fun(l,r)-> EpPlus(l,r)                                    )
# 2327 "parser.ml"
              
            in
            
# 109 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2333 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv374)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv375 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)) : 'freshtv378)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv383 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | MINUS | MULT | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv379 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : 'tv_expr = let _2 =
              let _1 = _1_inlined1 in
              
# 95 "parser.mly"
                                                    ( fun(l,r)-> EpMult(l,r)                                    )
# 2363 "parser.ml"
              
            in
            
# 109 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2369 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv380)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv381 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv382)) : 'freshtv384)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv389 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | RSQBR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv385 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv386)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv387 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv388)) : 'freshtv390)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv395 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | NEQ | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv391 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : 'tv_expr = let _2 =
              let _1 = _1_inlined1 in
              
# 99 "parser.mly"
                                                    ( fun(l,r)-> EpNEq(l,r)                                     )
# 2443 "parser.ml"
              
            in
            
# 109 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2449 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv392)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv393 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv394)) : 'freshtv396)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv401 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | MINUS | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv397 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : 'tv_expr = let _2 =
              let _1 = _1_inlined1 in
              
# 94 "parser.mly"
                                                    ( fun(l,r)-> EpMinus(l,r)                                   )
# 2481 "parser.ml"
              
            in
            
# 109 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2487 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv399 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv400)) : 'freshtv402)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv407 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv403 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_value_info = 
# 133 "parser.mly"
                                                    ( _2                                                                    )
# 2531 "parser.ml"
             in
            _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v) : 'freshtv404)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv405 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv406)) : 'freshtv408)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv413 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | NEQ | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv409 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : 'tv_expr = let _2 =
              let _1 = _1_inlined1 in
              
# 96 "parser.mly"
                                                    ( fun(l,r)-> EpLT(l,r)                                      )
# 2567 "parser.ml"
              
            in
            
# 109 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2573 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv410)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv411 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv412)) : 'freshtv414)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv419 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | LAND | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv415 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : 'tv_expr = let _2 =
              let _1 = _1_inlined1 in
              
# 100 "parser.mly"
                                                    ( fun(l,r)-> EpLAnd(l,r)                                    )
# 2617 "parser.ml"
              
            in
            
# 109 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2623 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv416)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv417 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv418)) : 'freshtv420)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv425 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | NEQ | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv421 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : 'tv_expr = let _2 =
              let _1 = _1_inlined1 in
              
# 97 "parser.mly"
                                                    ( fun(l,r)-> EpGT(l,r)                                      )
# 2659 "parser.ml"
              
            in
            
# 109 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2665 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv422)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv423 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv424)) : 'freshtv426)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv431 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | NEQ | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv427 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : 'tv_expr = let _2 =
              let _1 = _1_inlined1 in
              
# 98 "parser.mly"
                                                    ( fun(l,r)-> EpEq(l,r)                                      )
# 2701 "parser.ml"
              
            in
            
# 109 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2707 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv428)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv429 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv430)) : 'freshtv432)
    | MenhirState146 | MenhirState71 | MenhirState74 | MenhirState114 | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv439 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv433 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | EUINT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | EUINT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | ID _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | SENDER ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | THIS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | VALUE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv434)
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv435 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2788 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv436)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv437 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv438)) : 'freshtv440)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv447 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv443 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv441 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 114 "parser.mly"
                                                    ( EpBalance _3,                                                     ()  )
# 2837 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)) : 'freshtv444)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv445 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv446)) : 'freshtv448)
    | MenhirState139 | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv455 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv451 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv449 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 111 "parser.mly"
                                                    ( EpParen _2,                                                       ()  )
# 2885 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv450)) : 'freshtv452)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv453 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv454)) : 'freshtv456)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv461 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv457 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr = 
# 110 "parser.mly"
                                                    ( EpNot _2,                                                         ()  )
# 2929 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv458)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv459 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv460)) : 'freshtv462)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv469 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv465 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv463 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_stmt = 
# 90 "parser.mly"
                                                    ( SmSlfDstrct _2                                            )
# 2977 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv464)) : 'freshtv466)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv467 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv468)) : 'freshtv470)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv477 * _menhir_state) * _menhir_state * 'tv_option_expr_))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv473 * _menhir_state) * _menhir_state * 'tv_option_expr_))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv471 * _menhir_state) * _menhir_state * 'tv_option_expr_))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_option_expr_)), _, (_5 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_stmt = 
# 83 "parser.mly"
                                                    ( SmReturn{ret_expr=_2; ret_cont=_5}                        )
# 3027 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv472)) : 'freshtv474)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv475 * _menhir_state) * _menhir_state * 'tv_option_expr_))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv476)) : 'freshtv478)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv483 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv479 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_option_expr_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3070 "parser.ml"
             in
            _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv480)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv481 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv482)) : 'freshtv484)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv491 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv487 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv485 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _), _, (_4 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_stmt = 
# 86 "parser.mly"
                                                    ( SmExpr _4                                                 )
# 3120 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv486)) : 'freshtv488)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv489 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv490)) : 'freshtv492)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv497 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv493 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | ADDRESS ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | EUINT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | EUINT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | ID _v ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | IF ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | LBRACE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | LOG ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | LPAR ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | RETURN ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | SELFDESTRUCT ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | SENDER ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | THIS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | VALUE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152) : 'freshtv494)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv495 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv496)) : 'freshtv498)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv505 * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 3226 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv501 * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 3256 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv499 * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 3263 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), (_2 : (
# 7 "parser.mly"
       (string)
# 3268 "parser.ml"
            ))), _, (_4 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : 'tv_stmt = 
# 85 "parser.mly"
                                                    ( SmDecl{declTy=_1; declId=_2; declVal=_4}   )
# 3275 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv500)) : 'freshtv502)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv503 * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 3285 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv504)) : 'freshtv506)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv513 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv509 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv507 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_lexpr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_stmt = 
# 84 "parser.mly"
                                                    ( SmAssign(_1,_3)                                           )
# 3327 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv508)) : 'freshtv510)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv511 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv512)) : 'freshtv514)
    | MenhirState52 | MenhirState178 | MenhirState175 | MenhirState152 | MenhirState163 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv519 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv515 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | EUINT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
            | EUINT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | ID _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | SENDER ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | THIS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | VALUE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170) : 'freshtv516)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv517 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv518)) : 'freshtv520)
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv537 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | RSQBR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv533 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv529 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _v : 'tv_lexpr = 
# 136 "parser.mly"
                                                    ( LEpArray{arrId=_1; arrIndex=_3}                                       )
# 3447 "parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv527) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_lexpr) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv525 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | EQ ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv521 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ADDRESS ->
                        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                    | BALANCE ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                    | EUINT256 _v ->
                        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
                    | EUINT8 _v ->
                        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
                    | FALSE ->
                        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                    | ID _v ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
                    | LPAR ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                    | NEW ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                    | NOT ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                    | NOW ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                    | SENDER ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                    | THIS ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                    | TRUE ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                    | VALUE ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166) : 'freshtv522)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv523 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv524)) : 'freshtv526)) : 'freshtv528)) : 'freshtv530)
            | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS ->
                _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv531 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv532)) : 'freshtv534)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv535 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv536)) : 'freshtv538)
    | _ ->
        _menhir_fail ()

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_evnt_arg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv359) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv357) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_evnt_arg_) : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ = 
# 144 "<standard.mly>"
    ( x )
# 3576 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv358)) : 'freshtv360)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv363 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv361 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_evnt_arg_) : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_evnt_arg)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_evnt_arg_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3593 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv362)) : 'freshtv364)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_arg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState46 | MenhirState37 | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_arg_) : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 144 "<standard.mly>"
    ( x )
# 3614 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)) : 'freshtv352)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv355 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv353 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_arg_) : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_arg)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_arg_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3631 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv354)) : 'freshtv356)
    | _ ->
        _menhir_fail ()

and _menhir_goto_mthd_head : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_mthd_head -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv347 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51) : 'freshtv348)

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_mthd_ = 
# 211 "<standard.mly>"
    ( [] )
# 3657 "parser.ml"
     in
    _menhir_goto_list_mthd_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv343 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | BOOL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | BYTES32 ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv341 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState33 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv337 * _menhir_state)) * _menhir_state) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ID _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv333 * _menhir_state)) * _menhir_state)) = Obj.magic _menhir_stack in
                    let (_v : (
# 7 "parser.mly"
       (string)
# 3701 "parser.ml"
                    )) = _v in
                    ((let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | LPAR ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv329 * _menhir_state)) * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 3712 "parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | ADDRESS ->
                            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                        | BOOL ->
                            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                        | BYTES32 ->
                            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                        | ID _v ->
                            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
                        | UINT256 ->
                            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                        | UINT8 ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                        | RPAR ->
                            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv330)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv331 * _menhir_state)) * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 3742 "parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)) : 'freshtv334)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv335 * _menhir_state)) * _menhir_state)) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv339 * _menhir_state)) * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)) : 'freshtv342)
        | UINT256 ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | UINT8 ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv344)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv345 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv346)

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv327) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_mthd_head = 
# 57 "parser.mly"
                                                    ( TyDefault                                                 )
# 3786 "parser.ml"
     in
    _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | MSG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv319 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv315 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv313 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_expr = 
# 112 "parser.mly"
                                                    ( EpValue,                                                          ()  )
# 3822 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)) : 'freshtv316)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv317 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv318)) : 'freshtv320)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv321 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv322)) : 'freshtv324)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv326)

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv311) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_expr = 
# 104 "parser.mly"
                                                    ( EpTrue,                                                           ()  )
# 3857 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv309) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_expr = 
# 122 "parser.mly"
                                                    ( EpThis,                                                           ()  )
# 3871 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MSG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv301 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv297 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv295 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_expr = 
# 113 "parser.mly"
                                                    ( EpSender,                                                         ()  )
# 3907 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv296)) : 'freshtv298)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv299 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)) : 'freshtv302)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv303 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv304)) : 'freshtv306)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BLOCK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv287 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv283 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv281 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_expr = 
# 115 "parser.mly"
                                                    ( EpNow,                                                            ()  )
# 3964 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv282)) : 'freshtv284)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv285 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv286)) : 'freshtv288)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv289 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)) : 'freshtv292)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv293 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv294)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68
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
        let (_menhir_stack : 'freshtv277 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 7 "parser.mly"
       (string)
# 4040 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv273 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 4051 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | EUINT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | EUINT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | ID _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | SENDER ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | THIS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | VALUE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | RPAR ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv274)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv275 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 4097 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)) : 'freshtv278)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv279 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv280)

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | EUINT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | EUINT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | SENDER ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | THIS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | VALUE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 4151 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
        _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv271 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 4169 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv269) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_expr = 
# 105 "parser.mly"
                                                    ( EpFalse,                                                          ()  )
# 4184 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Big_int.big_int)
# 4191 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv267) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 9 "parser.mly"
       (Big_int.big_int)
# 4201 "parser.ml"
    )) : (
# 9 "parser.mly"
       (Big_int.big_int)
# 4205 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 108 "parser.mly"
                                                    ( EpUint8 _1,                                                     ()  )
# 4210 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (Big_int.big_int)
# 4217 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv265) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 8 "parser.mly"
       (Big_int.big_int)
# 4227 "parser.ml"
    )) : (
# 8 "parser.mly"
       (Big_int.big_int)
# 4231 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 107 "parser.mly"
                                                    ( EpUint256 _1,                                                   ()  )
# 4236 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv261 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | BALANCE ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | EUINT256 _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | EUINT8 _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | FALSE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | ID _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | LPAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NEW ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NOW ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SENDER ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | THIS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TRUE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | VALUE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv262)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv263 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv259 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)

and _menhir_goto_evnt_arg : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_evnt_arg -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv257 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | BOOL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | BYTES32 ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | ID _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | UINT256 ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | UINT8 ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv252)
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_evnt_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_evnt_arg_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4345 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv254)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)) : 'freshtv258)

and _menhir_run13 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_ty -> (
# 7 "parser.mly"
       (string)
# 4359 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
    let ((_2 : (
# 7 "parser.mly"
       (string)
# 4368 "parser.ml"
    )) : (
# 7 "parser.mly"
       (string)
# 4372 "parser.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s, (_1 : 'tv_ty)) = _menhir_stack in
    let _v : 'tv_arg = 
# 62 "parser.mly"
                                                    ( TyVar(_2,_1)                                              )
# 4378 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv247) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_arg) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv237 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_arg)) = _menhir_stack in
        let _v : 'tv_evnt_arg = 
# 65 "parser.mly"
                                                    ( tyEvntArg_of_arg _1 false                                 )
# 4395 "parser.ml"
         in
        _menhir_goto_evnt_arg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv236)) : 'freshtv238)
    | MenhirState26 | MenhirState46 | MenhirState42 | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv240)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_arg)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_arg_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4433 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv242)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)) : 'freshtv246)
    | _ ->
        _menhir_fail ()) : 'freshtv248)) : 'freshtv250)

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | BOOL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | BYTES32 ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | ID _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_cntrct : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_cntrct -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv233 * _menhir_state * 'tv_cntrct) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONTRACT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | EVENT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | EOF ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190) : 'freshtv234)

and _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv207 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 4501 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv203 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 4511 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv199 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 4521 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | DEFAULT ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | METHOD ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | RBRACE ->
                    _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv200)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv201 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 4543 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)) : 'freshtv204)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv205 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 4554 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)) : 'freshtv208)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv219 * _menhir_state)) * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 4563 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv215 * _menhir_state)) * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 4573 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv211 * _menhir_state)) * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 4583 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv209 * _menhir_state)) * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 4590 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _), (_5 : (
# 7 "parser.mly"
       (string)
# 4595 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
                let _7 = () in
                let _3_inlined1 = () in
                let _1_inlined1 = () in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_mthd_head = let _6 =
                  let (_3, _1) = (_3_inlined1, _1_inlined1) in
                  let _1 =
                    let x = 
# 232 "<standard.mly>"
    ( xs )
# 4610 "parser.ml"
                     in
                    
# 200 "<standard.mly>"
    ( x )
# 4615 "parser.ml"
                    
                  in
                  
# 41 "parser.mly"
                                                    ( _1                                                        )
# 4621 "parser.ml"
                  
                in
                
# 59 "parser.mly"
                                                    ( TyMethod(_5,_6,TyTuple[])                                 )
# 4627 "parser.ml"
                 in
                _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv210)) : 'freshtv212)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv213 * _menhir_state)) * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 4637 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)) : 'freshtv216)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv217 * _menhir_state)) * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 4648 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)) : 'freshtv220)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv231 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 4657 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv227 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 4667 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv223 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 4677 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv221 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 4684 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (_3 : 'tv_ty)), (_4 : (
# 7 "parser.mly"
       (string)
# 4689 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
                let _6 = () in
                let _3_inlined1 = () in
                let _1_inlined1 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_mthd_head = let _5 =
                  let (_3, _1) = (_3_inlined1, _1_inlined1) in
                  let _1 =
                    let x = 
# 232 "<standard.mly>"
    ( xs )
# 4702 "parser.ml"
                     in
                    
# 200 "<standard.mly>"
    ( x )
# 4707 "parser.ml"
                    
                  in
                  
# 41 "parser.mly"
                                                    ( _1                                                        )
# 4713 "parser.ml"
                  
                in
                
# 58 "parser.mly"
                                                    ( TyMethod(_4,_5,_3)                                        )
# 4719 "parser.ml"
                 in
                _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv222)) : 'freshtv224)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv225 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 4729 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)) : 'freshtv228)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv229 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 4740 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)) : 'freshtv232)
    | _ ->
        _menhir_fail ()

and _menhir_reduce79 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 7 "parser.mly"
       (string)
# 4750 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 7 "parser.mly"
       (string)
# 4756 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_ty = 
# 75 "parser.mly"
                                                    ( TyInstnce _1                                              )
# 4761 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _v
        | INDEXED ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv159 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
                let (_v : (
# 7 "parser.mly"
       (string)
# 4791 "parser.ml"
                )) = _v in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv157 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
                let ((_3 : (
# 7 "parser.mly"
       (string)
# 4799 "parser.ml"
                )) : (
# 7 "parser.mly"
       (string)
# 4803 "parser.ml"
                )) = _v in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_ty)) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_evnt_arg = 
# 66 "parser.mly"
                                                    ( {arg=TyVar(_3,_1); indexed=true}                          )
# 4810 "parser.ml"
                 in
                _menhir_goto_evnt_arg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)) : 'freshtv160)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv161 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)) : 'freshtv164)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv165 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv173 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _ | INDEXED ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv169 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), _, (_3 : 'tv_ty)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_ty = 
# 74 "parser.mly"
                                                    ( TyMap(_1,_3)                                              )
# 4843 "parser.ml"
             in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv170)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv171 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)) : 'freshtv174)
    | MenhirState46 | MenhirState42 | MenhirState37 | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv175 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)) : 'freshtv178)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv187 * _menhir_state)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv183 * _menhir_state)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 7 "parser.mly"
       (string)
# 4884 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv179 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 4895 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | BOOL ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | BYTES32 ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | ID _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
                | UINT256 ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | UINT8 ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | RPAR ->
                    _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46) : 'freshtv180)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv181 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 4925 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)) : 'freshtv184)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv185 * _menhir_state)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)) : 'freshtv188)
    | MenhirState178 | MenhirState52 | MenhirState175 | MenhirState163 | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv193 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 7 "parser.mly"
       (string)
# 4950 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv189 * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 4961 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | BALANCE ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | EUINT256 _v ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | EUINT8 _v ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | FALSE ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | ID _v ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | LPAR ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | NEW ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | NOT ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | NOW ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | SENDER ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | THIS ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | TRUE ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | VALUE ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159) : 'freshtv190)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv191 * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 5005 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv195 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)) : 'freshtv198)
    | _ ->
        _menhir_fail ()

and _menhir_reduce76 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _1 = () in
    let _v : 'tv_ty = 
# 72 "parser.mly"
                                                    ( TyAddr                                                    )
# 5026 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_cntrct_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_cntrct_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv147 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv145 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_list_cntrct_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 37 "parser.mly"
       (unit Syntax.toplevel list)
# 5050 "parser.ml"
            ) = 
# 44 "parser.mly"
                                                    ( _1                                                        )
# 5054 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv143) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 37 "parser.mly"
       (unit Syntax.toplevel list)
# 5062 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv141) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 37 "parser.mly"
       (unit Syntax.toplevel list)
# 5070 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 37 "parser.mly"
       (unit Syntax.toplevel list)
# 5078 "parser.ml"
            )) : (
# 37 "parser.mly"
       (unit Syntax.toplevel list)
# 5082 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv140)) : 'freshtv142)) : 'freshtv144)) : 'freshtv146)) : 'freshtv148)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv149 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)) : 'freshtv152)
    | MenhirState190 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * _menhir_state * 'tv_cntrct) * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv153 * _menhir_state * 'tv_cntrct) * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_cntrct)), _, (xs : 'tv_list_cntrct_)) = _menhir_stack in
        let _v : 'tv_list_cntrct_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 5101 "parser.ml"
         in
        _menhir_goto_list_cntrct_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv154)) : 'freshtv156)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv137 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5114 "parser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv133 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5124 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv129 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5134 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv127 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5141 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (_2 : (
# 7 "parser.mly"
       (string)
# 5146 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = _menhir_stack in
            let _4 = () in
            let _3 = () in
            let _1_inlined1 = () in
            let _1 = () in
            let _v : 'tv_cntrct = let _3 =
              let _1 = _1_inlined1 in
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 5158 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 5163 "parser.ml"
                
              in
              
# 41 "parser.mly"
                                                    ( _1                                                        )
# 5169 "parser.ml"
              
            in
            
# 48 "parser.mly"
                                                    ( Event {                 id=_2;    tyEvArgs=_3}            )
# 5175 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv128)) : 'freshtv130)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv131 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5185 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)) : 'freshtv134)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv135 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5196 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)) : 'freshtv138)

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 142 "<standard.mly>"
    ( [] )
# 5206 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv125) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_ty = 
# 70 "parser.mly"
                                                    ( TyUint8                                                   )
# 5220 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv126)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_ty = 
# 69 "parser.mly"
                                                    ( TyUint256                                                 )
# 5234 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv124)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 5241 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_ty = 
# 71 "parser.mly"
                                                    ( TyBytes32                                                 )
# 5258 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_ty = 
# 73 "parser.mly"
                                                    ( TyBool                                                    )
# 5272 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv120)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState190 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_cntrct) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_mthd) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv27 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * 'tv_lexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState163 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv33 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv35 * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 5325 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv37 * _menhir_state) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv41 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5344 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv43 * _menhir_state) * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv47 * _menhir_state) * _menhir_state * 'tv_option_expr_))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv51 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5373 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv53 * _menhir_state * 'tv_expr))))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv67 * _menhir_state * 'tv_expr)) * (
# 7 "parser.mly"
       (string)
# 5417 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv69 * _menhir_state * 'tv_expr)) * (
# 7 "parser.mly"
       (string)
# 5426 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * (
# 7 "parser.mly"
       (string)
# 5470 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv89 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5484 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv99 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 7 "parser.mly"
       (string)
# 5513 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv103 * _menhir_state)) * _menhir_state)) * (
# 7 "parser.mly"
       (string)
# 5527 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv107 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5541 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv109 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5550 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv113 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv115 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5569 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv118)

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_cntrct_ = 
# 211 "<standard.mly>"
    ( [] )
# 5583 "parser.ml"
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
# 7 "parser.mly"
       (string)
# 5599 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5610 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
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
# 5634 "parser.ml"
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
# 7 "parser.mly"
       (string)
# 5648 "parser.ml"
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

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 7 "parser.mly"
       (string)
# 5672 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv3 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5683 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | RPAR ->
                _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv5 * _menhir_state) * (
# 7 "parser.mly"
       (string)
# 5713 "parser.ml"
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
# 37 "parser.mly"
       (unit Syntax.toplevel list)
# 5740 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONTRACT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EVENT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 5773 "parser.ml"
