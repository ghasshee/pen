
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
    | INDEXED
    | IN
    | IF
    | ID of (
# 11 "parser.mly"
       (string)
# 49 "parser.ml"
  )
    | GT
    | FIX
    | FALSE
    | EVENT
    | EUINT8 of (
# 13 "parser.mly"
       (Big_int.big_int)
# 58 "parser.ml"
  )
    | EUINT256 of (
# 12 "parser.mly"
       (Big_int.big_int)
# 63 "parser.ml"
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
    | COLON
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
  | MenhirState211
  | MenhirState204
  | MenhirState199
  | MenhirState196
  | MenhirState191
  | MenhirState187
  | MenhirState181
  | MenhirState178
  | MenhirState176
  | MenhirState173
  | MenhirState167
  | MenhirState163
  | MenhirState158
  | MenhirState156
  | MenhirState152
  | MenhirState148
  | MenhirState146
  | MenhirState144
  | MenhirState138
  | MenhirState132
  | MenhirState129
  | MenhirState127
  | MenhirState125
  | MenhirState123
  | MenhirState119
  | MenhirState117
  | MenhirState116
  | MenhirState115
  | MenhirState112
  | MenhirState110
  | MenhirState107
  | MenhirState105
  | MenhirState103
  | MenhirState99
  | MenhirState97
  | MenhirState91
  | MenhirState89
  | MenhirState88
  | MenhirState87
  | MenhirState85
  | MenhirState82
  | MenhirState80
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

# 173 "parser.ml"

let rec _menhir_run185 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv805 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
    ((let (_menhir_stack, _menhir_s, (_1 : 'tv_tm)) = _menhir_stack in
    let _v : 'tv_stmt = 
# 93 "parser.mly"
                                                    ( SmExpr (_1 [])                                            )
# 184 "parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv806)

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_stmt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv793 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv787 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | ADDRESS ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | BALANCE ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | BYTES32 ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | EUINT256 _v ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | EUINT8 _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | FALSE ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | FIX ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | ID _v ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | IF ->
                _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | LBRACE ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187) : 'freshtv788)
        | ABORT | ADDRESS | BALANCE | BOOL | BYTES32 | EUINT256 _ | EUINT8 _ | FALSE | FIX | ID _ | IF | LAM | LET | LOG | LPAR | NEW | NOT | NOW | RBRACE | RETURN | SELFDESTRUCT | SENDER | THIS | TRUE | UINT256 | UINT8 | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv789 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_1_inlined1 : 'tv_stmt)) = _menhir_stack in
            let _v : 'tv_stmt = let _4 =
              let _1 = _1_inlined1 in
              
# 85 "parser.mly"
                                                    ( [_1]                                                      )
# 271 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                                    ( SmIf(_2 [],_4,[])                                         )
# 277 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv790)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv791 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv792)) : 'freshtv794)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv797 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv795 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_1_inlined1 : 'tv_stmt)), _, (_1_inlined2 : 'tv_stmt)) = _menhir_stack in
        let _v : 'tv_stmt = let _6 =
          let _1 = _1_inlined2 in
          
# 85 "parser.mly"
                                                    ( [_1]                                                      )
# 298 "parser.ml"
          
        in
        let _4 =
          let _1 = _1_inlined1 in
          
# 85 "parser.mly"
                                                    ( [_1]                                                      )
# 306 "parser.ml"
          
        in
        
# 91 "parser.mly"
                                                    ( SmIf(_2 [],_4,_6)                                         )
# 312 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv796)) : 'freshtv798)
    | MenhirState196 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv801 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv799 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_1_inlined1 : 'tv_block)), _, (_1_inlined2 : 'tv_stmt)) = _menhir_stack in
        let _v : 'tv_stmt = let _6 =
          let _1 = _1_inlined2 in
          
# 85 "parser.mly"
                                                    ( [_1]                                                      )
# 326 "parser.ml"
          
        in
        let _4 =
          let _1 = _1_inlined1 in
          
# 86 "parser.mly"
                                                    ( _1                                                        )
# 334 "parser.ml"
          
        in
        
# 91 "parser.mly"
                                                    ( SmIf(_2 [],_4,_6)                                         )
# 340 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv800)) : 'freshtv802)
    | MenhirState199 | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv803 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ABORT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | ADDRESS ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | BALANCE ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | BOOL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | BYTES32 ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | EUINT256 _v ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
        | EUINT8 _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
        | FALSE ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | FIX ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | ID _v ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
        | IF ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | LAM ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | LET ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | LOG ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | NEW ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | NOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | NOW ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | RETURN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | SELFDESTRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | SENDER ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | THIS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | TRUE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | UINT256 ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | UINT8 ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | VALUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | RBRACE ->
            _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState199) : 'freshtv804)
    | _ ->
        _menhir_fail ()

and _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState163 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv777 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv775 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_args)) = _menhir_stack in
        let _v : 'tv_args = 
# 153 "parser.mly"
                                                    ( fun ctx -> _1 ctx :: _3 ctx                                           )
# 423 "parser.ml"
         in
        _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv776)) : 'freshtv778)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv785 * _menhir_state) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv781 * _menhir_state) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv779 * _menhir_state) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_args)) = _menhir_stack in
            let _v : 'tv_arg_list = 
# 150 "parser.mly"
                                                    ( fun ctx -> _2 ctx                                                     )
# 442 "parser.ml"
             in
            _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv780)) : 'freshtv782)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv783 * _menhir_state) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv784)) : 'freshtv786)
    | _ ->
        _menhir_fail ()

and _menhir_run146 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146

and _menhir_reduce69 : _menhir_env -> (('ttv_tail * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (_1 : 'tv_lexpr)), _, (_3 : 'tv_tm)) = _menhir_stack in
    let _v : 'tv_tm = 
# 116 "parser.mly"
                                                    ( fun ctx -> TmAssign(_1 ctx, _3 ctx)                               ,() )
# 515 "parser.ml"
     in
    _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_value_info : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_value_info -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv773) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_value_info) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv771) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_value_info) : 'tv_value_info) = _v in
    ((let _v : 'tv_msg = 
# 159 "parser.mly"
                                                    ( _1                                                                    )
# 532 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv769) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_msg) = _v in
    ((match _menhir_s with
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv759 * _menhir_state * 'tv_tm)) * (
# 11 "parser.mly"
       (string)
# 544 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv757 * _menhir_state * 'tv_tm)) * (
# 11 "parser.mly"
       (string)
# 552 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_5 : 'tv_msg) : 'tv_msg) = _v in
        ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), (_3 : (
# 11 "parser.mly"
       (string)
# 559 "parser.ml"
        ))), _, (_4 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_tm = 
# 120 "parser.mly"
                                                    ( fun ctx -> EpSend{cn=_1 ctx; mthd=Some _3;args=_4 ctx; msg=_5 ctx},() )
# 564 "parser.ml"
         in
        _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv758)) : 'freshtv760)
    | MenhirState138 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv763 * _menhir_state * 'tv_tm))))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv761 * _menhir_state * 'tv_tm))))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_6 : 'tv_msg) : 'tv_msg) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_tm)) = _menhir_stack in
        let _v : 'tv_tm = 
# 119 "parser.mly"
                                                    ( fun ctx -> EpSend{cn=_1 ctx; mthd=None   ;args=[]    ; msg=_6 ctx},() )
# 580 "parser.ml"
         in
        _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv762)) : 'freshtv764)
    | MenhirState167 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv767 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 588 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv765 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 596 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : 'tv_msg) : 'tv_msg) = _v in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 11 "parser.mly"
       (string)
# 603 "parser.ml"
        ))), _, (_3 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_aTm = 
# 146 "parser.mly"
                                       ( reserved _2; fun ctx -> EpNew {new_id=_2;new_args=_3 ctx; new_msg=_4 ctx}      ,() )
# 608 "parser.ml"
         in
        _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv766)) : 'freshtv768)
    | _ ->
        _menhir_fail ()) : 'freshtv770)) : 'freshtv772)) : 'freshtv774)

and _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState103
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

and _menhir_run110 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run105 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run112 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_run119 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119

and _menhir_run107 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState107
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

and _menhir_run125 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125

and _menhir_run127 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState127
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_run132 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132

and _menhir_run114 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEFAULT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv751 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv747 * _menhir_state * 'tv_tm))) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv743 * _menhir_state * 'tv_tm)))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | WITH ->
                    _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | ABORT | ADDRESS | BALANCE | COMMA | DOT | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
                    _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138) : 'freshtv744)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv745 * _menhir_state * 'tv_tm)))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv746)) : 'freshtv748)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv749 * _menhir_state * 'tv_tm))) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv750)) : 'freshtv752)
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv753 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 1151 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115) : 'freshtv754)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv755 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv756)

and _menhir_goto_list_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_stmt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState199 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv707 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv705 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_stmt)), _, (xs : 'tv_list_stmt_)) = _menhir_stack in
        let _v : 'tv_list_stmt_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1184 "parser.ml"
         in
        _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv706)) : 'freshtv708)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv741 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv737 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv735 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_list_stmt_)) = _menhir_stack in
            let _v : 'tv_block = 
# 60 "parser.mly"
                                                    ( _2                                                        )
# 1203 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv733) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_block) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState187 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv711 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv709 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_1_inlined1 : 'tv_stmt)), _, (_1_inlined2 : 'tv_block)) = _menhir_stack in
                let _v : 'tv_stmt = let _6 =
                  let _1 = _1_inlined2 in
                  
# 86 "parser.mly"
                                                    ( _1                                                        )
# 1222 "parser.ml"
                  
                in
                let _4 =
                  let _1 = _1_inlined1 in
                  
# 85 "parser.mly"
                                                    ( [_1]                                                      )
# 1230 "parser.ml"
                  
                in
                
# 91 "parser.mly"
                                                    ( SmIf(_2 [],_4,_6)                                         )
# 1236 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv710)) : 'freshtv712)
            | MenhirState178 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv719 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ELSE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv713 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ABORT ->
                        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | ADDRESS ->
                        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | BALANCE ->
                        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | BOOL ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | BYTES32 ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | EUINT256 _v ->
                        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
                    | EUINT8 _v ->
                        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
                    | FALSE ->
                        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | FIX ->
                        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | ID _v ->
                        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
                    | IF ->
                        _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | LAM ->
                        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | LBRACE ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | LET ->
                        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | LOG ->
                        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | LPAR ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | NEW ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | NOT ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | NOW ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | RETURN ->
                        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | SELFDESTRUCT ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | SENDER ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | THIS ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | TRUE ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | UINT256 ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | UINT8 ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | VALUE ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196) : 'freshtv714)
                | ABORT | ADDRESS | BALANCE | BOOL | BYTES32 | EUINT256 _ | EUINT8 _ | FALSE | FIX | ID _ | IF | LAM | LET | LOG | LPAR | NEW | NOT | NOW | RBRACE | RETURN | SELFDESTRUCT | SENDER | THIS | TRUE | UINT256 | UINT8 | VALUE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv715 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_1_inlined1 : 'tv_block)) = _menhir_stack in
                    let _v : 'tv_stmt = let _4 =
                      let _1 = _1_inlined1 in
                      
# 86 "parser.mly"
                                                    ( _1                                                        )
# 1318 "parser.ml"
                      
                    in
                    
# 92 "parser.mly"
                                                    ( SmIf(_2 [],_4,[])                                         )
# 1324 "parser.ml"
                     in
                    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv716)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv717 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv718)) : 'freshtv720)
            | MenhirState196 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv723 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv721 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_1_inlined1 : 'tv_block)), _, (_1_inlined2 : 'tv_block)) = _menhir_stack in
                let _v : 'tv_stmt = let _6 =
                  let _1 = _1_inlined2 in
                  
# 86 "parser.mly"
                                                    ( _1                                                        )
# 1345 "parser.ml"
                  
                in
                let _4 =
                  let _1 = _1_inlined1 in
                  
# 86 "parser.mly"
                                                    ( _1                                                        )
# 1353 "parser.ml"
                  
                in
                
# 91 "parser.mly"
                                                    ( SmIf(_2 [],_4,_6)                                         )
# 1359 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv722)) : 'freshtv724)
            | MenhirState50 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv731 * _menhir_state * 'tv_mthd_head) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv729 * _menhir_state * 'tv_mthd_head) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_mthd_head)), _, (_2 : 'tv_block)) = _menhir_stack in
                let _v : 'tv_mthd = 
# 57 "parser.mly"
                                                    ( TmMthd(_1,_2)                                             )
# 1371 "parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv727) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_mthd) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv725 * _menhir_state * 'tv_mthd) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | DEFAULT ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                | METHOD ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                | RBRACE ->
                    _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState204) : 'freshtv726)) : 'freshtv728)) : 'freshtv730)) : 'freshtv732)
            | _ ->
                _menhir_fail ()) : 'freshtv734)) : 'freshtv736)) : 'freshtv738)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv739 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv740)) : 'freshtv742)
    | _ ->
        _menhir_fail ()

and _menhir_reduce91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_value_info = 
# 162 "parser.mly"
                                                    ( fun ctx -> EpFalse,()                                                 )
# 1410 "parser.ml"
     in
    _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v

and _menhir_run117 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117

and _menhir_goto_tm : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv501 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv497 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv495 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_aTm = 
# 144 "parser.mly"
                                                    ( fun ctx -> EpAddr (_3 ctx)                                        ,() )
# 1509 "parser.ml"
             in
            _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv496)) : 'freshtv498)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv499 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv500)) : 'freshtv502)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv507 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | MINUS | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv503 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 105 "parser.mly"
                                                    ( fun l r -> EpPlus(l,r)                                    )
# 1538 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 1543 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv504)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv505 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv506)) : 'freshtv508)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv513 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv509 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 106 "parser.mly"
                                                    ( fun l r -> EpMult(l,r)                                    )
# 1570 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 1575 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv510)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv511 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv512)) : 'freshtv514)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv539 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | RSQBR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv535 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT | ADDRESS | BALANCE | COMMA | DOT | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv515 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
                let _v : 'tv_tm = 
# 121 "parser.mly"
                                                    ( fun ctx -> EpArray{aid=_1 ctx;aidx=_3 ctx}                        ,() )
# 1624 "parser.ml"
                 in
                _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv516)
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv531 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
                let _v : 'tv_lexpr = 
# 166 "parser.mly"
                                                    ( fun ctx -> EpArray{aid=_1 ctx; aidx=_3 ctx}                           )
# 1634 "parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv529) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_lexpr) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                match _menhir_s with
                | MenhirState176 | MenhirState191 | MenhirState181 | MenhirState62 | MenhirState63 | MenhirState173 | MenhirState71 | MenhirState163 | MenhirState73 | MenhirState156 | MenhirState158 | MenhirState82 | MenhirState152 | MenhirState87 | MenhirState88 | MenhirState144 | MenhirState146 | MenhirState97 | MenhirState99 | MenhirState103 | MenhirState105 | MenhirState107 | MenhirState110 | MenhirState112 | MenhirState117 | MenhirState123 | MenhirState125 | MenhirState132 | MenhirState127 | MenhirState119 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv521 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                    ((assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | EQ ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv517 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | ABORT ->
                            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                        | ADDRESS ->
                            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                        | BALANCE ->
                            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                        | EUINT256 _v ->
                            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                        | EUINT8 _v ->
                            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                        | FALSE ->
                            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                        | FIX ->
                            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                        | ID _v ->
                            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
                        | IF ->
                            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                        | LAM ->
                            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState123
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
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123) : 'freshtv518)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv519 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv520)) : 'freshtv522)
                | MenhirState51 | MenhirState199 | MenhirState196 | MenhirState178 | MenhirState187 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv527 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                    ((assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | EQ ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv523 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | ABORT ->
                            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | ADDRESS ->
                            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | BALANCE ->
                            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | EUINT256 _v ->
                            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
                        | EUINT8 _v ->
                            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
                        | FALSE ->
                            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | FIX ->
                            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | ID _v ->
                            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
                        | IF ->
                            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | LAM ->
                            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | LET ->
                            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | LOG ->
                            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | LPAR ->
                            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | NEW ->
                            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | NOT ->
                            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | NOW ->
                            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | RETURN ->
                            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | SELFDESTRUCT ->
                            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | SENDER ->
                            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | THIS ->
                            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | TRUE ->
                            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | VALUE ->
                            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState191
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191) : 'freshtv524)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv525 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv526)) : 'freshtv528)
                | _ ->
                    _menhir_fail ()) : 'freshtv530)) : 'freshtv532)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv533 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv534)) : 'freshtv536)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv537 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv538)) : 'freshtv540)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv545 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | NEQ | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv541 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 103 "parser.mly"
                                                    ( fun l r -> EpNEq(l,r)                                     )
# 1815 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 1820 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv542)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv543 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv544)) : 'freshtv546)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv551 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | MINUS | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv547 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 107 "parser.mly"
                                                    ( fun l r -> EpMinus(l,r)                                   )
# 1849 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 1854 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv548)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv549 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv550)) : 'freshtv552)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv557 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv553 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_value_info = 
# 163 "parser.mly"
                                                    ( _2                                                                    )
# 1897 "parser.ml"
             in
            _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v) : 'freshtv554)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv555 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv556)) : 'freshtv558)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv563 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | NEQ | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv559 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 100 "parser.mly"
                                                    ( fun l r -> EpLT(l,r)                                      )
# 1930 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 1935 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv560)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv561 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv562)) : 'freshtv564)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv567 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv565 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv566)) : 'freshtv568)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv573 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LAND | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv569 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 104 "parser.mly"
                                                    ( fun l r -> EpLAnd(l,r)                                    )
# 2011 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 2016 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv570)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv571 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv572)) : 'freshtv574)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv579 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | NEQ | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv575 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 101 "parser.mly"
                                                    ( fun l r -> EpGT(l,r)                                      )
# 2049 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 2054 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv576)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv577 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv578)) : 'freshtv580)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv585 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | NEQ | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv581 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 102 "parser.mly"
                                                    ( fun l r -> EpEq(l,r)                                      )
# 2087 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx -> _2 (_1 ctx)(_3 ctx)                                    ,() )
# 2092 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv582)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv583 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv584)) : 'freshtv586)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv593 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv589 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv587 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_aTm = 
# 141 "parser.mly"
                                                    ( fun ctx -> EpBalance (_3 ctx)                                     ,() )
# 2138 "parser.ml"
             in
            _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv588)) : 'freshtv590)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv591 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv592)) : 'freshtv594)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv599 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv595 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | ADDRESS ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | BALANCE ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | EUINT256 _v ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
            | EUINT8 _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
            | FALSE ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | FIX ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | ID _v ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
            | IF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144) : 'freshtv596)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv597 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv598)) : 'freshtv600)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv603 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv601 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv602)) : 'freshtv604)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv609 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv605 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_4 : 'tv_tm)), _, (_6 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 115 "parser.mly"
                                                    ( fun ctx -> TmIf(_2 ctx, _4 ctx, _6 ctx)                           ,() )
# 2303 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv606)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv607 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv608)) : 'freshtv610)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv615 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2318 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv611 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2348 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (_2 : (
# 11 "parser.mly"
       (string)
# 2353 "parser.ml"
            ))), _, (_4 : 'tv_ty)), _, (_6 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 114 "parser.mly"
                                                    ( fun ctx -> TmAbs(_2, _4, _6(add_bruijn_idx ctx _2))               ,() )
# 2358 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv612)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv613 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2368 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv614)) : 'freshtv616)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv621 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2377 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv617 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2393 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | ADDRESS ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | BALANCE ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | EUINT256 _v ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | EUINT8 _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | FALSE ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | FIX ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | ID _v ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | IF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152) : 'freshtv618)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv619 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2467 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv620)) : 'freshtv622)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv627 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2476 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv623 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2506 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), _), (_3 : (
# 11 "parser.mly"
       (string)
# 2511 "parser.ml"
            ))), _, (_5 : 'tv_ty)), _, (_7 : 'tv_tm)), _, (_9 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 112 "parser.mly"
                                                    ( fun ctx -> let ctx' = add_bruijn_idx ctx _3 in 
                                                                 TmApp((TmAbs(_3,_5,_9 ctx'),()),(TmFix(TmAbs(_3,_5,_7 ctx'),()),())), ())
# 2517 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv624)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv625 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 2527 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv626)) : 'freshtv628)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv633 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 2536 "parser.ml"
        ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv629 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 2552 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | ADDRESS ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | BALANCE ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | EUINT256 _v ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
            | EUINT8 _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
            | FALSE ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | FIX ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | ID _v ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
            | IF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158) : 'freshtv630)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv631 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 2626 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv632)) : 'freshtv634)
    | MenhirState158 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv639 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 2635 "parser.ml"
        ))) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv635 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 2665 "parser.ml"
            ))) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), _, (_2 : 'tv_ty)), (_3 : (
# 11 "parser.mly"
       (string)
# 2670 "parser.ml"
            ))), _, (_5 : 'tv_tm)), _, (_7 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 111 "parser.mly"
                                                    ( fun ctx -> TmApp((TmAbs(_3,_2,_7(add_bruijn_idx ctx _3)),()),_5 ctx)   ,() )
# 2675 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv636)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv637 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 2685 "parser.ml"
            ))) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv638)) : 'freshtv640)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv647 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv643 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv641 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_aTm = 
# 132 "parser.mly"
                                                    ( fun ctx -> EpParen (_2 ctx)                                       ,() )
# 2725 "parser.ml"
             in
            _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv642)) : 'freshtv644)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv645 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv646)) : 'freshtv648)
    | MenhirState163 | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv655 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv649 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | ADDRESS ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | BALANCE ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | EUINT256 _v ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | EUINT8 _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | FALSE ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | FIX ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | ID _v ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | IF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163) : 'freshtv650)
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv651 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_args = 
# 152 "parser.mly"
                                                    ( fun ctx -> [_1 ctx]                                                   )
# 2822 "parser.ml"
             in
            _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv652)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv653 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv654)) : 'freshtv656)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv661 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv657 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_ret = 
# 97 "parser.mly"
                                                    ( _1                                                        )
# 2865 "parser.ml"
             in
            _menhir_goto_ret _menhir_env _menhir_stack _menhir_s _v) : 'freshtv658)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv659 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv660)) : 'freshtv662)
    | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv667 * _menhir_state) * _menhir_state * 'tv_ret))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv663 * _menhir_state) * _menhir_state * 'tv_ret))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_ret)), _, (_5 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_aTm = 
# 133 "parser.mly"
                                                    ( fun ctx -> TmReturn(_2 ctx,_5 ctx)                                ,() )
# 2908 "parser.ml"
             in
            _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv664)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv665 * _menhir_state) * _menhir_state * 'tv_ret))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv666)) : 'freshtv668)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv673 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EUINT256 _ | EUINT8 _ | FALSE | ID _ | IN | LPAR | NEW | NOW | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv669 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 118 "parser.mly"
                                                    ( fun ctx -> TmSlfDstrct(_2 ctx)                                    ,() )
# 2951 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv670)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv671 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv672)) : 'freshtv674)
    | MenhirState176 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv679 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv675 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | ADDRESS ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | BALANCE ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | BYTES32 ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | EUINT256 _v ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
            | EUINT8 _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
            | FALSE ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | FIX ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | ID _v ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
            | IF ->
                _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | LBRACE ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178) : 'freshtv676)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv677 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv678)) : 'freshtv680)
    | MenhirState181 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv687 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 3063 "parser.ml"
        ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv683 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 3093 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv681 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 3100 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), (_2 : (
# 11 "parser.mly"
       (string)
# 3105 "parser.ml"
            ))), _, (_4 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 90 "parser.mly"
                                       ( reserved _2; SmDecl(_1,_2,_4 [])                                       )
# 3110 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv682)) : 'freshtv684)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv685 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 3120 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv686)) : 'freshtv688)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv691 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            _menhir_run185 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv689 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv690)) : 'freshtv692)
    | MenhirState199 | MenhirState51 | MenhirState196 | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv695 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            _menhir_run185 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv693 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv694)) : 'freshtv696)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv703 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv699 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv697 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_lexpr)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 89 "parser.mly"
                                                    ( SmAssign(_1 [],_3 [])                                     )
# 3232 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv698)) : 'freshtv700)
        | ELSE ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv701 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv702)) : 'freshtv704)
    | _ ->
        _menhir_fail ()

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_stmt_ = 
# 211 "<standard.mly>"
    ( [] )
# 3252 "parser.ml"
     in
    _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run176 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176

and _menhir_goto_arg_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_arg_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv485 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3320 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv483 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3326 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 11 "parser.mly"
       (string)
# 3331 "parser.ml"
        ))), _, (_3 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_tm = 
# 117 "parser.mly"
                                                    ( fun ctx -> TmLog(_2,_3 ctx,None)                                  ,() )
# 3336 "parser.ml"
         in
        _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv484)) : 'freshtv486)
    | MenhirState148 | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv489 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 3344 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv487 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 3350 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 11 "parser.mly"
       (string)
# 3355 "parser.ml"
        ))), _, (_2 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_appTm = 
# 127 "parser.mly"
                                                    ( fun ctx -> EpCall{call_id=_1;call_args=_2 ctx}                    ,() )
# 3360 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv488)) : 'freshtv490)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv491 * _menhir_state * 'tv_tm)) * (
# 11 "parser.mly"
       (string)
# 3368 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | WITH ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | ABORT | ADDRESS | BALANCE | COMMA | DOT | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv492)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv493 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3386 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | WITH ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | ABORT | ADDRESS | BALANCE | COMMA | DOT | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
            _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState167) : 'freshtv494)
    | _ ->
        _menhir_fail ()

and _menhir_goto_appTm : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_appTm -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv481 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | COMMA | DOT | ELSE | EQEQ | GT | IN | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_appTm)) = _menhir_stack in
        let _v : 'tv_tm = 
# 110 "parser.mly"
                                                    ( _1                                                                    )
# 3447 "parser.ml"
         in
        _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv480)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv482)

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | ADDRESS ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BOOL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BYTES32 ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | ID _v ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState51
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
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_goto_list_mthd_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_mthd_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState204 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv469 * _menhir_state * 'tv_mthd) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv467 * _menhir_state * 'tv_mthd) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_mthd)), _, (xs : 'tv_list_mthd_)) = _menhir_stack in
        let _v : 'tv_list_mthd_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 3533 "parser.ml"
         in
        _menhir_goto_list_mthd_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv468)) : 'freshtv470)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv477 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3541 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv473 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3551 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv471 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3558 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (_2 : (
# 11 "parser.mly"
       (string)
# 3563 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)), _, (_5 : 'tv_list_mthd_)) = _menhir_stack in
            let _v : 'tv_cntrct = let _3 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 3570 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 3575 "parser.ml"
                
              in
              
# 47 "parser.mly"
                                                    ( _1                                                        )
# 3581 "parser.ml"
              
            in
            
# 53 "parser.mly"
                                                    ( reserved _2; Cntrct{id=_2; mthds=_5; fields=_3}           )
# 3587 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv472)) : 'freshtv474)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv475 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 3597 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv476)) : 'freshtv478)
    | _ ->
        _menhir_fail ()

and _menhir_goto_ret : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ret -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv465 * _menhir_state) * _menhir_state * 'tv_ret) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv461 * _menhir_state) * _menhir_state * 'tv_ret) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BECOME ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv457 * _menhir_state) * _menhir_state * 'tv_ret)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | ADDRESS ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | BALANCE ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | EUINT256 _v ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
            | EUINT8 _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
            | FALSE ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | FIX ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | ID _v ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
            | IF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173) : 'freshtv458)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv459 * _menhir_state) * _menhir_state * 'tv_ret)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv460)) : 'freshtv462)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv463 * _menhir_state) * _menhir_state * 'tv_ret) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv464)) : 'freshtv466)

and _menhir_reduce14 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 11 "parser.mly"
       (string)
# 3690 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 11 "parser.mly"
       (string)
# 3696 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_aTm = 
# 145 "parser.mly"
                                       ( reserved _1; fun ctx -> prBds ctx;pe _1;(try TmIdx(lookup_bruijn_idx _1 ctx,len ctx),() with _ -> TmId _1,()))
# 3701 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState71
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
        let (_menhir_stack : 'freshtv455 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState71 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv453 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_arg_list = 
# 149 "parser.mly"
                                                    ( fun ctx -> []                                                         )
# 3757 "parser.ml"
         in
        _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv454)) : 'freshtv456)
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

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 3778 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run99 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | LET ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | LOG ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | SELFDESTRUCT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_goto_aTm : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_aTm -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv451) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_aTm) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv449) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_aTm) : 'tv_aTm) = _v in
    ((let _v : 'tv_pathTm = 
# 130 "parser.mly"
                                                    ( _1                                                                    )
# 3852 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv447) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_pathTm) = _v in
    ((match _menhir_s with
    | MenhirState51 | MenhirState199 | MenhirState176 | MenhirState196 | MenhirState178 | MenhirState191 | MenhirState187 | MenhirState181 | MenhirState62 | MenhirState63 | MenhirState173 | MenhirState71 | MenhirState163 | MenhirState73 | MenhirState156 | MenhirState158 | MenhirState82 | MenhirState152 | MenhirState87 | MenhirState88 | MenhirState144 | MenhirState146 | MenhirState97 | MenhirState99 | MenhirState103 | MenhirState105 | MenhirState107 | MenhirState110 | MenhirState112 | MenhirState117 | MenhirState123 | MenhirState125 | MenhirState132 | MenhirState127 | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv433) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_pathTm) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv431) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_pathTm) : 'tv_pathTm) = _v in
        ((let _v : 'tv_appTm = 
# 124 "parser.mly"
                                                    ( _1                                                                    )
# 3871 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv432)) : 'freshtv434)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv437 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_pathTm) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv435 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_pathTm) : 'tv_pathTm) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_appTm)) = _menhir_stack in
        let _v : 'tv_appTm = 
# 128 "parser.mly"
                                                    ( fun ctx -> let e=_1 ctx in TmApp(e,_2 ctx)                                   ,() )
# 3887 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv436)) : 'freshtv438)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv441 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_pathTm) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv439 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_pathTm) : 'tv_pathTm) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_appTm = 
# 125 "parser.mly"
                                                    ( fun ctx -> TmFix(_2 ctx)                                          ,() )
# 3903 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv440)) : 'freshtv442)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv445 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_pathTm) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv443 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_pathTm) : 'tv_pathTm) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_appTm = 
# 126 "parser.mly"
                                                    ( fun ctx -> EpNot (_2 ctx)                                         ,() )
# 3919 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv444)) : 'freshtv446)
    | _ ->
        _menhir_fail ()) : 'freshtv448)) : 'freshtv450)) : 'freshtv452)

and _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_evnt_arg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv425) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv423) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_evnt_arg_) : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ = 
# 144 "<standard.mly>"
    ( x )
# 3940 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv424)) : 'freshtv426)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv429 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv427 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_evnt_arg_) : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_evnt_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_evnt_arg_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3956 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv428)) : 'freshtv430)
    | _ ->
        _menhir_fail ()

and _menhir_goto_mthd_head : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_mthd_head -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv421 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv422)

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_mthd_ = 
# 211 "<standard.mly>"
    ( [] )
# 3982 "parser.ml"
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
        let (_menhir_stack : 'freshtv419 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState34 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv415 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv411 * _menhir_state) * _menhir_state)) = Obj.magic _menhir_stack in
                let (_v : (
# 11 "parser.mly"
       (string)
# 4020 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv407 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 4031 "parser.ml"
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
                        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv408)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv409 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 4061 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv410)) : 'freshtv412)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv413 * _menhir_state) * _menhir_state)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv414)) : 'freshtv416)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv417 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv418)) : 'freshtv420)
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
    let (_menhir_stack : 'freshtv405) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_mthd_head = 
# 63 "parser.mly"
                                                    ( TyDefault                                                 )
# 4097 "parser.ml"
     in
    _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv406)

and _menhir_run148 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 4104 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState148
    | ARROW | DARROW | ID _ ->
        _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack)
    | ABORT | ADDRESS | BALANCE | COMMA | DOT | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | IN | LAND | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148

and _menhir_run149 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
    | ARROW | DARROW | ID _ ->
        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv403 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv404)

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv399 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MSG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv395 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv391 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv389 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_aTm = 
# 139 "parser.mly"
                                                    ( fun ctx -> EpValue                                                ,() )
# 4168 "parser.ml"
                 in
                _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv390)) : 'freshtv392)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv393 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv394)) : 'freshtv396)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv397 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv398)) : 'freshtv400)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv401 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv402)

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv387) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_aTm = 
# 135 "parser.mly"
                                                    ( fun ctx -> EpTrue                                                 ,() )
# 4202 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv388)

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv385) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_aTm = 
# 143 "parser.mly"
                                                    ( fun ctx -> EpThis                                                 ,() )
# 4215 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv386)

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv381 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MSG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv377 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv373 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv371 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_aTm = 
# 140 "parser.mly"
                                                    ( fun ctx -> EpSender                                               ,() )
# 4247 "parser.ml"
                 in
                _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv372)) : 'freshtv374)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv375 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)) : 'freshtv378)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv379 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)) : 'freshtv382)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv383 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv384)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState62
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
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState63
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
        let (_menhir_stack : 'freshtv369) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState63 in
        ((let _v : 'tv_ret = 
# 96 "parser.mly"
                                                    ( fun ctx -> TmUnit                                  ,()    )
# 4384 "parser.ml"
         in
        _menhir_goto_ret _menhir_env _menhir_stack _menhir_s _v) : 'freshtv370)
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
        let (_menhir_stack : 'freshtv365 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BLOCK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv361 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv357 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv355 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_aTm = 
# 142 "parser.mly"
                                                    ( fun ctx -> EpNow                                                  ,() )
# 4420 "parser.ml"
                 in
                _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv356)) : 'freshtv358)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv359 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv360)) : 'freshtv362)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv363 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv364)) : 'freshtv366)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv367 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
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
        let (_menhir_stack : 'freshtv351 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 4498 "parser.ml"
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv352)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv354)

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState73
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
        let (_menhir_stack : 'freshtv347 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 4585 "parser.ml"
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv348)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)

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
        let (_menhir_stack : 'freshtv345 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState77 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv341 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 4633 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv337 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 4644 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | BOOL ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | BYTES32 ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | ID _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
                | UINT256 ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | UINT8 ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv338)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv339 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 4672 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)) : 'freshtv342)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv343 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv344)) : 'freshtv346)
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv333 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 4704 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv329 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 4715 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | BYTES32 ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv330)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv331 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 4743 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)) : 'freshtv334)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv335 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | FIX ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | ID _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | IF ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState88
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
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run89 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 4813 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | ABORT | ADDRESS | BALANCE | COMMA | DOT | ELSE | EQEQ | EUINT256 _ | EUINT8 _ | FALSE | GT | ID _ | IN | LAND | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | PLUS | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | VALUE ->
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
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
    | ABORT ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | ADDRESS ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | BALANCE ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | EUINT256 _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | EUINT8 _v ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | FALSE ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | LPAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | RETURN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | SENDER ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | TRUE ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run93 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv327) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_aTm = 
# 136 "parser.mly"
                                                    ( fun ctx -> EpFalse                                                ,() )
# 4879 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)

and _menhir_run94 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "parser.mly"
       (Big_int.big_int)
# 4886 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv325) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 13 "parser.mly"
       (Big_int.big_int)
# 4896 "parser.ml"
    )) : (
# 13 "parser.mly"
       (Big_int.big_int)
# 4900 "parser.ml"
    )) = _v in
    ((let _v : 'tv_aTm = 
# 138 "parser.mly"
                                                    ( fun ctx -> EpUint256 _1                                           ,() )
# 4905 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (Big_int.big_int)
# 4912 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv323) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 12 "parser.mly"
       (Big_int.big_int)
# 4922 "parser.ml"
    )) : (
# 12 "parser.mly"
       (Big_int.big_int)
# 4926 "parser.ml"
    )) = _v in
    ((let _v : 'tv_aTm = 
# 137 "parser.mly"
                                                    ( fun ctx -> EpUint256 _1                                           ,() )
# 4931 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)

and _menhir_run96 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv319 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ABORT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | ADDRESS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | BALANCE ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | EUINT256 _v ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | EUINT8 _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | FALSE ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | FIX ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | ID _v ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | IF ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LAM ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LET ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LOG ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NEW ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NOW ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | RETURN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | SELFDESTRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | SENDER ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | THIS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | TRUE ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | VALUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv320)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv322)

and _menhir_run98 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv317 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv318)

and _menhir_run100 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv315) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_aTm = 
# 134 "parser.mly"
                                                    ( fun ctx -> TmAbort                                                ,() )
# 5028 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv316)

and _menhir_goto_separated_nonempty_list_COMMA_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_arg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState46 | MenhirState38 | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv309) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_arg_) : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 144 "<standard.mly>"
    ( x )
# 5047 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)) : 'freshtv310)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv313 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv311 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_arg_) : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_arg_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 5063 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)) : 'freshtv314)
    | _ ->
        _menhir_fail ()

and _menhir_goto_evnt_arg : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_evnt_arg -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv305 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv299 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv300)
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv301 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_evnt_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_evnt_arg_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 5108 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv303 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv304)) : 'freshtv306)

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
    let (_menhir_stack : 'freshtv297 * _menhir_state * 'tv_cntrct) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONTRACT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState211
    | EVENT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState211
    | EOF ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState211
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState211) : 'freshtv298)

and _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv279 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5196 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv275 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5206 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv271 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5216 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | DEFAULT ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | METHOD ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | RBRACE ->
                    _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv272)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv273 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5238 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv277 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5249 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)) : 'freshtv280)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv287 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 5258 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv283 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 5268 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv281 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 5275 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _), (_4 : (
# 11 "parser.mly"
       (string)
# 5280 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
            let _v : 'tv_mthd_head = let _5 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 5287 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 5292 "parser.ml"
                
              in
              
# 47 "parser.mly"
                                                    ( _1                                                        )
# 5298 "parser.ml"
              
            in
            
# 65 "parser.mly"
                                                    ( TyMthd(_4,_5,TyTuple[])                                   )
# 5304 "parser.ml"
             in
            _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv282)) : 'freshtv284)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv285 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 5314 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv286)) : 'freshtv288)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv295 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5323 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv291 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5333 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv289 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5340 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_ty)), (_3 : (
# 11 "parser.mly"
       (string)
# 5345 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
            let _v : 'tv_mthd_head = let _4 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 5352 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 5357 "parser.ml"
                
              in
              
# 47 "parser.mly"
                                                    ( _1                                                        )
# 5363 "parser.ml"
              
            in
            
# 64 "parser.mly"
                                                    ( TyMthd(_3,_4,_2)                                          )
# 5369 "parser.ml"
             in
            _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)) : 'freshtv292)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv293 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5379 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv294)) : 'freshtv296)
    | _ ->
        _menhir_fail ()

and _menhir_reduce90 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 11 "parser.mly"
       (string)
# 5389 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 11 "parser.mly"
       (string)
# 5395 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_ty = 
# 82 "parser.mly"
                                                    ( TyInstnce _1                                              )
# 5400 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv189 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 5424 "parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv187 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let ((_3 : (
# 11 "parser.mly"
       (string)
# 5432 "parser.ml"
            )) : (
# 11 "parser.mly"
       (string)
# 5436 "parser.ml"
            )) = _v in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_evnt_arg = 
# 72 "parser.mly"
                                                    ( TyEvVar(_3,_2,true)                                       )
# 5442 "parser.ml"
             in
            _menhir_goto_evnt_arg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv188)) : 'freshtv190)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv191 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv199 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | EQ | ID _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv195 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), _, (_3 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_ty = 
# 80 "parser.mly"
                                                    ( TyMap(_1,_3)                                              )
# 5469 "parser.ml"
             in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv197 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)) : 'freshtv200)
    | MenhirState87 | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv205 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | EQ | ID _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv201 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), _, (_3 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_ty = 
# 81 "parser.mly"
                                                    ( TyAbs(_1,_3)                                              )
# 5496 "parser.ml"
             in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv202)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv203 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)
    | MenhirState46 | MenhirState42 | MenhirState38 | MenhirState29 | MenhirState24 | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv227 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv223 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 5522 "parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv221 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let ((_2 : (
# 11 "parser.mly"
       (string)
# 5530 "parser.ml"
            )) : (
# 11 "parser.mly"
       (string)
# 5534 "parser.ml"
            )) = _v in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_arg = 
# 68 "parser.mly"
                                                    ( reserved _2; TyVar(_2,_1)                                 )
# 5540 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv219) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_arg) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState3 | MenhirState24 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv207 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_arg)) = _menhir_stack in
                let _v : 'tv_evnt_arg = 
# 71 "parser.mly"
                                                    ( let TyVar(id,ty)=_1 in TyEvVar(id,ty,false)               )
# 5557 "parser.ml"
                 in
                _menhir_goto_evnt_arg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)
            | MenhirState29 | MenhirState46 | MenhirState42 | MenhirState38 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv217 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | COMMA ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv211 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
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
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv212)
                | RPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv213 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, (x : 'tv_arg)) = _menhir_stack in
                    let _v : 'tv_separated_nonempty_list_COMMA_arg_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 5595 "parser.ml"
                     in
                    _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv214)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv215 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)) : 'freshtv218)
            | _ ->
                _menhir_fail ()) : 'freshtv220)) : 'freshtv222)) : 'freshtv224)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv225 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)) : 'freshtv228)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv237 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv233 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 5630 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv229 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5641 "parser.ml"
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
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46) : 'freshtv230)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv231 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5671 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv232)) : 'freshtv234)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv235 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv236)) : 'freshtv238)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv243 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5687 "parser.ml"
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
            let (_menhir_stack : (((('freshtv239 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5701 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | ADDRESS ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | BALANCE ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | EUINT256 _v ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | EUINT8 _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | FALSE ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | FIX ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | ID _v ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | IF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv240)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv241 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5761 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)) : 'freshtv244)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv249 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5770 "parser.ml"
        ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv245 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5780 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | ADDRESS ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | BALANCE ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | BYTES32 ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | EUINT256 _v ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | EUINT8 _v ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | FALSE ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | FIX ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | ID _v ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | IF ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LET ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LOG ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LPAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | RETURN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | SELFDESTRUCT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | SENDER ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | TRUE ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv246)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv247 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 5850 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv259 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv255 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 5870 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv251 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5881 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ABORT ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | ADDRESS ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | BALANCE ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | EUINT256 _v ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
                | EUINT8 _v ->
                    _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
                | FALSE ->
                    _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | FIX ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | ID _v ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
                | IF ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | LAM ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | LET ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | LOG ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | LPAR ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | NEW ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | NOT ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | NOW ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | RETURN ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | SELFDESTRUCT ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | SENDER ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | THIS ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | TRUE ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | VALUE ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156) : 'freshtv252)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv253 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5941 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv257 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)) : 'freshtv260)
    | MenhirState199 | MenhirState51 | MenhirState196 | MenhirState187 | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv269 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv265 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 5968 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv261 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 5979 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ABORT ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | ADDRESS ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | BALANCE ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | EUINT256 _v ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
                | EUINT8 _v ->
                    _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
                | FALSE ->
                    _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | FIX ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | ID _v ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
                | IF ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | LAM ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | LET ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | LOG ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | LPAR ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | NEW ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | NOT ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | NOW ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | RETURN ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | SELFDESTRUCT ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | SENDER ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | THIS ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | TRUE ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | VALUE ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181) : 'freshtv262)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv263 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6039 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)) : 'freshtv266)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv267 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)) : 'freshtv270)
    | _ ->
        _menhir_fail ()

and _menhir_reduce86 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : 'tv_ty = 
# 78 "parser.mly"
                                                    ( TyAddr                                                    )
# 6059 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_cntrct_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_cntrct_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv175 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_list_cntrct_)) = _menhir_stack in
            let _v : (
# 43 "parser.mly"
       (unit Syntax.toplevel list)
# 6082 "parser.ml"
            ) = 
# 50 "parser.mly"
                                                    ( _1                                                        )
# 6086 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv173) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 43 "parser.mly"
       (unit Syntax.toplevel list)
# 6094 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv171) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 43 "parser.mly"
       (unit Syntax.toplevel list)
# 6102 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv169) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 43 "parser.mly"
       (unit Syntax.toplevel list)
# 6110 "parser.ml"
            )) : (
# 43 "parser.mly"
       (unit Syntax.toplevel list)
# 6114 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv170)) : 'freshtv172)) : 'freshtv174)) : 'freshtv176)) : 'freshtv178)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv179 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)) : 'freshtv182)
    | MenhirState211 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * _menhir_state * 'tv_cntrct) * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183 * _menhir_state * 'tv_cntrct) * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_cntrct)), _, (xs : 'tv_list_cntrct_)) = _menhir_stack in
        let _v : 'tv_list_cntrct_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 6133 "parser.ml"
         in
        _menhir_goto_list_cntrct_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)) : 'freshtv186)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv167 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6146 "parser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv163 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6156 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv159 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6166 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv157 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6173 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (_2 : (
# 11 "parser.mly"
       (string)
# 6178 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = _menhir_stack in
            let _v : 'tv_cntrct = let _3 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 6185 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 6190 "parser.ml"
                
              in
              
# 47 "parser.mly"
                                                    ( _1                                                        )
# 6196 "parser.ml"
              
            in
            
# 54 "parser.mly"
                                                    ( Event (TyEv(_2,_3))                                       )
# 6202 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)) : 'freshtv160)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv161 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6212 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)) : 'freshtv164)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv165 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6223 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)

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

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 142 "<standard.mly>"
    ( [] )
# 6256 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv155) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 76 "parser.mly"
                                                    ( TyUint8                                                   )
# 6269 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv153) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 75 "parser.mly"
                                                    ( TyUint256                                                 )
# 6282 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv154)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 6289 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv151) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 77 "parser.mly"
                                                    ( TyBytes32                                                 )
# 6305 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv152)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv149) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 79 "parser.mly"
                                                    ( TyBool                                                    )
# 6318 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState211 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_cntrct) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState204 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_mthd) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState199 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState196 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv27 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_block)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_lexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv31 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_stmt)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState181 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv33 * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6366 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv35 * _menhir_state) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState176 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv39 * _menhir_state) * _menhir_state * 'tv_ret))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState167 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv41 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6390 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState163 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState158 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv45 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6404 "parser.ml"
        ))) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv47 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6413 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv49 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6422 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 6431 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv53 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv55 * _menhir_state) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState138 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv57 * _menhir_state * 'tv_tm))))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_lexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv73 * _menhir_state * 'tv_tm)) * (
# 11 "parser.mly"
       (string)
# 6490 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv75 * _menhir_state * 'tv_tm)) * (
# 11 "parser.mly"
       (string)
# 6499 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 6548 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv97 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6562 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv99 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6571 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv101 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6580 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv103 * _menhir_state) * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6589 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6603 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv113 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6622 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv125 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 11 "parser.mly"
       (string)
# 6656 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv129 * _menhir_state) * _menhir_state)) * (
# 11 "parser.mly"
       (string)
# 6670 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv133 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6684 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv135 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6693 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv137 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv141 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv145 * _menhir_state) * (
# 11 "parser.mly"
       (string)
# 6722 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv148)

and _menhir_reduce33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_cntrct_ = 
# 211 "<standard.mly>"
    ( [] )
# 6736 "parser.ml"
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
# 6752 "parser.ml"
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
# 6763 "parser.ml"
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
# 6789 "parser.ml"
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
# 6803 "parser.ml"
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
# 6827 "parser.ml"
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
# 6838 "parser.ml"
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
                _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState29
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
# 6868 "parser.ml"
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
# 43 "parser.mly"
       (unit Syntax.toplevel list)
# 6895 "parser.ml"
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
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 6923 "parser.ml"
