
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | WITH
    | VALUE
    | UNIT
    | UINT8
    | UINT256
    | U8 of (
# 13 "parser.mly"
       (Big_int.big_int)
# 19 "parser.ml"
  )
    | TRUE
    | THIS
    | THEN
    | SENDER
    | SEMI
    | SELFDESTRUCT
    | RSQBR
    | RPAR
    | RIG of (
# 11 "parser.mly"
       (int)
# 32 "parser.ml"
  )
    | RETURN
    | REF
    | REC
    | RBRACE
    | PLUS
    | NUM of (
# 13 "parser.mly"
       (Big_int.big_int)
# 42 "parser.ml"
  )
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
# 12 "parser.mly"
       (string)
# 69 "parser.ml"
  )
    | GT
    | FIX
    | FALSE
    | EVENT
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
    | COLONEQ
    | COLON
    | CALL
    | BYTES32
    | BOOL
    | BLOCK
    | BECOME
    | BANG
    | BALANCE
    | ARROW
    | ADDRESS
    | ABORT
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState205
  | MenhirState198
  | MenhirState192
  | MenhirState191
  | MenhirState186
  | MenhirState183
  | MenhirState179
  | MenhirState174
  | MenhirState172
  | MenhirState170
  | MenhirState166
  | MenhirState164
  | MenhirState162
  | MenhirState160
  | MenhirState155
  | MenhirState153
  | MenhirState147
  | MenhirState145
  | MenhirState143
  | MenhirState138
  | MenhirState132
  | MenhirState129
  | MenhirState126
  | MenhirState124
  | MenhirState122
  | MenhirState120
  | MenhirState118
  | MenhirState117
  | MenhirState116
  | MenhirState113
  | MenhirState111
  | MenhirState108
  | MenhirState106
  | MenhirState104
  | MenhirState100
  | MenhirState98
  | MenhirState95
  | MenhirState92
  | MenhirState90
  | MenhirState88
  | MenhirState87
  | MenhirState85
  | MenhirState82
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState74
  | MenhirState73
  | MenhirState71
  | MenhirState65
  | MenhirState64
  | MenhirState51
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

# 184 "parser.ml"

let rec _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv779 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv777 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_args)) = _menhir_stack in
        let _v : 'tv_args = 
# 160 "parser.mly"
                                                    ( fun ctx ->    _1 ctx :: _3 ctx                                                            )
# 199 "parser.ml"
         in
        _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv778)) : 'freshtv780)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv787 * _menhir_state) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv783 * _menhir_state) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv781 * _menhir_state) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_args)) = _menhir_stack in
            let _v : 'tv_arg_list = 
# 157 "parser.mly"
                                                    ( fun ctx ->    _2 ctx                                                                      )
# 218 "parser.ml"
             in
            _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv782)) : 'freshtv784)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv785 * _menhir_state) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv786)) : 'freshtv788)
    | _ ->
        _menhir_fail ()

and _menhir_goto_value_info : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_value_info -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv775) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_value_info) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv773) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_value_info) : 'tv_value_info) = _v in
    ((let _v : 'tv_msg = 
# 163 "parser.mly"
                                                    ( _1                                                                                        )
# 244 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv771) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_msg) = _v in
    ((match _menhir_s with
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv761 * _menhir_state * 'tv_tm)) * (
# 12 "parser.mly"
       (string)
# 256 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv759 * _menhir_state * 'tv_tm)) * (
# 12 "parser.mly"
       (string)
# 264 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_5 : 'tv_msg) : 'tv_msg) = _v in
        ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), (_3 : (
# 12 "parser.mly"
       (string)
# 271 "parser.ml"
        ))), _, (_4 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_tm = 
# 120 "parser.mly"
                                                    ( fun ctx ->    TmSend(_1 ctx,Some _3,_4 ctx,_5 ctx)                                    ,() )
# 276 "parser.ml"
         in
        _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv760)) : 'freshtv762)
    | MenhirState138 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv765 * _menhir_state * 'tv_tm))))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv763 * _menhir_state * 'tv_tm))))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_6 : 'tv_msg) : 'tv_msg) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_tm)) = _menhir_stack in
        let _v : 'tv_tm = 
# 119 "parser.mly"
                                                    ( fun ctx ->    TmSend(_1 ctx,None,[],_6 ctx)                                           ,() )
# 292 "parser.ml"
         in
        _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv764)) : 'freshtv766)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv769 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 300 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv767 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 308 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : 'tv_msg) : 'tv_msg) = _v in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 12 "parser.mly"
       (string)
# 315 "parser.ml"
        ))), _, (_3 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_aTm = 
# 152 "parser.mly"
                                       ( reserved _2; fun ctx ->    TmNew(_2,_3 ctx,_4 ctx)                                                 ,() )
# 320 "parser.ml"
         in
        _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv768)) : 'freshtv770)
    | _ ->
        _menhir_fail ()) : 'freshtv772)) : 'freshtv774)) : 'freshtv776)

and _menhir_run104 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_run111 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_run113 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113

and _menhir_run120 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120

and _menhir_run108 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run124 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

and _menhir_run126 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126

and _menhir_run132 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132

and _menhir_run115 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEFAULT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv753 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv749 * _menhir_state * 'tv_tm))) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv745 * _menhir_state * 'tv_tm)))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | WITH ->
                    _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | ABORT | ADDRESS | BALANCE | COLONEQ | COMMA | DOT | ELSE | EQEQ | FALSE | GT | ID _ | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | NUM _ | PLUS | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
                    _menhir_reduce87 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138) : 'freshtv746)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv747 * _menhir_state * 'tv_tm)))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv748)) : 'freshtv750)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv751 * _menhir_state * 'tv_tm))) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv752)) : 'freshtv754)
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv755 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        let (_v : (
# 12 "parser.mly"
       (string)
# 917 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv756)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv757 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv758)

and _menhir_run122 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122

and _menhir_reduce87 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_value_info = 
# 166 "parser.mly"
                                                    ( fun ctx ->    TmZero, ()                                                                  )
# 1002 "parser.ml"
     in
    _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v

and _menhir_run118 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118

and _menhir_goto_tm : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_tm -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv535 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv531 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv529 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_aTm = 
# 146 "parser.mly"
                                                    ( fun ctx ->    TmAddr (_3 ctx)                                                         ,() )
# 1109 "parser.ml"
             in
            _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv530)) : 'freshtv532)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv533 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv534)) : 'freshtv536)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv541 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | MINUS | NEQ | NEW | NOW | NUM _ | PLUS | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv537 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 94 "parser.mly"
                                                    ( fun l r ->    TmAdd(l,r)                                                                  )
# 1140 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx ->    _2 (_1 ctx)(_3 ctx)                                                     ,() )
# 1145 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv538)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv539 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv540)) : 'freshtv542)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv547 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | MINUS | MULT | NEQ | NEW | NOW | NUM _ | PLUS | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv543 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 95 "parser.mly"
                                                    ( fun l r ->    TmMul(l,r)                                                                  )
# 1174 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx ->    _2 (_1 ctx)(_3 ctx)                                                     ,() )
# 1179 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv544)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv545 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv546)) : 'freshtv548)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv555 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | RSQBR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv551 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv549 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 121 "parser.mly"
                                                    ( fun ctx ->    TmArr(_1 ctx,_3 ctx)                                                    ,() )
# 1227 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv550)) : 'freshtv552)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv553 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv554)) : 'freshtv556)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv561 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | NEQ | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv557 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 92 "parser.mly"
                                                    ( fun l r ->    TmNEQ(l,r)                                                                  )
# 1262 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx ->    _2 (_1 ctx)(_3 ctx)                                                     ,() )
# 1267 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv558)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv559 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv560)) : 'freshtv562)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv567 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | MINUS | NEQ | NEW | NOW | NUM _ | PLUS | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv563 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 96 "parser.mly"
                                                    ( fun l r ->    TmSub(l,r)                                                                  )
# 1298 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx ->    _2 (_1 ctx)(_3 ctx)                                                     ,() )
# 1303 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv564)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv565 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv566)) : 'freshtv568)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv573 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv569 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_value_info = 
# 167 "parser.mly"
                                                    ( _2                                                                                        )
# 1348 "parser.ml"
             in
            _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v) : 'freshtv570)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv571 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv572)) : 'freshtv574)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv579 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | NEQ | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv575 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 89 "parser.mly"
                                                    ( fun l r ->    TmLT(l,r)                                                                   )
# 1383 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx ->    _2 (_1 ctx)(_3 ctx)                                                     ,() )
# 1388 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv576)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv577 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv578)) : 'freshtv580)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv585 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv581 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 116 "parser.mly"
                                                    ( fun ctx ->    TmAssign(_1 ctx, _3 ctx)                                                ,() )
# 1433 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv582)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv583 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv584)) : 'freshtv586)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv591 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LAND | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv587 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 93 "parser.mly"
                                                    ( fun l r ->    TmLAND(l,r)                                                                 )
# 1476 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx ->    _2 (_1 ctx)(_3 ctx)                                                     ,() )
# 1481 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv588)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv589 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv590)) : 'freshtv592)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv597 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | NEQ | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv593 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 90 "parser.mly"
                                                    ( fun l r ->    TmGT(l,r)                                                                   )
# 1516 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx ->    _2 (_1 ctx)(_3 ctx)                                                     ,() )
# 1521 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv594)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv595 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv596)) : 'freshtv598)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv603 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | EQEQ | FALSE | GT | ID _ | IN | LAND | LPAR | LT | NEQ | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv599 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_tm)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = let _2 = 
# 91 "parser.mly"
                                                    ( fun l r ->    TmEQ(l,r)                                                                   )
# 1556 "parser.ml"
             in
            
# 122 "parser.mly"
                                                    ( fun ctx ->    _2 (_1 ctx)(_3 ctx)                                                     ,() )
# 1561 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv600)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv601 * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv602)) : 'freshtv604)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv611 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv607 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv605 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_aTm = 
# 143 "parser.mly"
                                                    ( fun ctx ->    Balanc (_3 ctx)                                                         ,() )
# 1609 "parser.ml"
             in
            _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv606)) : 'freshtv608)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv609 * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv610)) : 'freshtv612)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv617 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv613 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | ADDRESS ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | ID _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143) : 'freshtv614)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv615 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv616)) : 'freshtv618)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv625 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv619 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | ADDRESS ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | ID _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147) : 'freshtv620)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv621 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | ADDRESS ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | ID _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145) : 'freshtv622)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv623 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv624)) : 'freshtv626)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv631 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv627 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_4 : 'tv_tm)), _, (_6 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 115 "parser.mly"
                                                    ( fun ctx ->    TmIf(_2 ctx, _4 ctx, _6 ctx)                                            ,() )
# 1904 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv628)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv629 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv630)) : 'freshtv632)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv637 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv633 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)), _, (_4 : 'tv_tm)), _, (_6 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 114 "parser.mly"
                                                    ( fun ctx ->    TmIf(_2 ctx, _4 ctx, _6 ctx)                                            ,() )
# 1949 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv634)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv635 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv636)) : 'freshtv638)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv643 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 1964 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv639 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 1996 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (_2 : (
# 12 "parser.mly"
       (string)
# 2001 "parser.ml"
            ))), _, (_4 : 'tv_ty)), _, (_6 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 113 "parser.mly"
                                                    ( fun ctx ->    TmAbs(_2, _4, _6(add_bruijn_idx ctx _2))                                ,() )
# 2006 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv640)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv641 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 2016 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv642)) : 'freshtv644)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv651 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv645 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | ADDRESS ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | ID _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155) : 'freshtv646)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv647 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | ADDRESS ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | ID _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153) : 'freshtv648)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv649 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv650)) : 'freshtv652)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv657 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv653 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _), _, (_4 : 'tv_tm)), _, (_6 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 104 "parser.mly"
                                                    ( fun ctx ->    TmApp((TmAbs("_",TyUnit,_6(add_bruijn_idx ctx "_")),()),_4 ctx)         ,() )
# 2210 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv654)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv655 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv656)) : 'freshtv658)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv663 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv659 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _), _, (_4 : 'tv_tm)), _, (_6 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 103 "parser.mly"
                                                    ( fun ctx ->    TmApp((TmAbs("_",TyUnit,_6(add_bruijn_idx ctx "_")),()),_4 ctx)         ,() )
# 2255 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv660)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv661 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv662)) : 'freshtv664)
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv671 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 2270 "parser.ml"
        )) * (
# 12 "parser.mly"
       (string)
# 2274 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv665 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 2292 "parser.ml"
            )) * (
# 12 "parser.mly"
       (string)
# 2296 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | ADDRESS ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | ID _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166) : 'freshtv666)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv667 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 2374 "parser.ml"
            )) * (
# 12 "parser.mly"
       (string)
# 2378 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | ADDRESS ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | ID _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164) : 'freshtv668)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv669 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 2444 "parser.ml"
            )) * (
# 12 "parser.mly"
       (string)
# 2448 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv670)) : 'freshtv672)
    | MenhirState164 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv677 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 2457 "parser.ml"
        )) * (
# 12 "parser.mly"
       (string)
# 2461 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv673 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 2493 "parser.ml"
            )) * (
# 12 "parser.mly"
       (string)
# 2497 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (((((((_menhir_stack, _menhir_s), _), (_3 : (
# 12 "parser.mly"
       (string)
# 2502 "parser.ml"
            ))), (_4 : (
# 12 "parser.mly"
       (string)
# 2506 "parser.ml"
            ))), _, (_6 : 'tv_ty)), _, (_8 : 'tv_tm)), _, (_10 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 109 "parser.mly"
                                                    ( fun ctx ->    let ctx' = add_rec_idx ctx (_3^"'")                         in 
                                                                    let ctx''= add_struct_idx ctx' _4                           in 
                                                                    let ctx  = add_bruijn_idx ctx  _3                           in 
                                                                    TmApp((TmAbs(_3,_6,_10 ctx),()),(TmFix((_3^"'"),_4,_6,_8 ctx''),()))    ,() )
# 2514 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv674)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv675 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 2524 "parser.ml"
            )) * (
# 12 "parser.mly"
       (string)
# 2528 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv676)) : 'freshtv678)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv683 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 2537 "parser.ml"
        )) * (
# 12 "parser.mly"
       (string)
# 2541 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv679 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 2573 "parser.ml"
            )) * (
# 12 "parser.mly"
       (string)
# 2577 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (((((((_menhir_stack, _menhir_s), _), (_3 : (
# 12 "parser.mly"
       (string)
# 2582 "parser.ml"
            ))), (_4 : (
# 12 "parser.mly"
       (string)
# 2586 "parser.ml"
            ))), _, (_6 : 'tv_ty)), _, (_8 : 'tv_tm)), _, (_10 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 105 "parser.mly"
                                                    ( fun ctx ->    let ctx' = add_rec_idx ctx (_3^"'")                         in 
                                                                    let ctx''= add_struct_idx ctx' _4                           in 
                                                                    let ctx  = add_bruijn_idx ctx  _3                           in 
                                                                    TmApp((TmAbs(_3,_6,_10 ctx),()),(TmFix((_3^"'"),_4,_6,_8 ctx''),()))    ,() )
# 2594 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv680)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv681 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 2604 "parser.ml"
            )) * (
# 12 "parser.mly"
       (string)
# 2608 "parser.ml"
            ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv682)) : 'freshtv684)
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv691 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 2617 "parser.ml"
        ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv685 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 2635 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | ADDRESS ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | ID _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174) : 'freshtv686)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv687 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 2713 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | ADDRESS ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | ID _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172) : 'freshtv688)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv689 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 2779 "parser.ml"
            ))) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv690)) : 'freshtv692)
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv697 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 2788 "parser.ml"
        ))) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv693 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 2820 "parser.ml"
            ))) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), _, (_2 : 'tv_ty)), (_3 : (
# 12 "parser.mly"
       (string)
# 2825 "parser.ml"
            ))), _, (_5 : 'tv_tm)), _, (_7 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 102 "parser.mly"
                                                    ( fun ctx ->    TmApp((TmAbs(_3,_2,_7(add_bruijn_idx ctx _3)),()),_5 ctx)               ,() )
# 2830 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv694)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv695 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 2840 "parser.ml"
            ))) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv696)) : 'freshtv698)
    | MenhirState174 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv703 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 2849 "parser.ml"
        ))) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv699 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 2881 "parser.ml"
            ))) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), _, (_2 : 'tv_ty)), (_3 : (
# 12 "parser.mly"
       (string)
# 2886 "parser.ml"
            ))), _, (_5 : 'tv_tm)), _, (_7 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 101 "parser.mly"
                                                    ( fun ctx ->    TmApp((TmAbs(_3,_2,_7(add_bruijn_idx ctx _3)),()),_5 ctx)               ,() )
# 2891 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv700)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv701 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 2901 "parser.ml"
            ))) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv702)) : 'freshtv704)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv711 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv707 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv705 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_aTm = 
# 133 "parser.mly"
                                                    ( _2                                                                                        )
# 2943 "parser.ml"
             in
            _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv706)) : 'freshtv708)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv709 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv710)) : 'freshtv712)
    | MenhirState179 | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv719 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv713 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | ADDRESS ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | ID _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179) : 'freshtv714)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv715 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_args = 
# 159 "parser.mly"
                                                    ( fun ctx ->    [_1 ctx]                                                                    )
# 3048 "parser.ml"
             in
            _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv716)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv717 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv718)) : 'freshtv720)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv725 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv721 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_ret = 
# 86 "parser.mly"
                                                    ( _1                                                                                        )
# 3093 "parser.ml"
             in
            _menhir_goto_ret _menhir_env _menhir_stack _menhir_s _v) : 'freshtv722)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv723 * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv724)) : 'freshtv726)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv731 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | ABORT | ADDRESS | BALANCE | COMMA | ELSE | FALSE | ID _ | IN | LPAR | NEW | NOW | NUM _ | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv727 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_tm = 
# 118 "parser.mly"
                                                    ( fun ctx ->    TmSfDstr(_2 ctx)                                                        ,() )
# 3138 "parser.ml"
             in
            _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv728)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv729 * _menhir_state) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv730)) : 'freshtv732)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv743 * _menhir_state * 'tv_mthd_head)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQ ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv739 * _menhir_state * 'tv_mthd_head)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv737 * _menhir_state * 'tv_mthd_head)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_mthd_head)), _, (_3 : 'tv_tm)) = _menhir_stack in
            let _v : 'tv_mthd = 
# 58 "parser.mly"
                                                    ( TmMthd(_1,_3 [])                                                                          )
# 3186 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv735) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_mthd) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv733 * _menhir_state * 'tv_mthd) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEFAULT ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | METHOD ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | RBRACE ->
                _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198) : 'freshtv734)) : 'freshtv736)) : 'freshtv738)) : 'freshtv740)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv741 * _menhir_state * 'tv_mthd_head)) * _menhir_state * 'tv_tm) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv742)) : 'freshtv744)
    | _ ->
        _menhir_fail ()

and _menhir_goto_arg_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_arg_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv497 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 3227 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv495 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 3233 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 12 "parser.mly"
       (string)
# 3238 "parser.ml"
        ))), _, (_3 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_tm = 
# 117 "parser.mly"
                                                    ( fun ctx ->    TmLog(_2,_3 ctx,None)                                                   ,() )
# 3243 "parser.ml"
         in
        _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv496)) : 'freshtv498)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv501 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv499 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_appTm = 
# 128 "parser.mly"
                                                    ( fun ctx ->    TmCall("keccak256"       ,_2 ctx)                                       ,() )
# 3255 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv500)) : 'freshtv502)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv505 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv503 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_appTm = 
# 126 "parser.mly"
                                                    ( fun ctx ->    TmCall("iszero"          ,_2 ctx)                                       ,() )
# 3267 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv504)) : 'freshtv506)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv509 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv507 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_appTm = 
# 127 "parser.mly"
                                                    ( fun ctx ->    TmCall("pre_ecdsarecover",_2 ctx)                                       ,() )
# 3279 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv508)) : 'freshtv510)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv511 * _menhir_state * 'tv_tm)) * (
# 12 "parser.mly"
       (string)
# 3287 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | WITH ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | ABORT | ADDRESS | BALANCE | COLONEQ | COMMA | DOT | ELSE | EQEQ | FALSE | GT | ID _ | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | NUM _ | PLUS | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            _menhir_reduce87 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117) : 'freshtv512)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv513 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 3305 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | WITH ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | ABORT | ADDRESS | BALANCE | COLONEQ | COMMA | DOT | ELSE | EQEQ | FALSE | GT | ID _ | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | NUM _ | PLUS | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            _menhir_reduce87 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183) : 'freshtv514)
    | MenhirState192 | MenhirState186 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv527 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 3323 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv525 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 3329 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : (
# 12 "parser.mly"
       (string)
# 3334 "parser.ml"
        ))), _, (_2 : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_call = 
# 154 "parser.mly"
                                                    ( fun ctx ->    TmCall(_1,_2 ctx)                                                       ,() )
# 3339 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv523) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_call) = _v in
        ((match _menhir_s with
        | MenhirState191 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv517 * _menhir_state) * _menhir_state * 'tv_ret))) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_call) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv515 * _menhir_state) * _menhir_state * 'tv_ret))) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((_5 : 'tv_call) : 'tv_call) = _v in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_ret)) = _menhir_stack in
            let _v : 'tv_aTm = 
# 134 "parser.mly"
                                                    ( fun ctx ->    TmReturn(_2 ctx,_5 ctx)                                                 ,() )
# 3359 "parser.ml"
             in
            _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv516)) : 'freshtv518)
        | MenhirState65 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv521) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_call) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv519) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_call) : 'tv_call) = _v in
            ((let _v : 'tv_ret = 
# 85 "parser.mly"
                                                    ( _1                                                                                        )
# 3374 "parser.ml"
             in
            _menhir_goto_ret _menhir_env _menhir_stack _menhir_s _v) : 'freshtv520)) : 'freshtv522)
        | _ ->
            _menhir_fail ()) : 'freshtv524)) : 'freshtv526)) : 'freshtv528)
    | _ ->
        _menhir_fail ()

and _menhir_goto_appTm : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_appTm -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv493 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | COLONEQ | COMMA | DOT | ELSE | EQEQ | GT | IN | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | RBRACE | RPAR | RSQBR | SEMI | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv491 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_appTm)) = _menhir_stack in
        let _v : 'tv_tm = 
# 99 "parser.mly"
                                                    ( _1                                                                                        )
# 3429 "parser.ml"
         in
        _menhir_goto_tm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv492)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv494)

and _menhir_goto_list_mthd_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_mthd_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv481 * _menhir_state * 'tv_mthd) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv479 * _menhir_state * 'tv_mthd) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_mthd)), _, (xs : 'tv_list_mthd_)) = _menhir_stack in
        let _v : 'tv_list_mthd_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 3450 "parser.ml"
         in
        _menhir_goto_list_mthd_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv480)) : 'freshtv482)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv489 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 3458 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv485 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 3468 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv483 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 3475 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (_2 : (
# 12 "parser.mly"
       (string)
# 3480 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)), _, (_5 : 'tv_list_mthd_)) = _menhir_stack in
            let _v : 'tv_cntrct = let _3 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 3487 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 3492 "parser.ml"
                
              in
              
# 48 "parser.mly"
                                                    ( _1                                                                                        )
# 3498 "parser.ml"
              
            in
            
# 54 "parser.mly"
                                                    ( reserved _2; TmCn(_2,_3,_5)                                                               )
# 3504 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv484)) : 'freshtv486)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv487 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 3514 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv488)) : 'freshtv490)
    | _ ->
        _menhir_fail ()

and _menhir_goto_ret : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ret -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv477 * _menhir_state) * _menhir_state * 'tv_ret) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv473 * _menhir_state) * _menhir_state * 'tv_ret) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BECOME ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv469 * _menhir_state) * _menhir_state * 'tv_ret)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv467) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState191 in
                let (_v : (
# 12 "parser.mly"
       (string)
# 3548 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAR ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState192
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192) : 'freshtv468)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191) : 'freshtv470)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv471 * _menhir_state) * _menhir_state * 'tv_ret)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv472)) : 'freshtv474)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv475 * _menhir_state) * _menhir_state * 'tv_ret) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv476)) : 'freshtv478)

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv465 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState74 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv463 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_arg_list = 
# 156 "parser.mly"
                                                    ( fun ctx ->    []                                                                          )
# 3633 "parser.ml"
         in
        _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv464)) : 'freshtv466)
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_goto_aTm : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_aTm -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv461) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_aTm) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv459) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_aTm) : 'tv_aTm) = _v in
    ((let _v : 'tv_pathTm = 
# 131 "parser.mly"
                                                    ( _1                                                                                        )
# 3668 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv457) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_pathTm) = _v in
    ((match _menhir_s with
    | MenhirState51 | MenhirState64 | MenhirState65 | MenhirState74 | MenhirState179 | MenhirState76 | MenhirState170 | MenhirState174 | MenhirState172 | MenhirState162 | MenhirState166 | MenhirState164 | MenhirState82 | MenhirState155 | MenhirState153 | MenhirState87 | MenhirState92 | MenhirState143 | MenhirState147 | MenhirState145 | MenhirState98 | MenhirState100 | MenhirState104 | MenhirState106 | MenhirState108 | MenhirState111 | MenhirState113 | MenhirState118 | MenhirState120 | MenhirState122 | MenhirState124 | MenhirState132 | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv447) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_pathTm) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv445) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_pathTm) : 'tv_pathTm) = _v in
        ((let _v : 'tv_appTm = 
# 124 "parser.mly"
                                                    ( _1                                                                                        )
# 3687 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv446)) : 'freshtv448)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv451 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_pathTm) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv449 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_pathTm) : 'tv_pathTm) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_appTm)) = _menhir_stack in
        let _v : 'tv_appTm = 
# 129 "parser.mly"
                                                    ( fun ctx ->    TmApp(_1 ctx,_2 ctx)                                                    ,() )
# 3703 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv450)) : 'freshtv452)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv455 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_pathTm) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv453 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_pathTm) : 'tv_pathTm) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_appTm = 
# 125 "parser.mly"
                                                    ( fun ctx ->    TmNOT (_2 ctx)                                                          ,() )
# 3719 "parser.ml"
         in
        _menhir_goto_appTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv454)) : 'freshtv456)
    | _ ->
        _menhir_fail ()) : 'freshtv458)) : 'freshtv460)) : 'freshtv462)

and _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_evnt_arg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv439) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv437) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_evnt_arg_) : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ = 
# 144 "<standard.mly>"
    ( x )
# 3740 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv438)) : 'freshtv440)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv443 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv441 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_evnt_arg_) : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_evnt_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_evnt_arg_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3756 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)) : 'freshtv444)
    | _ ->
        _menhir_fail ()

and _menhir_goto_mthd_head : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_mthd_head -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv435 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv431 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ABORT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | ADDRESS ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | BALANCE ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | ECDSARECOVER ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FALSE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | ID _v ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | IF ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | ISZERO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | KECCAK ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LAM ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LET ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LOG ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NEW ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NOW ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NUM _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | RETURN ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SELFDESTRUCT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SENDER ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | THIS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | TRUE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | U8 _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | UNIT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | VALUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51) : 'freshtv432)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv433 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv434)) : 'freshtv436)

and _menhir_reduce36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_mthd_ = 
# 211 "<standard.mly>"
    ( [] )
# 3843 "parser.ml"
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
        let (_menhir_stack : 'freshtv429 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState34 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv425 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv421 * _menhir_state) * _menhir_state)) = Obj.magic _menhir_stack in
                let (_v : (
# 12 "parser.mly"
       (string)
# 3881 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv417 * _menhir_state) * _menhir_state)) * (
# 12 "parser.mly"
       (string)
# 3892 "parser.ml"
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
                        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv418)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv419 * _menhir_state) * _menhir_state)) * (
# 12 "parser.mly"
       (string)
# 3922 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv420)) : 'freshtv422)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv423 * _menhir_state) * _menhir_state)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv424)) : 'freshtv426)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv427 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv428)) : 'freshtv430)
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
    let (_menhir_stack : 'freshtv415) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_mthd_head = 
# 61 "parser.mly"
                                                    ( TyDflt                                                                                 )
# 3958 "parser.ml"
     in
    _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv416)

and _menhir_run93 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 3965 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run99 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
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
# 141 "parser.mly"
                                                    ( fun ctx ->    EpValue                                                                 ,() )
# 4016 "parser.ml"
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
                                                    ( fun ctx ->    TmUnit                                                                  ,() )
# 4050 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "parser.mly"
       (Big_int.big_int)
# 4057 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv395) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 13 "parser.mly"
       (Big_int.big_int)
# 4067 "parser.ml"
    )) : (
# 13 "parser.mly"
       (Big_int.big_int)
# 4071 "parser.ml"
    )) = _v in
    ((let _v : 'tv_aTm = 
# 139 "parser.mly"
                                                    ( fun ctx ->    TmU256 _1                                                               ,() )
# 4076 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv396)

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv393) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_aTm = 
# 136 "parser.mly"
                                                    ( fun ctx ->    TmTrue                                                                  ,() )
# 4089 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv391) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_aTm = 
# 145 "parser.mly"
                                                    ( fun ctx ->    TmThis                                                                  ,() )
# 4102 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv392)

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv387 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MSG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv383 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv379 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv377 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_aTm = 
# 142 "parser.mly"
                                                    ( fun ctx ->    TmSender                                                                ,() )
# 4134 "parser.ml"
                 in
                _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv378)) : 'freshtv380)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv381 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv382)) : 'freshtv384)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv385 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv386)) : 'freshtv388)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv389 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv390)

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv373) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState65 in
        let (_v : (
# 12 "parser.mly"
       (string)
# 4243 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | ABORT | ADDRESS | BALANCE | COLONEQ | DOT | EQEQ | FALSE | GT | ID _ | LAND | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | NUM _ | PLUS | RETURN | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186) : 'freshtv374)
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv375) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState65 in
        ((let _v : 'tv_ret = 
# 84 "parser.mly"
                                                    ( fun ctx ->    TmUnit                                                                  ,() )
# 4302 "parser.ml"
         in
        _menhir_goto_ret _menhir_env _menhir_stack _menhir_s _v) : 'freshtv376)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "parser.mly"
       (Big_int.big_int)
# 4313 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv371) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 13 "parser.mly"
       (Big_int.big_int)
# 4323 "parser.ml"
    )) : (
# 13 "parser.mly"
       (Big_int.big_int)
# 4327 "parser.ml"
    )) = _v in
    ((let _v : 'tv_aTm = 
# 138 "parser.mly"
                                                    ( fun ctx ->    TmU256 _1                                                               ,() )
# 4332 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv372)

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv367 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BLOCK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv363 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv359 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv357 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_aTm = 
# 144 "parser.mly"
                                                    ( fun ctx ->    EpNow                                                                   ,() )
# 4364 "parser.ml"
                 in
                _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv358)) : 'freshtv360)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv361 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv362)) : 'freshtv364)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv365 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv366)) : 'freshtv368)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv369 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv370)

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 12 "parser.mly"
       (string)
# 4444 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv354)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv355 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv356)

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 12 "parser.mly"
       (string)
# 4537 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv350)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv352)

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
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
    | REC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv341 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState80 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv337 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "parser.mly"
       (string)
# 4585 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv333 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 4596 "parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_v : (
# 12 "parser.mly"
       (string)
# 4601 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | COLON ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv329 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 4612 "parser.ml"
                    )) * (
# 12 "parser.mly"
       (string)
# 4616 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ADDRESS ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState160
                    | BOOL ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState160
                    | BYTES32 ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState160
                    | ID _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
                    | UINT256 ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState160
                    | UINT8 ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState160
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160) : 'freshtv330)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv331 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 4644 "parser.ml"
                    )) * (
# 12 "parser.mly"
       (string)
# 4648 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)) : 'freshtv334)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv335 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 4659 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv339 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)) : 'freshtv342)
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | UNIT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv347 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState80 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv343 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | ADDRESS ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | ID _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv344)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv345 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv346)) : 'freshtv348)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 12 "parser.mly"
       (string)
# 4766 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv321 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 4777 "parser.ml"
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv322)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv323 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 4805 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)) : 'freshtv326)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv327 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_reduce15 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 12 "parser.mly"
       (string)
# 4907 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 12 "parser.mly"
       (string)
# 4913 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_aTm = 
# 147 "parser.mly"
                                       ( reserved _1; fun ctx ->    (* #DEBUG prBds ctx;pe $1 ; *)
                                                (           try     TmI(lookup_bruijn_idx _1 ctx,len ctx)                                   ,()  
                                                with _ ->   try     TmIRec(lookup_rec_idx (_1^"'") ctx)                                     ,() 
                                                with _ ->   try     TmIStrct(lookup_struct_idx _1 ctx)                                      ,() 
                                                with _ ->           TmId _1                                                                 ,()))
# 4922 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v

and _menhir_run94 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv319) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_aTm = 
# 137 "parser.mly"
                                                    ( fun ctx ->    TmFalse                                                                 ,() )
# 4935 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv315 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ABORT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | ADDRESS ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | BALANCE ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | ECDSARECOVER ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | FALSE ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | ID _v ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | IF ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | ISZERO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | KECCAK ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LAM ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LET ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LOG ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | NEW ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | NOT ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | NOW ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | NUM _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | RETURN ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | SELFDESTRUCT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | SENDER ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | THIS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | TRUE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | U8 _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | UNIT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | VALUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv316)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv317 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv318)

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | ADDRESS ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | BALANCE ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | ECDSARECOVER ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | FALSE ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | ID _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | ISZERO ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | KECCAK ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | LAM ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | LET ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | LOG ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | LPAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NEW ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NOT ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NOW ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NUM _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | RETURN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | SELFDESTRUCT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | U8 _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | UNIT ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | VALUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run101 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv313) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_aTm = 
# 135 "parser.mly"
                                                    ( fun ctx ->    TmAbort                                                                 ,() )
# 5095 "parser.ml"
     in
    _menhir_goto_aTm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)

and _menhir_goto_separated_nonempty_list_COMMA_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_arg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState46 | MenhirState38 | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_arg_) : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 144 "<standard.mly>"
    ( x )
# 5114 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv306)) : 'freshtv308)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv311 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv309 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_arg_) : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_arg_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 5130 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)) : 'freshtv312)
    | _ ->
        _menhir_fail ()

and _menhir_goto_evnt_arg : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_evnt_arg -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv303 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv297 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv298)
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv299 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_evnt_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_evnt_arg_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 5175 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv300)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv301 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)) : 'freshtv304)

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
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_cntrct : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_cntrct -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv295 * _menhir_state * 'tv_cntrct) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONTRACT ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState205
    | EVENT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState205
    | EOF ->
        _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState205
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205) : 'freshtv296)

and _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv277 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 5263 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv273 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 5273 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv269 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 5283 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | DEFAULT ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | METHOD ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | RBRACE ->
                    _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv270)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv271 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 5305 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)) : 'freshtv274)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv275 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 5316 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)) : 'freshtv278)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv285 * _menhir_state) * _menhir_state)) * (
# 12 "parser.mly"
       (string)
# 5325 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv281 * _menhir_state) * _menhir_state)) * (
# 12 "parser.mly"
       (string)
# 5335 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv279 * _menhir_state) * _menhir_state)) * (
# 12 "parser.mly"
       (string)
# 5342 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _), (_4 : (
# 12 "parser.mly"
       (string)
# 5347 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
            let _v : 'tv_mthd_head = let _5 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 5354 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 5359 "parser.ml"
                
              in
              
# 48 "parser.mly"
                                                    ( _1                                                                                        )
# 5365 "parser.ml"
              
            in
            
# 63 "parser.mly"
                                                    ( TyMthd(_4,_5,TyUnit   )                                                                   )
# 5371 "parser.ml"
             in
            _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv280)) : 'freshtv282)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv283 * _menhir_state) * _menhir_state)) * (
# 12 "parser.mly"
       (string)
# 5381 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)) : 'freshtv286)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv293 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 5390 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv289 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 5400 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv287 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 5407 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_ty)), (_3 : (
# 12 "parser.mly"
       (string)
# 5412 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
            let _v : 'tv_mthd_head = let _4 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 5419 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 5424 "parser.ml"
                
              in
              
# 48 "parser.mly"
                                                    ( _1                                                                                        )
# 5430 "parser.ml"
              
            in
            
# 62 "parser.mly"
                                                    ( TyMthd(_3,_4,_2)                                                                          )
# 5436 "parser.ml"
             in
            _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv288)) : 'freshtv290)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv291 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 5446 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv292)) : 'freshtv294)
    | _ ->
        _menhir_fail ()

and _menhir_reduce86 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5456 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 12 "parser.mly"
       (string)
# 5462 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_ty = 
# 80 "parser.mly"
                                                    ( TyIstc _1                                                                               )
# 5467 "parser.ml"
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
# 12 "parser.mly"
       (string)
# 5491 "parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv187 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let ((_3 : (
# 12 "parser.mly"
       (string)
# 5499 "parser.ml"
            )) : (
# 12 "parser.mly"
       (string)
# 5503 "parser.ml"
            )) = _v in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_evnt_arg = 
# 70 "parser.mly"
                                                    ( TyEvVar(_3,_2,true)                                                                       )
# 5509 "parser.ml"
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
# 78 "parser.mly"
                                                    ( TyMap(_1,_3)                                                                              )
# 5536 "parser.ml"
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
# 79 "parser.mly"
                                                    ( TyAbs(_1,_3)                                                                              )
# 5563 "parser.ml"
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
# 12 "parser.mly"
       (string)
# 5589 "parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv221 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let ((_2 : (
# 12 "parser.mly"
       (string)
# 5597 "parser.ml"
            )) : (
# 12 "parser.mly"
       (string)
# 5601 "parser.ml"
            )) = _v in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_arg = 
# 66 "parser.mly"
                                                    ( reserved _2; TyVar(_2,_1)                                                                 )
# 5607 "parser.ml"
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
# 69 "parser.mly"
                                                    ( let TyVar(id,ty)=_1 in TyEvVar(id,ty,false)                                               )
# 5624 "parser.ml"
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
# 5662 "parser.ml"
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
# 12 "parser.mly"
       (string)
# 5697 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv229 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 5708 "parser.ml"
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
                    _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46) : 'freshtv230)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv231 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 5738 "parser.ml"
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
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv251 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 5754 "parser.ml"
        ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv247 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 5764 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | ADDRESS ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv241) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState87 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAR ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
                | ARROW | DARROW ->
                    _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv239 * _menhir_state) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)) : 'freshtv242)
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | BYTES32 ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv245) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState87 in
                let (_v : (
# 12 "parser.mly"
       (string)
# 5807 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ARROW | DARROW ->
                    _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack)
                | ABORT | ADDRESS | BALANCE | COLONEQ | COMMA | DOT | ELSE | EQEQ | FALSE | GT | ID _ | IN | LAND | LPAR | LSQBR | LT | MINUS | MULT | NEQ | NEW | NOW | NUM _ | PLUS | RBRACE | RETURN | RPAR | RSQBR | SEMI | SENDER | THEN | THIS | TRUE | U8 _ | UNIT | VALUE ->
                    _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv243 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5824 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)) : 'freshtv246)
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv248)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv249 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 5883 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv250)) : 'freshtv252)
    | MenhirState160 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv257 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 5892 "parser.ml"
        )) * (
# 12 "parser.mly"
       (string)
# 5896 "parser.ml"
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
            let (_menhir_stack : ((((('freshtv253 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 5910 "parser.ml"
            )) * (
# 12 "parser.mly"
       (string)
# 5914 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | ADDRESS ->
                _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | BALANCE ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | ECDSARECOVER ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | FALSE ->
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | ID _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
            | IF ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | ISZERO ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | KECCAK ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | LAM ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | LET ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | LOG ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | LPAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | NEW ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | NOT ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | NOW ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | NUM _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
            | RETURN ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | SELFDESTRUCT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | U8 _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
            | UNIT ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | VALUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162) : 'freshtv254)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv255 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 5980 "parser.ml"
            )) * (
# 12 "parser.mly"
       (string)
# 5984 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)) : 'freshtv258)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv267 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv263 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 12 "parser.mly"
       (string)
# 6004 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv259 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 6015 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ABORT ->
                    _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | ADDRESS ->
                    _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | BALANCE ->
                    _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | ECDSARECOVER ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | FALSE ->
                    _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | ID _v ->
                    _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
                | IF ->
                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | ISZERO ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | KECCAK ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | LAM ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | LET ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | LOG ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | LPAR ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | NEW ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | NOT ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | NOW ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | NUM _v ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
                | RETURN ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | SELFDESTRUCT ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | SENDER ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | THIS ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | TRUE ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | U8 _v ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
                | UNIT ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | VALUE ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170) : 'freshtv260)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv261 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 6081 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)) : 'freshtv264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv265 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)) : 'freshtv268)
    | _ ->
        _menhir_fail ()

and _menhir_reduce82 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : 'tv_ty = 
# 76 "parser.mly"
                                                    ( TyAddr                                                                                    )
# 6101 "parser.ml"
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
# 44 "parser.mly"
       (unit Syntax.toplevel list)
# 6124 "parser.ml"
            ) = 
# 51 "parser.mly"
                                                    ( _1                                                                                        )
# 6128 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv173) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 44 "parser.mly"
       (unit Syntax.toplevel list)
# 6136 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv171) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 44 "parser.mly"
       (unit Syntax.toplevel list)
# 6144 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv169) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 44 "parser.mly"
       (unit Syntax.toplevel list)
# 6152 "parser.ml"
            )) : (
# 44 "parser.mly"
       (unit Syntax.toplevel list)
# 6156 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv170)) : 'freshtv172)) : 'freshtv174)) : 'freshtv176)) : 'freshtv178)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv179 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)) : 'freshtv182)
    | MenhirState205 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * _menhir_state * 'tv_cntrct) * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183 * _menhir_state * 'tv_cntrct) * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_cntrct)), _, (xs : 'tv_list_cntrct_)) = _menhir_stack in
        let _v : 'tv_list_cntrct_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 6175 "parser.ml"
         in
        _menhir_goto_list_cntrct_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)) : 'freshtv186)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv167 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6188 "parser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv163 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6198 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv159 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6208 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv157 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6215 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (_2 : (
# 12 "parser.mly"
       (string)
# 6220 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = _menhir_stack in
            let _v : 'tv_cntrct = let _3 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 6227 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 6232 "parser.ml"
                
              in
              
# 48 "parser.mly"
                                                    ( _1                                                                                        )
# 6238 "parser.ml"
              
            in
            
# 55 "parser.mly"
                                                    ( TmEv(TyEv(_2,_3))                                                                         )
# 6244 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)) : 'freshtv160)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv161 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6254 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)) : 'freshtv164)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv165 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6265 "parser.ml"
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

and _menhir_reduce38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 142 "<standard.mly>"
    ( [] )
# 6298 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv155) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 74 "parser.mly"
                                                    ( TyU8                                                                                      )
# 6311 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv153) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 73 "parser.mly"
                                                    ( TyU256                                                                                    )
# 6324 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv154)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 6331 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv151) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 75 "parser.mly"
                                                    ( TyBytes32                                                                                 )
# 6347 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv152)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv149) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 77 "parser.mly"
                                                    ( TyBool                                                                                    )
# 6360 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState205 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_cntrct) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_mthd) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState192 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6388 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv27 * _menhir_state) * _menhir_state * 'tv_ret))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState186 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6402 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv31 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6411 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv33 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState174 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv35 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 6425 "parser.ml"
        ))) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv37 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 6434 "parser.ml"
        ))) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv39 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 6443 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv41 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6452 "parser.ml"
        )) * (
# 12 "parser.mly"
       (string)
# 6456 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState164 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv43 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6465 "parser.ml"
        )) * (
# 12 "parser.mly"
       (string)
# 6469 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv45 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6478 "parser.ml"
        )) * (
# 12 "parser.mly"
       (string)
# 6482 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState160 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv47 * _menhir_state) * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6491 "parser.ml"
        )) * (
# 12 "parser.mly"
       (string)
# 6495 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv49 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv51 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv53 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv55 * _menhir_state) * _menhir_state * 'tv_tm)) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv57 * _menhir_state) * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState138 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv59 * _menhir_state * 'tv_tm))))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state * 'tv_appTm) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv75 * _menhir_state * 'tv_tm)) * (
# 12 "parser.mly"
       (string)
# 6569 "parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv77 * _menhir_state * 'tv_tm)) * (
# 12 "parser.mly"
       (string)
# 6578 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * 'tv_tm)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv101 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6642 "parser.ml"
        ))) * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv103 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6651 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv105 * _menhir_state) * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv109 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6670 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv115 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6689 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123 * _menhir_state * 'tv_mthd_head)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv125 * _menhir_state) * _menhir_state * 'tv_ty) * (
# 12 "parser.mly"
       (string)
# 6718 "parser.ml"
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
# 12 "parser.mly"
       (string)
# 6732 "parser.ml"
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
# 12 "parser.mly"
       (string)
# 6746 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv135 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6755 "parser.ml"
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
# 12 "parser.mly"
       (string)
# 6784 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv148)

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_cntrct_ = 
# 211 "<standard.mly>"
    ( [] )
# 6798 "parser.ml"
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
# 12 "parser.mly"
       (string)
# 6814 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6825 "parser.ml"
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
# 6851 "parser.ml"
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
# 12 "parser.mly"
       (string)
# 6865 "parser.ml"
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
# 12 "parser.mly"
       (string)
# 6889 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv3 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6900 "parser.ml"
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
                _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv5 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6930 "parser.ml"
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
# 6957 "parser.ml"
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
        _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 6985 "parser.ml"
