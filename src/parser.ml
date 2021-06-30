
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
# 6 "parser.mly"
       (string)
# 43 "parser.ml"
  )
    | GT
    | FALSE
    | EVENT
    | EQEQ
    | EQ
    | EOF
    | ELSE
    | DOT
    | DEFAULT
    | DECLIT8 of (
# 8 "parser.mly"
       (Big_int.big_int)
# 57 "parser.ml"
  )
    | DECLIT256 of (
# 7 "parser.mly"
       (Big_int.big_int)
# 62 "parser.ml"
  )
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
  | MenhirState189
  | MenhirState182
  | MenhirState177
  | MenhirState174
  | MenhirState168
  | MenhirState165
  | MenhirState161
  | MenhirState154
  | MenhirState152
  | MenhirState148
  | MenhirState142
  | MenhirState139
  | MenhirState136
  | MenhirState131
  | MenhirState122
  | MenhirState118
  | MenhirState113
  | MenhirState110
  | MenhirState108
  | MenhirState106
  | MenhirState104
  | MenhirState102
  | MenhirState101
  | MenhirState98
  | MenhirState94
  | MenhirState92
  | MenhirState89
  | MenhirState87
  | MenhirState85
  | MenhirState81
  | MenhirState79
  | MenhirState74
  | MenhirState72
  | MenhirState71
  | MenhirState68
  | MenhirState53
  | MenhirState51
  | MenhirState50
  | MenhirState45
  | MenhirState41
  | MenhirState36
  | MenhirState33
  | MenhirState31
  | MenhirState26
  | MenhirState21
  | MenhirState14
  | MenhirState3
  | MenhirState0

# 1 "parser.mly"
  
    open Syntax 

# 144 "parser.ml"

let rec _menhir_goto_list_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_stmt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState177 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv655 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv653 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_stmt)), _, (xs : 'tv_list_stmt_)) = _menhir_stack in
        let _v : 'tv_list_stmt_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 159 "parser.ml"
         in
        _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv654)) : 'freshtv656)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv713 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv709 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv707 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_list_stmt_)) = _menhir_stack in
            let _v : 'tv_block = 
# 53 "parser.mly"
                                                    ( _2                                                        )
# 178 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv705) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_block) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState113 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv679) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv677) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, (_2 : 'tv_block)) = _menhir_stack in
                let _v : 'tv_reentrance_info = 
# 133 "parser.mly"
                                                    ( _2                                                                    )
# 195 "parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv675) = _menhir_stack in
                let (_v : 'tv_reentrance_info) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv673 * _menhir_state * 'tv_value_info) = Obj.magic _menhir_stack in
                let (_v : 'tv_reentrance_info) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv671 * _menhir_state * 'tv_value_info) = Obj.magic _menhir_stack in
                let ((_2 : 'tv_reentrance_info) : 'tv_reentrance_info) = _v in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_value_info)) = _menhir_stack in
                let _v : 'tv_msg = 
# 126 "parser.mly"
                                                    ( {msg_value=_1; msg_reentrance=_2}                                     )
# 210 "parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv669) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_msg) = _v in
                ((match _menhir_s with
                | MenhirState101 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((('freshtv659 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 222 "parser.ml"
                    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = _menhir_s in
                    let (_v : 'tv_msg) = _v in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((('freshtv657 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 230 "parser.ml"
                    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                    let (_ : _menhir_state) = _menhir_s in
                    let ((_5 : 'tv_msg) : 'tv_msg) = _v in
                    ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), (_3 : (
# 6 "parser.mly"
       (string)
# 237 "parser.ml"
                    ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
                    let _v : 'tv_expr = let _4 =
                      let _1 =
                        let _1 =
                          let x = 
# 232 "<standard.mly>"
    ( xs )
# 245 "parser.ml"
                           in
                          
# 200 "<standard.mly>"
    ( x )
# 250 "parser.ml"
                          
                        in
                        
# 40 "parser.mly"
                                                    ( _1                                                        )
# 256 "parser.ml"
                        
                      in
                      
# 123 "parser.mly"
                                                    ( _1                                                                    )
# 262 "parser.ml"
                      
                    in
                    
# 118 "parser.mly"
                                                    ( EpSend{send_cntrct=_1;send_mthd=Some _3;send_args=_4;send_msg=_5},()  )
# 268 "parser.ml"
                     in
                    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv658)) : 'freshtv660)
                | MenhirState122 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((('freshtv663 * _menhir_state * 'tv_expr))))) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = _menhir_s in
                    let (_v : 'tv_msg) = _v in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((('freshtv661 * _menhir_state * 'tv_expr))))) = Obj.magic _menhir_stack in
                    let (_ : _menhir_state) = _menhir_s in
                    let ((_6 : 'tv_msg) : 'tv_msg) = _v in
                    ((let (_menhir_stack, _menhir_s, (_1 : 'tv_expr)) = _menhir_stack in
                    let _v : 'tv_expr = 
# 117 "parser.mly"
                                                    ( EpSend{send_cntrct=_1;send_mthd=None   ;send_args=[];send_msg=_6},()  )
# 284 "parser.ml"
                     in
                    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv662)) : 'freshtv664)
                | MenhirState131 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((('freshtv667 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 292 "parser.ml"
                    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = _menhir_s in
                    let (_v : 'tv_msg) = _v in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((('freshtv665 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 300 "parser.ml"
                    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                    let (_ : _menhir_state) = _menhir_s in
                    let ((_4 : 'tv_msg) : 'tv_msg) = _v in
                    ((let (((_menhir_stack, _menhir_s), (_2 : (
# 6 "parser.mly"
       (string)
# 307 "parser.ml"
                    ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
                    let _v : 'tv_expr = let _3 =
                      let _1 =
                        let _1 =
                          let x = 
# 232 "<standard.mly>"
    ( xs )
# 315 "parser.ml"
                           in
                          
# 200 "<standard.mly>"
    ( x )
# 320 "parser.ml"
                          
                        in
                        
# 40 "parser.mly"
                                                    ( _1                                                        )
# 326 "parser.ml"
                        
                      in
                      
# 123 "parser.mly"
                                                    ( _1                                                                    )
# 332 "parser.ml"
                      
                    in
                    
# 116 "parser.mly"
                                                    ( EpNew {new_head=_2;new_args=_3; new_msg=_4},                      ()  )
# 338 "parser.ml"
                     in
                    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv666)) : 'freshtv668)
                | _ ->
                    _menhir_fail ()) : 'freshtv670)) : 'freshtv672)) : 'freshtv674)) : 'freshtv676)) : 'freshtv678)) : 'freshtv680)
            | MenhirState165 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv683 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv681 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_stmt)), _, (_1_inlined2 : 'tv_block)) = _menhir_stack in
                let _v : 'tv_stmt = let _6 =
                  let _1 = _1_inlined2 in
                  
# 78 "parser.mly"
                                                    ( _1                                                        )
# 354 "parser.ml"
                  
                in
                let _4 =
                  let _1 = _1_inlined1 in
                  
# 77 "parser.mly"
                                                    ( [_1]                                                      )
# 362 "parser.ml"
                  
                in
                
# 86 "parser.mly"
                                                    ( SmIf(_2,_4,_6)                                            )
# 368 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv682)) : 'freshtv684)
            | MenhirState154 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv691 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ELSE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv685 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ABORT ->
                        _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | ADDRESS ->
                        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | BALANCE ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | BOOL ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | BYTES32 ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | DECLIT256 _v ->
                        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
                    | DECLIT8 _v ->
                        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
                    | FALSE ->
                        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | ID _v ->
                        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
                    | IF ->
                        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | LBRACE ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | LOG ->
                        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | LPAR ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | NEW ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | NOT ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | NOW ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | RETURN ->
                        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | SELFDESTRUCT ->
                        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | SENDER ->
                        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | THIS ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | TRUE ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | UINT256 ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | UINT8 ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | VALUE ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | VOID ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174) : 'freshtv686)
                | ABORT | ADDRESS | BALANCE | BOOL | BYTES32 | DECLIT256 _ | DECLIT8 _ | FALSE | ID _ | IF | LOG | LPAR | NEW | NOT | NOW | RBRACE | RETURN | SELFDESTRUCT | SENDER | THIS | TRUE | UINT256 | UINT8 | VALUE | VOID ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv687 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_block)) = _menhir_stack in
                    let _v : 'tv_stmt = let _4 =
                      let _1 = _1_inlined1 in
                      
# 78 "parser.mly"
                                                    ( _1                                                        )
# 446 "parser.ml"
                      
                    in
                    
# 87 "parser.mly"
                                                    ( SmIfThen (_2, _4)                                         )
# 452 "parser.ml"
                     in
                    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv688)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv689 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv690)) : 'freshtv692)
            | MenhirState174 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv695 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv693 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_block)), _, (_1_inlined2 : 'tv_block)) = _menhir_stack in
                let _v : 'tv_stmt = let _6 =
                  let _1 = _1_inlined2 in
                  
# 78 "parser.mly"
                                                    ( _1                                                        )
# 473 "parser.ml"
                  
                in
                let _4 =
                  let _1 = _1_inlined1 in
                  
# 78 "parser.mly"
                                                    ( _1                                                        )
# 481 "parser.ml"
                  
                in
                
# 86 "parser.mly"
                                                    ( SmIf(_2,_4,_6)                                            )
# 487 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv694)) : 'freshtv696)
            | MenhirState50 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv703 * _menhir_state * 'tv_mthd_head) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv701 * _menhir_state * 'tv_mthd_head) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_mthd_head)), _, (_2 : 'tv_block)) = _menhir_stack in
                let _v : 'tv_mthd = 
# 50 "parser.mly"
                                                    ( {mthd_head=_1; mthd_body=_2}                              )
# 499 "parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv699) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_mthd) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv697 * _menhir_state * 'tv_mthd) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | DEFAULT ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState182
                | METHOD ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState182
                | RBRACE ->
                    _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState182
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182) : 'freshtv698)) : 'freshtv700)) : 'freshtv702)) : 'freshtv704)
            | _ ->
                _menhir_fail ()) : 'freshtv706)) : 'freshtv708)) : 'freshtv710)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv711 * _menhir_state) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv712)) : 'freshtv714)
    | _ ->
        _menhir_fail ()

and _menhir_reduce81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_value_info = 
# 129 "parser.mly"
                                                    ( None                                                                  )
# 538 "parser.ml"
     in
    _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_stmt_ = 
# 211 "<standard.mly>"
    ( [] )
# 586 "parser.ml"
     in
    _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv625 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 599 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv621 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 609 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ALONG ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | REENTRANCE ->
                _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv622)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv623 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 629 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv624)) : 'freshtv626)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv633 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 638 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv629 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 648 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv627 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 655 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 660 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _v : 'tv_expr = let _2 =
              let _1 =
                let _1 =
                  let x = 
# 232 "<standard.mly>"
    ( xs )
# 668 "parser.ml"
                   in
                  
# 200 "<standard.mly>"
    ( x )
# 673 "parser.ml"
                  
                in
                
# 40 "parser.mly"
                                                    ( _1                                                        )
# 679 "parser.ml"
                
              in
              
# 123 "parser.mly"
                                                    ( _1                                                                    )
# 685 "parser.ml"
              
            in
            
# 115 "parser.mly"
                                                    ( Printf.printf "\n%s\n" _1; EpFnCall{call_head=_1;call_args=_2},                              ()  )
# 691 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv628)) : 'freshtv630)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv631 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 701 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv632)) : 'freshtv634)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv639 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 710 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv635 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 720 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ALONG ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | REENTRANCE ->
                _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131) : 'freshtv636)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv637 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 740 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv638)) : 'freshtv640)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv651 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 749 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv647 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 759 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv643 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 769 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv641 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 776 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s), (_2 : (
# 6 "parser.mly"
       (string)
# 781 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
                let _v : 'tv_stmt = let _3 =
                  let _1 =
                    let _1 =
                      let x = 
# 232 "<standard.mly>"
    ( xs )
# 789 "parser.ml"
                       in
                      
# 200 "<standard.mly>"
    ( x )
# 794 "parser.ml"
                      
                    in
                    
# 40 "parser.mly"
                                                    ( _1                                                        )
# 800 "parser.ml"
                    
                  in
                  
# 123 "parser.mly"
                                                    ( _1                                                                    )
# 806 "parser.ml"
                  
                in
                
# 88 "parser.mly"
                                                    ( SmLog(_2,_3,None)                                         )
# 812 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv642)) : 'freshtv644)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv645 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 822 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv646)) : 'freshtv648)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv649 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 833 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv650)) : 'freshtv652)
    | _ ->
        _menhir_fail ()

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv617 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | BALANCE ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | DECLIT256 _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | DECLIT8 _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | FALSE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | ID _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | LPAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NEW ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NOW ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | SENDER ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | THIS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | TRUE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | VALUE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv618)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv619 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv620)

and _menhir_run136 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136

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
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | DECLIT8 _v ->
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
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv615) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState139 in
        ((let _v : 'tv_option_expr_ = 
# 114 "<standard.mly>"
    ( None )
# 972 "parser.ml"
         in
        _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv616)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139

and _menhir_run146 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv611 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 6 "parser.mly"
       (string)
# 992 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv607 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 1003 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | DECLIT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | DECLIT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | ID _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | RPAR ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148) : 'freshtv608)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv609 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 1049 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv610)) : 'freshtv612)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv613 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv614)

and _menhir_run152 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152

and _menhir_run155 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (string)
# 1103 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
    | DARROW | ID _ ->
        _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack)
    | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS ->
        _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv605 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 1123 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv606)

and _menhir_run156 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
    | DARROW | ID _ ->
        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv603 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv604)

and _menhir_run157 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv599 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv597 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_stmt = 
# 81 "parser.mly"
                                                    ( SmAbort                                                   )
# 1162 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv598)) : 'freshtv600)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv601 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv602)

and _menhir_goto_option_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv595 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv591 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BECOME ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv587 * _menhir_state) * _menhir_state * 'tv_option_expr_)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | DECLIT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | DECLIT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | ID _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142) : 'freshtv588)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv589 * _menhir_state) * _menhir_state * 'tv_option_expr_)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv590)) : 'freshtv592)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv593 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv594)) : 'freshtv596)

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_stmt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv575 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv569 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | ADDRESS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | DECLIT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | DECLIT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | ID _v ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | IF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | LBRACE ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | LOG ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | RETURN ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | SELFDESTRUCT ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | VOID ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165) : 'freshtv570)
        | ABORT | ADDRESS | BALANCE | BOOL | BYTES32 | DECLIT256 _ | DECLIT8 _ | FALSE | ID _ | IF | LOG | LPAR | NEW | NOT | NOW | RBRACE | RETURN | SELFDESTRUCT | SENDER | THIS | TRUE | UINT256 | UINT8 | VALUE | VOID ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv571 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_stmt)) = _menhir_stack in
            let _v : 'tv_stmt = let _4 =
              let _1 = _1_inlined1 in
              
# 77 "parser.mly"
                                                    ( [_1]                                                      )
# 1319 "parser.ml"
              
            in
            
# 87 "parser.mly"
                                                    ( SmIfThen (_2, _4)                                         )
# 1325 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv572)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv573 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv574)) : 'freshtv576)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv579 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv577 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_stmt)), _, (_1_inlined2 : 'tv_stmt)) = _menhir_stack in
        let _v : 'tv_stmt = let _6 =
          let _1 = _1_inlined2 in
          
# 77 "parser.mly"
                                                    ( [_1]                                                      )
# 1346 "parser.ml"
          
        in
        let _4 =
          let _1 = _1_inlined1 in
          
# 77 "parser.mly"
                                                    ( [_1]                                                      )
# 1354 "parser.ml"
          
        in
        
# 86 "parser.mly"
                                                    ( SmIf(_2,_4,_6)                                            )
# 1360 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv578)) : 'freshtv580)
    | MenhirState174 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv583 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv581 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_block)), _, (_1_inlined2 : 'tv_stmt)) = _menhir_stack in
        let _v : 'tv_stmt = let _6 =
          let _1 = _1_inlined2 in
          
# 77 "parser.mly"
                                                    ( [_1]                                                      )
# 1374 "parser.ml"
          
        in
        let _4 =
          let _1 = _1_inlined1 in
          
# 78 "parser.mly"
                                                    ( _1                                                        )
# 1382 "parser.ml"
          
        in
        
# 86 "parser.mly"
                                                    ( SmIf(_2,_4,_6)                                            )
# 1388 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv582)) : 'freshtv584)
    | MenhirState177 | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv585 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ABORT ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | ADDRESS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | BALANCE ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | BOOL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | BYTES32 ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | DECLIT256 _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
        | DECLIT8 _v ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
        | FALSE ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | ID _v ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
        | IF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | LOG ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | LPAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | NEW ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | NOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | NOW ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | RETURN ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | SELFDESTRUCT ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | SENDER ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | THIS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | TRUE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | UINT256 ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | UINT8 ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | VALUE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | VOID ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | RBRACE ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177) : 'freshtv586)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState148 | MenhirState71 | MenhirState74 | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv563) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv561) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 144 "<standard.mly>"
    ( x )
# 1469 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv562)) : 'freshtv564)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv567 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv565 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 1485 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv566)) : 'freshtv568)
    | _ ->
        _menhir_fail ()

and _menhir_goto_value_info : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_value_info -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv559 * _menhir_state * 'tv_value_info) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REENTRANCE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv555) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv556)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv557 * _menhir_state * 'tv_value_info) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv558)) : 'freshtv560)

and _menhir_reduce32 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_lexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : 'tv_lexpr)) = _menhir_stack in
    let _v : 'tv_expr = 
# 120 "parser.mly"
                                                    ( EpArray _1,                                                       ()  )
# 1525 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run104 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_run108 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run110 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | ID _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEFAULT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv545 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv541 * _menhir_state * 'tv_expr))) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv537 * _menhir_state * 'tv_expr)))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ALONG ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | REENTRANCE ->
                    _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122) : 'freshtv538)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv539 * _menhir_state * 'tv_expr)))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv540)) : 'freshtv542)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv543 * _menhir_state * 'tv_expr))) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv544)) : 'freshtv546)
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv551 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        let (_v : (
# 6 "parser.mly"
       (string)
# 1922 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv547 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 1933 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | DECLIT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | DECLIT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | ID _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | RPAR ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv548)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv549 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 1979 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv550)) : 'freshtv552)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv553 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv554)

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | ADDRESS ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BOOL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BYTES32 ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | FALSE ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | ID _v ->
        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LOG ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | RETURN ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SELFDESTRUCT ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | VOID ->
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
    | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv527 * _menhir_state * 'tv_mthd) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv525 * _menhir_state * 'tv_mthd) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_mthd)), _, (xs : 'tv_list_mthd_)) = _menhir_stack in
        let _v : 'tv_list_mthd_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2065 "parser.ml"
         in
        _menhir_goto_list_mthd_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv526)) : 'freshtv528)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv535 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 2073 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv531 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 2083 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv529 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 2090 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (_2 : (
# 6 "parser.mly"
       (string)
# 2095 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)), _, (_5 : 'tv_list_mthd_)) = _menhir_stack in
            let _v : 'tv_cntrct = let _3 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 2102 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 2107 "parser.ml"
                
              in
              
# 40 "parser.mly"
                                                    ( _1                                                        )
# 2113 "parser.ml"
              
            in
            
# 46 "parser.mly"
                                                    ( Cntrct{mthds=_5; cntrct_name=_2; cntrct_args=_3}          )
# 2119 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv530)) : 'freshtv532)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv533 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 2129 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv534)) : 'freshtv536)
    | _ ->
        _menhir_fail ()

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 142 "<standard.mly>"
    ( [] )
# 2141 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce26 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 6 "parser.mly"
       (string)
# 2148 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 2154 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_expr = 
# 114 "parser.mly"
                                                    ( EpIdent _1,                                                       ()  )
# 2159 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 6 "parser.mly"
       (string)
# 2166 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | DECLIT8 _v ->
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
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
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
        let (_menhir_stack : (('freshtv365 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv361 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv359 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = 
# 113 "parser.mly"
                                                    ( EpAddr _3,                                                        ()  )
# 2247 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv360)) : 'freshtv362)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv363 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv364)) : 'freshtv366)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv371 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | MINUS | NEQ | PLUS | REENTRANCE | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv367 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 92 "parser.mly"
                                                    ( fun(l,r)-> EpPlus(l,r)                                    )
# 2276 "parser.ml"
             in
            
# 106 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2281 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv368)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv369 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv370)) : 'freshtv372)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv377 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | MINUS | MULT | NEQ | PLUS | REENTRANCE | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv373 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 94 "parser.mly"
                                                    ( fun(l,r)-> EpMult(l,r)                                    )
# 2308 "parser.ml"
             in
            
# 106 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2313 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv374)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv375 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)) : 'freshtv378)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv395 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | RSQBR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv391 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv389 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_lexpr = 
# 136 "parser.mly"
                                                    ( LEpArray{arrIdent=_1; arrIndex=_3}                                    )
# 2359 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv387) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_lexpr) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState168 | MenhirState161 | MenhirState152 | MenhirState148 | MenhirState139 | MenhirState142 | MenhirState136 | MenhirState53 | MenhirState68 | MenhirState71 | MenhirState72 | MenhirState74 | MenhirState79 | MenhirState118 | MenhirState98 | MenhirState110 | MenhirState108 | MenhirState106 | MenhirState104 | MenhirState102 | MenhirState94 | MenhirState92 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState81 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv379 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                (_menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) : 'freshtv380)
            | MenhirState51 | MenhirState177 | MenhirState174 | MenhirState154 | MenhirState165 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv385 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | EQ ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv381 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ADDRESS ->
                        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | BALANCE ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | DECLIT256 _v ->
                        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                    | DECLIT8 _v ->
                        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                    | FALSE ->
                        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | ID _v ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                    | LPAR ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | NEW ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | NOT ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | NOW ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | SENDER ->
                        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | THIS ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | TRUE ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | VALUE ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168) : 'freshtv382)
                | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS ->
                    _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv383 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv384)) : 'freshtv386)
            | _ ->
                _menhir_fail ()) : 'freshtv388)) : 'freshtv390)) : 'freshtv392)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv393 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv394)) : 'freshtv396)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv401 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | NEQ | REENTRANCE | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv397 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 98 "parser.mly"
                                                    ( fun(l,r)-> EpNeq(l,r)                                     )
# 2456 "parser.ml"
             in
            
# 106 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2461 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv399 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv400)) : 'freshtv402)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv407 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | MINUS | NEQ | PLUS | REENTRANCE | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv403 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 93 "parser.mly"
                                                    ( fun(l,r)-> EpMinus(l,r)                                   )
# 2490 "parser.ml"
             in
            
# 106 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2495 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv404)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv405 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv406)) : 'freshtv408)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv413 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | REENTRANCE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv409 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_value_info = 
# 130 "parser.mly"
                                                    ( Some _2                                                               )
# 2538 "parser.ml"
             in
            _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v) : 'freshtv410)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv411 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv412)) : 'freshtv414)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv419 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | NEQ | REENTRANCE | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv415 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 95 "parser.mly"
                                                    ( fun(l,r)-> EpLT(l,r)                                      )
# 2571 "parser.ml"
             in
            
# 106 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2576 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv416)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv417 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv418)) : 'freshtv420)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv425 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | LAND | REENTRANCE | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv421 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 99 "parser.mly"
                                                    ( fun(l,r)-> EpLAnd(l,r)                                    )
# 2617 "parser.ml"
             in
            
# 106 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2622 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv422)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv423 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv424)) : 'freshtv426)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv431 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | NEQ | REENTRANCE | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv427 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 96 "parser.mly"
                                                    ( fun(l,r)-> EpGT(l,r)                                      )
# 2655 "parser.ml"
             in
            
# 106 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2660 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv428)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv429 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv430)) : 'freshtv432)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv437 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQEQ | GT | LAND | LT | NEQ | REENTRANCE | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv433 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 97 "parser.mly"
                                                    ( fun(l,r)-> EpEq(l,r)                                      )
# 2693 "parser.ml"
             in
            
# 106 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2698 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv434)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv435 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv436)) : 'freshtv438)
    | MenhirState148 | MenhirState71 | MenhirState74 | MenhirState118 | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv445 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv439 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | DECLIT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | DECLIT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | ID _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118) : 'freshtv440)
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv441 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2779 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv443 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv444)) : 'freshtv446)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv453 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv449 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv447 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = 
# 111 "parser.mly"
                                                    ( EpBalance _3,                                                     ()  )
# 2825 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv448)) : 'freshtv450)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv451 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv452)) : 'freshtv454)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv461 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv457 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv455 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = 
# 108 "parser.mly"
                                                    ( EpParen _2,                                                       ()  )
# 2871 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv456)) : 'freshtv458)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv459 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv460)) : 'freshtv462)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv467 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | REENTRANCE | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv463 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = 
# 107 "parser.mly"
                                                    ( EpNot _2,                                                         ()  )
# 2914 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv464)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv465 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv466)) : 'freshtv468)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv475 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv471 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv469 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 85 "parser.mly"
                                                    ( SmExpr _3                                                 )
# 2960 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv470)) : 'freshtv472)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv473 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv474)) : 'freshtv476)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv483 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv479 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv477 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 89 "parser.mly"
                                                    ( SmSlfDstrct _2                                            )
# 3006 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv478)) : 'freshtv480)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv481 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv482)) : 'freshtv484)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv491 * _menhir_state) * _menhir_state * 'tv_option_expr_))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv487 * _menhir_state) * _menhir_state * 'tv_option_expr_))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv485 * _menhir_state) * _menhir_state * 'tv_option_expr_))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_option_expr_)), _, (_5 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 82 "parser.mly"
                                                    ( SmReturn{ret_expr=_2; ret_cont=_5}                        )
# 3052 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv486)) : 'freshtv488)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv489 * _menhir_state) * _menhir_state * 'tv_option_expr_))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv490)) : 'freshtv492)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv497 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv493 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_option_expr_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3095 "parser.ml"
             in
            _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv495 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv496)) : 'freshtv498)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv503 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv499 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | ADDRESS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | DECLIT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | DECLIT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | FALSE ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | ID _v ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | IF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LBRACE ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LOG ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | RETURN ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | SELFDESTRUCT ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | VOID ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154) : 'freshtv500)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv501 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv502)) : 'freshtv504)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv511 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 3203 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv507 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 3233 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv505 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 3240 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), (_2 : (
# 6 "parser.mly"
       (string)
# 3245 "parser.ml"
            ))), _, (_4 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 84 "parser.mly"
                                                    ( SmDecl{declTy=_1; declId=_2; declVal=_4}   )
# 3250 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv509 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 3260 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv510)) : 'freshtv512)
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv519 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv515 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv513 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_lexpr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 83 "parser.mly"
                                                    ( SmAssign(_1,_3)                                           )
# 3300 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv514)) : 'freshtv516)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv517 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv518)) : 'freshtv520)
    | MenhirState51 | MenhirState177 | MenhirState174 | MenhirState154 | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv523 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv521 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv522)) : 'freshtv524)
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
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | DECLIT8 _v ->
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
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_evnt_arg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_evnt_arg_) : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ = 
# 144 "<standard.mly>"
    ( x )
# 3399 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv352)) : 'freshtv354)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv357 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv355 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_evnt_arg_) : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_evnt_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_evnt_arg_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3415 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv356)) : 'freshtv358)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_arg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState45 | MenhirState36 | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv345) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv343) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_arg_) : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 144 "<standard.mly>"
    ( x )
# 3436 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv344)) : 'freshtv346)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv349 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv347 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_arg_) : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_arg_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3452 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv348)) : 'freshtv350)
    | _ ->
        _menhir_fail ()

and _menhir_goto_mthd_head : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_mthd_head -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv341 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv342)

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_mthd_ = 
# 211 "<standard.mly>"
    ( [] )
# 3478 "parser.ml"
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
        let (_menhir_stack : 'freshtv337 * _menhir_state) = Obj.magic _menhir_stack in
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
        | UINT256 ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | UINT8 ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | VOID ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv335 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState33 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv331 * _menhir_state)) * _menhir_state) = Obj.magic _menhir_stack in
                let (_v : (
# 6 "parser.mly"
       (string)
# 3520 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv327 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 3531 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ADDRESS ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                    | BOOL ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                    | BYTES32 ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                    | ID _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
                    | UINT256 ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                    | UINT8 ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                    | RPAR ->
                        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv328)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv329 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 3561 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv330)) : 'freshtv332)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv333 * _menhir_state)) * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv334)) : 'freshtv336)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv338)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv339 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv325) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_mthd_head = 
# 56 "parser.mly"
                                                    ( Default                                                   )
# 3593 "parser.ml"
     in
    _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MSG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv317 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv313 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv311 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_expr = 
# 109 "parser.mly"
                                                    ( EpValue,                                                          ()  )
# 3625 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)) : 'freshtv314)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv315 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv316)) : 'freshtv318)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv319 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv309) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 102 "parser.mly"
                                                    ( EpTrue,                                                           ()  )
# 3659 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv307) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 119 "parser.mly"
                                                    ( EpThis,                                                           ()  )
# 3672 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv303 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MSG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv299 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv295 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv293 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_expr = 
# 110 "parser.mly"
                                                    ( EpSender,                                                         ()  )
# 3704 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)) : 'freshtv296)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv297 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv298)) : 'freshtv300)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv301 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)) : 'freshtv304)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BLOCK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv285 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv281 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv279 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_expr = 
# 112 "parser.mly"
                                                    ( EpNow,                                                            ()  )
# 3757 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv280)) : 'freshtv282)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv283 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)) : 'freshtv286)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv287 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)) : 'freshtv290)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv292)

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
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | DECLIT8 _v ->
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
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState68
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
        let (_menhir_stack : 'freshtv275 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 6 "parser.mly"
       (string)
# 3833 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv271 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 3844 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | DECLIT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | DECLIT8 _v ->
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
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | RPAR ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv272)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv273 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 3890 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)

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
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | DECLIT8 _v ->
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
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (string)
# 3944 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | REENTRANCE | RPAR | RSQBR | SEMI | THEN ->
        _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv269 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 3962 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv270)

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv267) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 103 "parser.mly"
                                                    ( EpFalse,                                                          ()  )
# 3976 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv268)

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (Big_int.big_int)
# 3983 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv265) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 8 "parser.mly"
       (Big_int.big_int)
# 3993 "parser.ml"
    )) : (
# 8 "parser.mly"
       (Big_int.big_int)
# 3997 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 105 "parser.mly"
                                                    ( EpDecLit8 _1,                                                     ()  )
# 4002 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (Big_int.big_int)
# 4009 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv263) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 7 "parser.mly"
       (Big_int.big_int)
# 4019 "parser.ml"
    )) : (
# 7 "parser.mly"
       (Big_int.big_int)
# 4023 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 104 "parser.mly"
                                                    ( EpDecLit256 _1,                                                   ()  )
# 4028 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv264)

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv259 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | BALANCE ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DECLIT256 _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | DECLIT8 _v ->
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
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | THIS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TRUE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | VALUE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv260)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv261 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)

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
        let (_menhir_stack : 'freshtv257 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)

and _menhir_goto_evnt_arg : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_evnt_arg -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv250)
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_evnt_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_evnt_arg_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4137 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)

and _menhir_run13 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_ty -> (
# 6 "parser.mly"
       (string)
# 4151 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
    let ((_2 : (
# 6 "parser.mly"
       (string)
# 4160 "parser.ml"
    )) : (
# 6 "parser.mly"
       (string)
# 4164 "parser.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s, (_1 : 'tv_ty)) = _menhir_stack in
    let _v : 'tv_arg = 
# 61 "parser.mly"
                                                    ( {ty=_1; id=_2}                                            )
# 4170 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv245) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_arg) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv233 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_arg)) = _menhir_stack in
        let _v : 'tv_evnt_arg = 
# 64 "parser.mly"
                                                    ( evnt_arg_of_arg _1 false                                  )
# 4187 "parser.ml"
         in
        _menhir_goto_evnt_arg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)) : 'freshtv236)
    | MenhirState26 | MenhirState45 | MenhirState41 | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv237 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | BOOL ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | BYTES32 ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | ID _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv238)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_arg)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_arg_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4225 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv240)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)) : 'freshtv244)
    | _ ->
        _menhir_fail ()) : 'freshtv246)) : 'freshtv248)

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
    let (_menhir_stack : 'freshtv231 * _menhir_state * 'tv_cntrct) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONTRACT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState189
    | EVENT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState189
    | EOF ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState189
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189) : 'freshtv232)

and _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv205 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4293 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv201 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4303 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv197 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4313 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | DEFAULT ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | METHOD ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | RBRACE ->
                    _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv198)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv199 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4335 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv203 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4346 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv217 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4355 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv213 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4365 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv209 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4375 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv207 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4382 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _), (_4 : (
# 6 "parser.mly"
       (string)
# 4387 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
                let _v : 'tv_mthd_head = let _5 =
                  let _1 =
                    let x = 
# 232 "<standard.mly>"
    ( xs )
# 4394 "parser.ml"
                     in
                    
# 200 "<standard.mly>"
    ( x )
# 4399 "parser.ml"
                    
                  in
                  
# 40 "parser.mly"
                                                    ( _1                                                        )
# 4405 "parser.ml"
                  
                in
                
# 58 "parser.mly"
                                                    ( Method{mthd_retTy=TyTuple[];mthd_name=_4; mthd_args=_5}   )
# 4411 "parser.ml"
                 in
                _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv211 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4421 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv215 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4432 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)) : 'freshtv218)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv229 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4441 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv225 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4451 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv221 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4461 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv219 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4468 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (_3 : 'tv_ty)), (_4 : (
# 6 "parser.mly"
       (string)
# 4473 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
                let _v : 'tv_mthd_head = let _5 =
                  let _1 =
                    let x = 
# 232 "<standard.mly>"
    ( xs )
# 4480 "parser.ml"
                     in
                    
# 200 "<standard.mly>"
    ( x )
# 4485 "parser.ml"
                    
                  in
                  
# 40 "parser.mly"
                                                    ( _1                                                        )
# 4491 "parser.ml"
                  
                in
                
# 57 "parser.mly"
                                                    ( Method{mthd_retTy=_3;       mthd_name=_4; mthd_args=_5}   )
# 4497 "parser.ml"
                 in
                _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)) : 'freshtv222)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv223 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4507 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)) : 'freshtv226)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv227 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4518 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)
    | _ ->
        _menhir_fail ()

and _menhir_reduce80 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 6 "parser.mly"
       (string)
# 4528 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 4534 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_ty = 
# 74 "parser.mly"
                                                    ( TyInstnce _1                                              )
# 4539 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _v
        | INDEXED ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv161 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv157 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
                let (_v : (
# 6 "parser.mly"
       (string)
# 4569 "parser.ml"
                )) = _v in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv155 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
                let ((_3 : (
# 6 "parser.mly"
       (string)
# 4577 "parser.ml"
                )) : (
# 6 "parser.mly"
       (string)
# 4581 "parser.ml"
                )) = _v in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_ty)) = _menhir_stack in
                let _v : 'tv_evnt_arg = 
# 65 "parser.mly"
                                                    ( {arg={ty=_1; id=_3}; indexed=true}                        )
# 4587 "parser.ml"
                 in
                _menhir_goto_evnt_arg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)) : 'freshtv158)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv159 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)) : 'freshtv166)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv171 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _ | INDEXED ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv167 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), _, (_3 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_ty = 
# 73 "parser.mly"
                                                    ( TyMap(_1,_3)                                              )
# 4619 "parser.ml"
             in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv168)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv169 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)) : 'freshtv172)
    | MenhirState45 | MenhirState41 | MenhirState36 | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
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
            let (_menhir_stack : 'freshtv173 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv185 * _menhir_state)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv181 * _menhir_state)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 6 "parser.mly"
       (string)
# 4660 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv177 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4671 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | BOOL ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | BYTES32 ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | ID _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
                | UINT256 ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | UINT8 ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | RPAR ->
                    _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv178)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv179 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4701 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)) : 'freshtv182)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv183 * _menhir_state)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)) : 'freshtv186)
    | MenhirState177 | MenhirState51 | MenhirState174 | MenhirState165 | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv191 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 6 "parser.mly"
       (string)
# 4726 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv187 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4737 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | BALANCE ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | DECLIT256 _v ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
                | DECLIT8 _v ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
                | FALSE ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | ID _v ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
                | LPAR ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | NEW ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | NOT ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | NOW ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | SENDER ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | THIS ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | TRUE ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | VALUE ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161) : 'freshtv188)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv189 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4781 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)) : 'freshtv192)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv193 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)) : 'freshtv196)
    | _ ->
        _menhir_fail ()

and _menhir_reduce77 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : 'tv_ty = 
# 71 "parser.mly"
                                                    ( TyAddr                                                    )
# 4801 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_cntrct_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_cntrct_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv145 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv143 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_list_cntrct_)) = _menhir_stack in
            let _v : (
# 36 "parser.mly"
       (unit Syntax.toplevel list)
# 4824 "parser.ml"
            ) = 
# 43 "parser.mly"
                                                    ( _1                                                        )
# 4828 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv141) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 36 "parser.mly"
       (unit Syntax.toplevel list)
# 4836 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 36 "parser.mly"
       (unit Syntax.toplevel list)
# 4844 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv137) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 36 "parser.mly"
       (unit Syntax.toplevel list)
# 4852 "parser.ml"
            )) : (
# 36 "parser.mly"
       (unit Syntax.toplevel list)
# 4856 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv138)) : 'freshtv140)) : 'freshtv142)) : 'freshtv144)) : 'freshtv146)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv147 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)) : 'freshtv150)
    | MenhirState189 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv153 * _menhir_state * 'tv_cntrct) * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state * 'tv_cntrct) * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_cntrct)), _, (xs : 'tv_list_cntrct_)) = _menhir_stack in
        let _v : 'tv_list_cntrct_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 4875 "parser.ml"
         in
        _menhir_goto_list_cntrct_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv152)) : 'freshtv154)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv135 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4888 "parser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv131 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4898 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv127 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4908 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv125 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4915 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (_2 : (
# 6 "parser.mly"
       (string)
# 4920 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = _menhir_stack in
            let _v : 'tv_cntrct = let _3 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 4927 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 4932 "parser.ml"
                
              in
              
# 40 "parser.mly"
                                                    ( _1                                                        )
# 4938 "parser.ml"
              
            in
            
# 47 "parser.mly"
                                                    ( Event {            evnt_name=_2;   evnt_args=_3}          )
# 4944 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv126)) : 'freshtv128)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv129 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4954 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv133 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4965 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)) : 'freshtv136)

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 142 "<standard.mly>"
    ( [] )
# 4975 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 69 "parser.mly"
                                                    ( TyUint8                                                   )
# 4988 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv124)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 68 "parser.mly"
                                                    ( TyUint256                                                 )
# 5001 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (string)
# 5008 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 70 "parser.mly"
                                                    ( TyBytes32                                                 )
# 5024 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv120)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 72 "parser.mly"
                                                    ( TyBool                                                    )
# 5037 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv118)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState189 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_cntrct) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_mthd) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState177 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState174 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv27 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv29 * _menhir_state * 'tv_lexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv31 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv33 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 5085 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv35 * _menhir_state) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv39 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5104 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv41 * _menhir_state) * _menhir_state * 'tv_option_expr_))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv47 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5128 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv49 * _menhir_state * 'tv_expr))))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv54)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv65 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 5176 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv67 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 5185 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 5229 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv87 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5243 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv97 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 5272 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv101 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5286 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv105 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5300 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv107 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5309 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv109 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5328 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv116)

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_cntrct_ = 
# 211 "<standard.mly>"
    ( [] )
# 5342 "parser.ml"
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
# 6 "parser.mly"
       (string)
# 5358 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5369 "parser.ml"
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
# 5393 "parser.ml"
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
# 6 "parser.mly"
       (string)
# 5407 "parser.ml"
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
# 6 "parser.mly"
       (string)
# 5431 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv3 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5442 "parser.ml"
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
# 6 "parser.mly"
       (string)
# 5472 "parser.ml"
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
# 36 "parser.mly"
       (unit Syntax.toplevel list)
# 5499 "parser.ml"
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
  

# 5527 "parser.ml"
