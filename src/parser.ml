
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
    | RARROW
    | PLUS
    | NOW
    | NOT
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
    | IDENT of (
# 8 "parser.mly"
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
    | DEPLOY
    | DEFAULT
    | DECLIT8 of (
# 10 "parser.mly"
       (Big_int.big_int)
# 58 "parser.ml"
  )
    | DECLIT256 of (
# 9 "parser.mly"
       (Big_int.big_int)
# 63 "parser.ml"
  )
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
  | MenhirState184
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
  | MenhirState127
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
  | MenhirState75
  | MenhirState71
  | MenhirState69
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
  | MenhirState11
  | MenhirState3
  | MenhirState0

# 2 "parser.mly"
  
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
            ((let ((_menhir_stack, _menhir_s), _, (scs : 'tv_list_stmt_)) = _menhir_stack in
            let _v : 'tv_block = 
# 72 "parser.mly"
                                      ( scs )
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
# 156 "parser.mly"
                                                ( _2 )
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
# 149 "parser.mly"
                                                ( {msg_value=_1; msg_reentrance=_2} )
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
# 8 "parser.mly"
       (string)
# 222 "parser.ml"
                    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = _menhir_s in
                    let (_v : 'tv_msg) = _v in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((('freshtv657 * _menhir_state * 'tv_expr)) * (
# 8 "parser.mly"
       (string)
# 230 "parser.ml"
                    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                    let (_ : _menhir_state) = _menhir_s in
                    let ((_5 : 'tv_msg) : 'tv_msg) = _v in
                    ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), (_3 : (
# 8 "parser.mly"
       (string)
# 237 "parser.ml"
                    ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
                    let _v : 'tv_expr = let _4 =
                      let _1 =
                        let xs =
                          let x = 
# 232 "<standard.mly>"
    ( xs )
# 245 "parser.ml"
                           in
                          
# 200 "<standard.mly>"
    ( x )
# 250 "parser.ml"
                          
                        in
                        
# 53 "parser.mly"
                                                        (xs)
# 256 "parser.ml"
                        
                      in
                      
# 147 "parser.mly"
                                                ( _1 )
# 262 "parser.ml"
                      
                    in
                    
# 140 "parser.mly"
                                           ( EpSend{send_cntrct=_1; send_mthd=Some _3; send_args=_4; send_msg=_5},() )
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
# 139 "parser.mly"
                                           ( EpSend{send_cntrct=_1; send_mthd=None   ; send_args=[]; send_msg=_6},() )
# 284 "parser.ml"
                     in
                    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv662)) : 'freshtv664)
                | MenhirState127 ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((('freshtv667 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 292 "parser.ml"
                    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = _menhir_s in
                    let (_v : 'tv_msg) = _v in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (((('freshtv665 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 300 "parser.ml"
                    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                    let (_ : _menhir_state) = _menhir_s in
                    let ((_4 : 'tv_msg) : 'tv_msg) = _v in
                    ((let (((_menhir_stack, _menhir_s), (_2 : (
# 8 "parser.mly"
       (string)
# 307 "parser.ml"
                    ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
                    let _v : 'tv_expr = let _3 =
                      let _1 =
                        let xs =
                          let x = 
# 232 "<standard.mly>"
    ( xs )
# 315 "parser.ml"
                           in
                          
# 200 "<standard.mly>"
    ( x )
# 320 "parser.ml"
                          
                        in
                        
# 53 "parser.mly"
                                                        (xs)
# 326 "parser.ml"
                        
                      in
                      
# 147 "parser.mly"
                                                ( _1 )
# 332 "parser.ml"
                      
                    in
                    
# 138 "parser.mly"
                                           ( EpNew{new_head=_2; new_args=_3; new_msg=_4},() )
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
                  
# 100 "parser.mly"
                                                ( _1   )
# 354 "parser.ml"
                  
                in
                let _4 =
                  let _1 = _1_inlined1 in
                  
# 99 "parser.mly"
                                                ( [_1] )
# 362 "parser.ml"
                  
                in
                
# 108 "parser.mly"
                                                ( SmIf(_2,_4,_6) )
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
                    | DEPLOY ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | FALSE ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | IDENT _v ->
                        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
                    | IF ->
                        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | LBRACE ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | LOG ->
                        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState174
                    | LPAR ->
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
                | ABORT | ADDRESS | BALANCE | BOOL | BYTES32 | DECLIT256 _ | DECLIT8 _ | DEPLOY | FALSE | IDENT _ | IF | LOG | LPAR | NOT | NOW | RBRACE | RETURN | SELFDESTRUCT | SENDER | THIS | TRUE | UINT256 | UINT8 | VALUE | VOID ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv687 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_block)) = _menhir_stack in
                    let _v : 'tv_stmt = let _4 =
                      let _1 = _1_inlined1 in
                      
# 100 "parser.mly"
                                                ( _1   )
# 446 "parser.ml"
                      
                    in
                    
# 109 "parser.mly"
                                                ( SmIfThen (_2, _4) )
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
                  
# 100 "parser.mly"
                                                ( _1   )
# 473 "parser.ml"
                  
                in
                let _4 =
                  let _1 = _1_inlined1 in
                  
# 100 "parser.mly"
                                                ( _1   )
# 481 "parser.ml"
                  
                in
                
# 108 "parser.mly"
                                                ( SmIf(_2,_4,_6) )
# 487 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv694)) : 'freshtv696)
            | MenhirState50 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv703 * _menhir_state * 'tv_mthd_head) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv701 * _menhir_state * 'tv_mthd_head) * _menhir_state * 'tv_block) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_mthd_head)), _, (_2 : 'tv_block)) = _menhir_stack in
                let _v : 'tv_case = 
# 67 "parser.mly"
                              (   { mthd_head    = _1
                                  ; mthd_body    = _2 }   )
# 500 "parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv699) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_case) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv697 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | DEFAULT ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | METHOD ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | RBRACE ->
                    _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184) : 'freshtv698)) : 'freshtv700)) : 'freshtv702)) : 'freshtv704)
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
# 152 "parser.mly"
                                                ( None )
# 539 "parser.ml"
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | LPAR ->
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

and _menhir_reduce40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_stmt_ = 
# 211 "<standard.mly>"
    ( [] )
# 587 "parser.ml"
     in
    _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv625 * _menhir_state * 'tv_expr)) * (
# 8 "parser.mly"
       (string)
# 600 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv621 * _menhir_state * 'tv_expr)) * (
# 8 "parser.mly"
       (string)
# 610 "parser.ml"
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
# 8 "parser.mly"
       (string)
# 630 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv624)) : 'freshtv626)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv631 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 639 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv627 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 649 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ALONG ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | REENTRANCE ->
                _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv628)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv629 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 669 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv630)) : 'freshtv632)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv639 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 678 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv635 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 688 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv633 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 695 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "parser.mly"
       (string)
# 700 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _v : 'tv_expr = let _2 =
              let _1 =
                let xs =
                  let x = 
# 232 "<standard.mly>"
    ( xs )
# 708 "parser.ml"
                   in
                  
# 200 "<standard.mly>"
    ( x )
# 713 "parser.ml"
                  
                in
                
# 53 "parser.mly"
                                                        (xs)
# 719 "parser.ml"
                
              in
              
# 147 "parser.mly"
                                                ( _1 )
# 725 "parser.ml"
              
            in
            
# 137 "parser.mly"
                                                ( EpFnCall {call_head=_1; call_args=_2 }, () )
# 731 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv634)) : 'freshtv636)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv637 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 741 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv638)) : 'freshtv640)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv651 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 750 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv647 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 760 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv643 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 770 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv641 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 777 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s), (_2 : (
# 8 "parser.mly"
       (string)
# 782 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
                let _v : 'tv_stmt = let _3 =
                  let _1 =
                    let xs =
                      let x = 
# 232 "<standard.mly>"
    ( xs )
# 790 "parser.ml"
                       in
                      
# 200 "<standard.mly>"
    ( x )
# 795 "parser.ml"
                      
                    in
                    
# 53 "parser.mly"
                                                        (xs)
# 801 "parser.ml"
                    
                  in
                  
# 147 "parser.mly"
                                                ( _1 )
# 807 "parser.ml"
                  
                in
                
# 110 "parser.mly"
                                                ( SmLog(_2,_3,None))
# 813 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv642)) : 'freshtv644)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv645 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 823 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv646)) : 'freshtv648)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv649 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 834 "parser.ml"
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
        | DEPLOY ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | FALSE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | IDENT _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | LPAR ->
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | LPAR ->
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | LPAR ->
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
# 973 "parser.ml"
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
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv611 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "parser.mly"
       (string)
# 993 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv607 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 1004 "parser.ml"
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
            | DEPLOY ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | IDENT _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | LPAR ->
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
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148) : 'freshtv608)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv609 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 1050 "parser.ml"
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
    | LPAR ->
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
# 8 "parser.mly"
       (string)
# 1104 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
    | IDENT _ | RARROW ->
        _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack)
    | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS ->
        _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv605 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 1124 "parser.ml"
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
    | IDENT _ | RARROW ->
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
# 103 "parser.mly"
                                                ( SmAbort )
# 1163 "parser.ml"
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
            | DEPLOY ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | IDENT _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | LPAR ->
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
            | DEPLOY ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | IDENT _v ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | IF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | LBRACE ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | LOG ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | LPAR ->
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
        | ABORT | ADDRESS | BALANCE | BOOL | BYTES32 | DECLIT256 _ | DECLIT8 _ | DEPLOY | FALSE | IDENT _ | IF | LOG | LPAR | NOT | NOW | RBRACE | RETURN | SELFDESTRUCT | SENDER | THIS | TRUE | UINT256 | UINT8 | VALUE | VOID ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv571 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_1_inlined1 : 'tv_stmt)) = _menhir_stack in
            let _v : 'tv_stmt = let _4 =
              let _1 = _1_inlined1 in
              
# 99 "parser.mly"
                                                ( [_1] )
# 1320 "parser.ml"
              
            in
            
# 109 "parser.mly"
                                                ( SmIfThen (_2, _4) )
# 1326 "parser.ml"
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
          
# 99 "parser.mly"
                                                ( [_1] )
# 1347 "parser.ml"
          
        in
        let _4 =
          let _1 = _1_inlined1 in
          
# 99 "parser.mly"
                                                ( [_1] )
# 1355 "parser.ml"
          
        in
        
# 108 "parser.mly"
                                                ( SmIf(_2,_4,_6) )
# 1361 "parser.ml"
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
          
# 99 "parser.mly"
                                                ( [_1] )
# 1375 "parser.ml"
          
        in
        let _4 =
          let _1 = _1_inlined1 in
          
# 100 "parser.mly"
                                                ( _1   )
# 1383 "parser.ml"
          
        in
        
# 108 "parser.mly"
                                                ( SmIf(_2,_4,_6) )
# 1389 "parser.ml"
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
        | DEPLOY ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | FALSE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | IDENT _v ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
        | IF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | LOG ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | LPAR ->
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
            _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177) : 'freshtv586)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState148 | MenhirState71 | MenhirState75 | MenhirState98 ->
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
# 1470 "parser.ml"
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
# 1486 "parser.ml"
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

and _menhir_reduce33 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_lexpr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : 'tv_lexpr)) = _menhir_stack in
    let _v : 'tv_expr = 
# 144 "parser.mly"
                                                ( EpArray _1, () )
# 1526 "parser.ml"
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LPAR ->
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | LPAR ->
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | LPAR ->
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | LPAR ->
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | LPAR ->
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | LPAR ->
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | LPAR ->
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState108
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
    | LPAR ->
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | LPAR ->
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
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv551 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "parser.mly"
       (string)
# 1923 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv547 * _menhir_state * 'tv_expr)) * (
# 8 "parser.mly"
       (string)
# 1934 "parser.ml"
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
            | DEPLOY ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | IDENT _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | LPAR ->
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
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv548)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv549 * _menhir_state * 'tv_expr)) * (
# 8 "parser.mly"
       (string)
# 1980 "parser.ml"
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENT _v ->
        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LOG ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LPAR ->
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
        _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_goto_list_case_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_case_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv531 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2062 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_case_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv527 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2072 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_case_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv525 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2079 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_case_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (_2 : (
# 8 "parser.mly"
       (string)
# 2084 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)), _, (_5 : 'tv_list_case_)) = _menhir_stack in
            let _v : 'tv_cntrct = let _3 =
              let xs =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 2091 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 2096 "parser.ml"
                
              in
              
# 53 "parser.mly"
                                                        (xs)
# 2102 "parser.ml"
              
            in
            
# 61 "parser.mly"
                                        ( Cntrct { mthds       = _5
                                                   ; cntrct_name = _2
                                                   ; cntrct_args = _3 } )
# 2110 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv526)) : 'freshtv528)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv529 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 2120 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_case_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv530)) : 'freshtv532)
    | MenhirState184 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv535 * _menhir_state * 'tv_case) * _menhir_state * 'tv_list_case_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv533 * _menhir_state * 'tv_case) * _menhir_state * 'tv_list_case_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_case)), _, (xs : 'tv_list_case_)) = _menhir_stack in
        let _v : 'tv_list_case_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2133 "parser.ml"
         in
        _menhir_goto_list_case_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv534)) : 'freshtv536)
    | _ ->
        _menhir_fail ()

and _menhir_reduce24 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "parser.mly"
       (string)
# 2142 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 8 "parser.mly"
       (string)
# 2148 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_expr = 
# 135 "parser.mly"
                                                ( EpIdent _1, () )
# 2153 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "parser.mly"
       (string)
# 2160 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | LPAR ->
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
        _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_reduce46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 142 "<standard.mly>"
    ( [] )
# 2206 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

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
# 141 "parser.mly"
                                                ( EpAddr _3, () )
# 2250 "parser.ml"
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
# 115 "parser.mly"
                                                ( fun (l,r) -> EpPlus(l, r))
# 2279 "parser.ml"
             in
            
# 134 "parser.mly"
                                                ( (_2(_1,_3)), () )
# 2284 "parser.ml"
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
# 117 "parser.mly"
                                                ( fun (l,r) -> EpMult(l, r))
# 2311 "parser.ml"
             in
            
# 134 "parser.mly"
                                                ( (_2(_1,_3)), () )
# 2316 "parser.ml"
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
# 159 "parser.mly"
                                                ( LEpArray{arrIdent=_1; arrIndex=_3} )
# 2362 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv387) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_lexpr) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState168 | MenhirState161 | MenhirState152 | MenhirState148 | MenhirState139 | MenhirState142 | MenhirState136 | MenhirState53 | MenhirState68 | MenhirState69 | MenhirState71 | MenhirState75 | MenhirState79 | MenhirState118 | MenhirState98 | MenhirState110 | MenhirState108 | MenhirState106 | MenhirState104 | MenhirState102 | MenhirState94 | MenhirState92 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState81 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv379 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                (_menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) : 'freshtv380)
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
                    | DEPLOY ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | FALSE ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                    | IDENT _v ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                    | LPAR ->
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
                    _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
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
# 121 "parser.mly"
                                                ( fun (l,r) -> EpNeq(l, r))
# 2459 "parser.ml"
             in
            
# 134 "parser.mly"
                                                ( (_2(_1,_3)), () )
# 2464 "parser.ml"
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
# 116 "parser.mly"
                                                ( fun (l,r) -> EpMinus(l, r))
# 2493 "parser.ml"
             in
            
# 134 "parser.mly"
                                                ( (_2(_1,_3)), () )
# 2498 "parser.ml"
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
# 153 "parser.mly"
                                                ( Some _2 )
# 2541 "parser.ml"
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
# 118 "parser.mly"
                                                ( fun (l,r) -> EpLt(l, r))
# 2574 "parser.ml"
             in
            
# 134 "parser.mly"
                                                ( (_2(_1,_3)), () )
# 2579 "parser.ml"
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
            let _v : 'tv_expr = 
# 125 "parser.mly"
                                                ( EpLand(_1,_3), () )
# 2620 "parser.ml"
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
# 119 "parser.mly"
                                                ( fun (l,r) -> EpGt(l, r))
# 2653 "parser.ml"
             in
            
# 134 "parser.mly"
                                                ( (_2(_1,_3)), () )
# 2658 "parser.ml"
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
# 120 "parser.mly"
                                                ( fun (l,r) -> EpEq(l, r))
# 2691 "parser.ml"
             in
            
# 134 "parser.mly"
                                                ( (_2(_1,_3)), () )
# 2696 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv434)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv435 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv436)) : 'freshtv438)
    | MenhirState148 | MenhirState71 | MenhirState75 | MenhirState118 | MenhirState98 ->
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
            | DEPLOY ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | IDENT _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | LPAR ->
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
# 2777 "parser.ml"
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
# 132 "parser.mly"
                                                ( EpBalance _3, () )
# 2823 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv448)) : 'freshtv450)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv451 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv452)) : 'freshtv454)
    | MenhirState69 ->
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
# 136 "parser.mly"
                                                ( EpParen _2, () )
# 2869 "parser.ml"
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
# 142 "parser.mly"
                                                ( EpNot _2, () )
# 2912 "parser.ml"
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
# 107 "parser.mly"
                                                ( SmExpr _3 )
# 2958 "parser.ml"
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
# 111 "parser.mly"
                                                ( SmSelfDestruct _2 )
# 3004 "parser.ml"
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
# 104 "parser.mly"
                                                ( SmReturn{ret_expr=_2; ret_cont=_5} )
# 3050 "parser.ml"
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
# 3093 "parser.ml"
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
            | DEPLOY ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | IDENT _v ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | IF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LBRACE ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LOG ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LPAR ->
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
# 8 "parser.mly"
       (string)
# 3201 "parser.ml"
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
# 8 "parser.mly"
       (string)
# 3231 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv505 * _menhir_state * 'tv_ty) * (
# 8 "parser.mly"
       (string)
# 3238 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), (_2 : (
# 8 "parser.mly"
       (string)
# 3243 "parser.ml"
            ))), _, (_4 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 106 "parser.mly"
                                                ( SmVarDecl{ varDecl_ty=_1; varDecl_id=_2; varDecl_val=_4 } )
# 3248 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv509 * _menhir_state * 'tv_ty) * (
# 8 "parser.mly"
       (string)
# 3258 "parser.ml"
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
# 105 "parser.mly"
                                                ( SmAssign(_1,_3) )
# 3298 "parser.ml"
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LPAR ->
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
# 3397 "parser.ml"
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
# 3413 "parser.ml"
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
# 3434 "parser.ml"
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
# 3450 "parser.ml"
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

and _menhir_reduce36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_case_ = 
# 211 "<standard.mly>"
    ( [] )
# 3476 "parser.ml"
     in
    _menhir_goto_list_case_ _menhir_env _menhir_stack _menhir_s _v

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
        | IDENT _v ->
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
            | IDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv331 * _menhir_state)) * _menhir_state) = Obj.magic _menhir_stack in
                let (_v : (
# 8 "parser.mly"
       (string)
# 3518 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv327 * _menhir_state)) * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 3529 "parser.ml"
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
                    | IDENT _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
                    | UINT256 ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                    | UINT8 ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                    | RPAR ->
                        _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv328)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv329 * _menhir_state)) * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 3559 "parser.ml"
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
# 76 "parser.mly"
                                            ( Default )
# 3591 "parser.ml"
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
# 130 "parser.mly"
                                                ( EpValue, () )
# 3623 "parser.ml"
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
# 126 "parser.mly"
                                                ( EpTrue, () )
# 3657 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv307) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 143 "parser.mly"
                                                ( EpThis, () )
# 3670 "parser.ml"
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
# 131 "parser.mly"
                                                ( EpSender, () )
# 3702 "parser.ml"
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
# 133 "parser.mly"
                                                ( EpNow, () )
# 3755 "parser.ml"
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
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LPAR ->
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
    | ADDRESS ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | BALANCE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | DECLIT256 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | DECLIT8 _v ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | DEPLOY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IDENT _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LPAR ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (string)
# 3861 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | REENTRANCE | RPAR | RSQBR | SEMI | THEN ->
        _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 3879 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv275) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 127 "parser.mly"
                                                ( EpFalse, () )
# 3893 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv276)

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv271 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "parser.mly"
       (string)
# 3909 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv267 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 3920 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | BALANCE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | DECLIT256 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | DECLIT8 _v ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | DEPLOY ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | IDENT _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | LPAR ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | RPAR ->
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv268)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv269 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 3966 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv270)) : 'freshtv272)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv273 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "parser.mly"
       (Big_int.big_int)
# 3981 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv265) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 10 "parser.mly"
       (Big_int.big_int)
# 3991 "parser.ml"
    )) : (
# 10 "parser.mly"
       (Big_int.big_int)
# 3995 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 129 "parser.mly"
                                                ( EpDecLit8 _1, ())
# 4000 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Big_int.big_int)
# 4007 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv263) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 9 "parser.mly"
       (Big_int.big_int)
# 4017 "parser.ml"
    )) : (
# 9 "parser.mly"
       (Big_int.big_int)
# 4021 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 128 "parser.mly"
                                                ( EpDecLit256 _1, ())
# 4026 "parser.ml"
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
        | DEPLOY ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | FALSE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | IDENT _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | LPAR ->
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

and _menhir_run11 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_ty -> 'ttv_return =
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
        | IDENT _v ->
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
# 4157 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)

and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_ty -> (
# 8 "parser.mly"
       (string)
# 4171 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
    let ((_2 : (
# 8 "parser.mly"
       (string)
# 4180 "parser.ml"
    )) : (
# 8 "parser.mly"
       (string)
# 4184 "parser.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s, (_1 : 'tv_ty)) = _menhir_stack in
    let _v : 'tv_arg = 
# 81 "parser.mly"
                                               ( { ty = _1; id = _2 } )
# 4190 "parser.ml"
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
# 85 "parser.mly"
                                                ( evnt_arg_of_arg _1 false )
# 4207 "parser.ml"
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
            | IDENT _v ->
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
# 4245 "parser.ml"
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
        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState189
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
# 8 "parser.mly"
       (string)
# 4291 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv201 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4301 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv197 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4311 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | DEFAULT ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | METHOD ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | RBRACE ->
                    _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv198)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv199 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4333 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv203 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4344 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv217 * _menhir_state)) * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4353 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv213 * _menhir_state)) * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4363 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv209 * _menhir_state)) * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4373 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv207 * _menhir_state)) * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4380 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _), (_4 : (
# 8 "parser.mly"
       (string)
# 4385 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
                let _v : 'tv_mthd_head = let _5 =
                  let xs =
                    let x = 
# 232 "<standard.mly>"
    ( xs )
# 4392 "parser.ml"
                     in
                    
# 200 "<standard.mly>"
    ( x )
# 4397 "parser.ml"
                    
                  in
                  
# 53 "parser.mly"
                                                        (xs)
# 4403 "parser.ml"
                  
                in
                
# 78 "parser.mly"
                                            ( Method { mthd_retTy = TyTuple([]); mthd_name = _4; mthd_args = _5 } )
# 4409 "parser.ml"
                 in
                _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv211 * _menhir_state)) * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4419 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv215 * _menhir_state)) * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4430 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)) : 'freshtv218)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv229 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 8 "parser.mly"
       (string)
# 4439 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv225 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 8 "parser.mly"
       (string)
# 4449 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv221 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 8 "parser.mly"
       (string)
# 4459 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv219 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 8 "parser.mly"
       (string)
# 4466 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (_3 : 'tv_ty)), (_4 : (
# 8 "parser.mly"
       (string)
# 4471 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
                let _v : 'tv_mthd_head = let _5 =
                  let xs =
                    let x = 
# 232 "<standard.mly>"
    ( xs )
# 4478 "parser.ml"
                     in
                    
# 200 "<standard.mly>"
    ( x )
# 4483 "parser.ml"
                    
                  in
                  
# 53 "parser.mly"
                                                        (xs)
# 4489 "parser.ml"
                  
                in
                
# 77 "parser.mly"
                                            ( Method { mthd_retTy = _3;     mthd_name = _4; mthd_args = _5 } )
# 4495 "parser.ml"
                 in
                _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)) : 'freshtv222)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv223 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 8 "parser.mly"
       (string)
# 4505 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)) : 'freshtv226)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv227 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 8 "parser.mly"
       (string)
# 4516 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)
    | _ ->
        _menhir_fail ()

and _menhir_reduce80 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "parser.mly"
       (string)
# 4526 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 8 "parser.mly"
       (string)
# 4532 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_ty = 
# 96 "parser.mly"
                                                ( TyInstnce _1 )
# 4537 "parser.ml"
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
        | IDENT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _v
        | INDEXED ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv161 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv157 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
                let (_v : (
# 8 "parser.mly"
       (string)
# 4565 "parser.ml"
                )) = _v in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv155 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
                let ((_3 : (
# 8 "parser.mly"
       (string)
# 4573 "parser.ml"
                )) : (
# 8 "parser.mly"
       (string)
# 4577 "parser.ml"
                )) = _v in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_ty)) = _menhir_stack in
                let _v : 'tv_evnt_arg = 
# 86 "parser.mly"
                                               ( { arg =    { ty  = _1 ; id  = _3 }; 
                                                                        indexed = true } )
# 4584 "parser.ml"
                 in
                _menhir_goto_evnt_arg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)) : 'freshtv158)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv159 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)
        | RARROW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)) : 'freshtv166)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv171 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RARROW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | IDENT _ | INDEXED ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv167 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), _, (_3 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_ty = 
# 95 "parser.mly"
                                                ( TyMap(_1,_3)          )
# 4618 "parser.ml"
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
        | IDENT _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _v
        | RARROW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
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
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv181 * _menhir_state)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 8 "parser.mly"
       (string)
# 4657 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv177 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 8 "parser.mly"
       (string)
# 4668 "parser.ml"
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
                | IDENT _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
                | UINT256 ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | UINT8 ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | RPAR ->
                    _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv178)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv179 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 8 "parser.mly"
       (string)
# 4698 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)) : 'freshtv182)
        | RARROW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
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
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv191 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 8 "parser.mly"
       (string)
# 4723 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv187 * _menhir_state * 'tv_ty) * (
# 8 "parser.mly"
       (string)
# 4734 "parser.ml"
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
                | DEPLOY ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | FALSE ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState161
                | IDENT _v ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
                | LPAR ->
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
# 8 "parser.mly"
       (string)
# 4778 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)) : 'freshtv192)
        | RARROW ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
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
# 93 "parser.mly"
                                                ( TyAddr                )
# 4800 "parser.ml"
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
            ((let (_menhir_stack, _menhir_s, (cs : 'tv_list_cntrct_)) = _menhir_stack in
            let _v : (
# 48 "parser.mly"
       (unit Syntax.toplevel list)
# 4823 "parser.ml"
            ) = 
# 56 "parser.mly"
                              ( cs )
# 4827 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv141) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 48 "parser.mly"
       (unit Syntax.toplevel list)
# 4835 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 48 "parser.mly"
       (unit Syntax.toplevel list)
# 4843 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv137) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 48 "parser.mly"
       (unit Syntax.toplevel list)
# 4851 "parser.ml"
            )) : (
# 48 "parser.mly"
       (unit Syntax.toplevel list)
# 4855 "parser.ml"
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
# 4874 "parser.ml"
         in
        _menhir_goto_list_cntrct_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv152)) : 'freshtv154)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv135 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4887 "parser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv131 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4897 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv127 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4907 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv125 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4914 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (_2 : (
# 8 "parser.mly"
       (string)
# 4919 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = _menhir_stack in
            let _v : 'tv_cntrct = let _3 =
              let xs =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 4926 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 4931 "parser.ml"
                
              in
              
# 53 "parser.mly"
                                                        (xs)
# 4937 "parser.ml"
              
            in
            
# 64 "parser.mly"
                                       ( Event    { evnt_args = _3; evnt_name = _2 } )
# 4943 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv126)) : 'freshtv128)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv129 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4953 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv133 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 4964 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)) : 'freshtv136)

and _menhir_reduce42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 142 "<standard.mly>"
    ( [] )
# 4974 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 91 "parser.mly"
                                                ( TyUint8               )
# 4987 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv124)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 90 "parser.mly"
                                                ( TyUint256             )
# 5000 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (string)
# 5007 "parser.ml"
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
# 92 "parser.mly"
                                                ( TyBytes32             )
# 5023 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv120)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 94 "parser.mly"
                                                ( TyBool                )
# 5036 "parser.ml"
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
    | MenhirState184 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_case) = Obj.magic _menhir_stack in
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
# 8 "parser.mly"
       (string)
# 5084 "parser.ml"
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
# 8 "parser.mly"
       (string)
# 5103 "parser.ml"
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
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv47 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5127 "parser.ml"
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
# 8 "parser.mly"
       (string)
# 5175 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv67 * _menhir_state * 'tv_expr)) * (
# 8 "parser.mly"
       (string)
# 5184 "parser.ml"
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
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv83 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5228 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * (
# 8 "parser.mly"
       (string)
# 5237 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
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
# 8 "parser.mly"
       (string)
# 5271 "parser.ml"
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
# 8 "parser.mly"
       (string)
# 5285 "parser.ml"
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
# 8 "parser.mly"
       (string)
# 5299 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv107 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5308 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv109 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5327 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv116)

and _menhir_reduce38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_cntrct_ = 
# 211 "<standard.mly>"
    ( [] )
# 5341 "parser.ml"
     in
    _menhir_goto_list_cntrct_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "parser.mly"
       (string)
# 5357 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5368 "parser.ml"
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
            | IDENT _v ->
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
# 5392 "parser.ml"
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
# 8 "parser.mly"
       (string)
# 5406 "parser.ml"
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
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "parser.mly"
       (string)
# 5430 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv3 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5441 "parser.ml"
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
            | IDENT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | UINT256 ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | UINT8 ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | RPAR ->
                _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv5 * _menhir_state) * (
# 8 "parser.mly"
       (string)
# 5471 "parser.ml"
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
# 48 "parser.mly"
       (unit Syntax.toplevel list)
# 5498 "parser.ml"
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
        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 5526 "parser.ml"
