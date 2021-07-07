
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
  | MenhirState182
  | MenhirState175
  | MenhirState169
  | MenhirState165
  | MenhirState162
  | MenhirState159
  | MenhirState151
  | MenhirState145
  | MenhirState142
  | MenhirState139
  | MenhirState134
  | MenhirState129
  | MenhirState127
  | MenhirState120
  | MenhirState116
  | MenhirState111
  | MenhirState109
  | MenhirState107
  | MenhirState105
  | MenhirState103
  | MenhirState102
  | MenhirState99
  | MenhirState95
  | MenhirState93
  | MenhirState90
  | MenhirState88
  | MenhirState86
  | MenhirState82
  | MenhirState80
  | MenhirState75
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState68
  | MenhirState53
  | MenhirState51
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

# 142 "parser.ml"

let rec _menhir_reduce75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_value_info = 
# 131 "parser.mly"
                                                    ( EpFalse,()                                                            )
# 149 "parser.ml"
     in
    _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v

and _menhir_run103 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_goto_list_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_stmt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv667 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv665 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_stmt)), _, (xs : 'tv_list_stmt_)) = _menhir_stack in
        let _v : 'tv_list_stmt_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 209 "parser.ml"
         in
        _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv666)) : 'freshtv668)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv685) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv681) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv679) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (_2 : 'tv_list_stmt_)) = _menhir_stack in
            let _v : 'tv_block = 
# 53 "parser.mly"
                                                    ( _2                                                        )
# 228 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv677) = _menhir_stack in
            let (_v : 'tv_block) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv675 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
            let (_v : 'tv_block) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv673 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
            let ((_2 : 'tv_block) : 'tv_block) = _v in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_mthd_head)) = _menhir_stack in
            let _v : 'tv_mthd = 
# 50 "parser.mly"
                                                    ( {mthd_head=_1; mthd_body=_2}                              )
# 243 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv671) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_mthd) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv669 * _menhir_state * 'tv_mthd) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEFAULT ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | METHOD ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | RBRACE ->
                _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175) : 'freshtv670)) : 'freshtv672)) : 'freshtv674)) : 'freshtv676)) : 'freshtv678)) : 'freshtv680)) : 'freshtv682)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv683) * _menhir_state * 'tv_list_stmt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv684)) : 'freshtv686)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv637 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 284 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv633 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 294 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ALONG ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | COMMA | DOT | ELSE | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
                _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv634)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv635 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 314 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv636)) : 'freshtv638)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv645 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 323 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv641 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 333 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv639 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 340 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 345 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _v : 'tv_expr = let _2 =
              let _1 =
                let _1 =
                  let x = 
# 232 "<standard.mly>"
    ( xs )
# 353 "parser.ml"
                   in
                  
# 200 "<standard.mly>"
    ( x )
# 358 "parser.ml"
                  
                in
                
# 40 "parser.mly"
                                                    ( _1                                                        )
# 364 "parser.ml"
                
              in
              
# 125 "parser.mly"
                                                    ( _1                                                                    )
# 370 "parser.ml"
              
            in
            
# 117 "parser.mly"
                                                    ( Printf.printf "\n%s\n" _1; EpFnCall{call_id=_1;call_args=_2},     ()  )
# 376 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv640)) : 'freshtv642)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv643 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 386 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv644)) : 'freshtv646)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv651 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 395 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv647 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 405 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ALONG ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | COMMA | DOT | ELSE | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
                _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134) : 'freshtv648)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv649 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 425 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv650)) : 'freshtv652)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv663 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 434 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv659 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 444 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv655 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 454 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv653 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 461 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s), (_2 : (
# 6 "parser.mly"
       (string)
# 466 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
                let _v : 'tv_stmt = let _3 =
                  let _1 =
                    let _1 =
                      let x = 
# 232 "<standard.mly>"
    ( xs )
# 474 "parser.ml"
                       in
                      
# 200 "<standard.mly>"
    ( x )
# 479 "parser.ml"
                      
                    in
                    
# 40 "parser.mly"
                                                    ( _1                                                        )
# 485 "parser.ml"
                    
                  in
                  
# 125 "parser.mly"
                                                    ( _1                                                                    )
# 491 "parser.ml"
                  
                in
                
# 88 "parser.mly"
                                                    ( SmLog(_2,_3,None)                                         )
# 497 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv654)) : 'freshtv656)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv657 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 507 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv658)) : 'freshtv660)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv661 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 518 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv662)) : 'freshtv664)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv631 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv627 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BECOME ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv623 * _menhir_state) * _menhir_state * 'tv_option_expr_)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | ADDRESS ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | BALANCE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | DECLIT256 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
            | DECLIT8 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
            | FALSE ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | ID _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
            | IF ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState145
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145) : 'freshtv624)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv625 * _menhir_state) * _menhir_state * 'tv_option_expr_)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv626)) : 'freshtv628)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv629 * _menhir_state) * _menhir_state * 'tv_option_expr_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv630)) : 'freshtv632)

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_stmt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv621 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | ADDRESS ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | BOOL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | BYTES32 ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | ID _v ->
        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | LOG ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | RETURN ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | SELFDESTRUCT ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | UINT256 ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | UINT8 ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | VOID ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | RBRACE ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162) : 'freshtv622)

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState151 | MenhirState71 | MenhirState75 | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv615) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv613) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 144 "<standard.mly>"
    ( x )
# 674 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv614)) : 'freshtv616)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv619 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv617 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 690 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv618)) : 'freshtv620)
    | _ ->
        _menhir_fail ()

and _menhir_goto_value_info : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_value_info -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv611) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_value_info) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv609) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_value_info) : 'tv_value_info) = _v in
    ((let _v : 'tv_msg = 
# 128 "parser.mly"
                                                    ( _1                                                                    )
# 709 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv607) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_msg) = _v in
    ((match _menhir_s with
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv597 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 721 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv595 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 729 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_5 : 'tv_msg) : 'tv_msg) = _v in
        ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), (_3 : (
# 6 "parser.mly"
       (string)
# 736 "parser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
        let _v : 'tv_expr = let _4 =
          let _1 =
            let _1 =
              let x = 
# 232 "<standard.mly>"
    ( xs )
# 744 "parser.ml"
               in
              
# 200 "<standard.mly>"
    ( x )
# 749 "parser.ml"
              
            in
            
# 40 "parser.mly"
                                                    ( _1                                                        )
# 755 "parser.ml"
            
          in
          
# 125 "parser.mly"
                                                    ( _1                                                                    )
# 761 "parser.ml"
          
        in
        
# 120 "parser.mly"
                                                    ( EpSend{sd_cn=_1;sd_mthd=Some _3;sd_args=_4;sd_msg=_5},            ()  )
# 767 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv596)) : 'freshtv598)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv601 * _menhir_state * 'tv_expr))))) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv599 * _menhir_state * 'tv_expr))))) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_6 : 'tv_msg) : 'tv_msg) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_expr)) = _menhir_stack in
        let _v : 'tv_expr = 
# 119 "parser.mly"
                                                    ( EpSend{sd_cn=_1;sd_mthd=None   ;sd_args=[];sd_msg=_6},            ()  )
# 783 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv600)) : 'freshtv602)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv605 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 791 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_msg) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv603 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 799 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_4 : 'tv_msg) : 'tv_msg) = _v in
        ((let (((_menhir_stack, _menhir_s), (_2 : (
# 6 "parser.mly"
       (string)
# 806 "parser.ml"
        ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
        let _v : 'tv_expr = let _3 =
          let _1 =
            let _1 =
              let x = 
# 232 "<standard.mly>"
    ( xs )
# 814 "parser.ml"
               in
              
# 200 "<standard.mly>"
    ( x )
# 819 "parser.ml"
              
            in
            
# 40 "parser.mly"
                                                    ( _1                                                        )
# 825 "parser.ml"
            
          in
          
# 125 "parser.mly"
                                                    ( _1                                                                    )
# 831 "parser.ml"
          
        in
        
# 118 "parser.mly"
                                                    ( EpNew {new_id=_2;new_args=_3; new_msg=_4},                        ()  )
# 837 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv604)) : 'freshtv606)
    | _ ->
        _menhir_fail ()) : 'freshtv608)) : 'freshtv610)) : 'freshtv612)

and _menhir_reduce34 : _menhir_env -> ((('ttv_tail * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
    let _v : 'tv_expr = 
# 122 "parser.mly"
                                                    ( EpArray{arrId=_1;arrIndex=_3},                                    ()  )
# 849 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_run105 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run107 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run109 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_run111 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run97 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEFAULT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv585 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv581 * _menhir_state * 'tv_expr))) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv577 * _menhir_state * 'tv_expr)))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ALONG ->
                    _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState120
                | COMMA | DOT | ELSE | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
                    _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState120
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv578)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv579 * _menhir_state * 'tv_expr)))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv580)) : 'freshtv582)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv583 * _menhir_state * 'tv_expr))) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv584)) : 'freshtv586)
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv591 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        let (_v : (
# 6 "parser.mly"
       (string)
# 1282 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv587 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 1293 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | ADDRESS ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | BALANCE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | DECLIT256 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | DECLIT8 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | FALSE ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | ID _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | IF ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | RPAR ->
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv588)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv589 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 1343 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv590)) : 'freshtv592)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv593 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv594)

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_stmt_ = 
# 211 "<standard.mly>"
    ( [] )
# 1360 "parser.ml"
     in
    _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv573 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ABORT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | ADDRESS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | BALANCE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | DECLIT256 _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | DECLIT8 _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | FALSE ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | ID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | IF ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState53
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv574)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv575 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv576)

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState139
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
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139

and _menhir_run142 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState142
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
    | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv571) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState142 in
        ((let _v : 'tv_option_expr_ = 
# 114 "<standard.mly>"
    ( None )
# 1508 "parser.ml"
         in
        _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv572)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142

and _menhir_run149 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv567 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 6 "parser.mly"
       (string)
# 1528 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv563 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 1539 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | ADDRESS ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | BALANCE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | DECLIT256 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | DECLIT8 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | FALSE ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | ID _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | IF ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | RPAR ->
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151) : 'freshtv564)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv565 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 1589 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv566)) : 'freshtv568)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv569 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv570)

and _menhir_run155 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (string)
# 1604 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
    | DARROW | ID _ ->
        _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack)
    | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv561 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 1624 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv562)

and _menhir_run156 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
    | DARROW | ID _ ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv559 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv560)

and _menhir_goto_list_mthd_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_mthd_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv549 * _menhir_state * 'tv_mthd) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv547 * _menhir_state * 'tv_mthd) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_mthd)), _, (xs : 'tv_list_mthd_)) = _menhir_stack in
        let _v : 'tv_list_mthd_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1660 "parser.ml"
         in
        _menhir_goto_list_mthd_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv548)) : 'freshtv550)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv557 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 1668 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv553 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 1678 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv551 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 1685 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (_2 : (
# 6 "parser.mly"
       (string)
# 1690 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)), _, (_5 : 'tv_list_mthd_)) = _menhir_stack in
            let _v : 'tv_cntrct = let _3 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 1697 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 1702 "parser.ml"
                
              in
              
# 40 "parser.mly"
                                                    ( _1                                                        )
# 1708 "parser.ml"
              
            in
            
# 46 "parser.mly"
                                                    ( Cntrct{mthds=_5; cntrct_id=_2; cntrct_args=_3}          )
# 1714 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv552)) : 'freshtv554)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv555 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 1724 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) * _menhir_state * 'tv_list_mthd_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv556)) : 'freshtv558)
    | _ ->
        _menhir_fail ()

and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 142 "<standard.mly>"
    ( [] )
# 1736 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce28 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 6 "parser.mly"
       (string)
# 1743 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 1749 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_expr = 
# 116 "parser.mly"
                                                    ( EpIdent _1,                                                       ()  )
# 1754 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 6 "parser.mly"
       (string)
# 1761 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NEW ->
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
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv367 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv363 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv361 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = 
# 115 "parser.mly"
                                                    ( EpAddr _3,                                                        ()  )
# 1888 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv362)) : 'freshtv364)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv365 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv366)) : 'freshtv368)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv373 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQEQ | GT | LAND | LT | MINUS | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv369 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 92 "parser.mly"
                                                    ( fun(l,r)-> EpPlus(l,r)                                    )
# 1917 "parser.ml"
             in
            
# 108 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 1922 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv370)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv371 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv372)) : 'freshtv374)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv379 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQEQ | GT | LAND | LT | MINUS | MULT | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv375 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 94 "parser.mly"
                                                    ( fun(l,r)-> EpMult(l,r)                                    )
# 1949 "parser.ml"
             in
            
# 108 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 1954 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv376)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv377 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv378)) : 'freshtv380)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv385 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | RSQBR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv381 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv382)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv383 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv384)) : 'freshtv386)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv391 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQEQ | GT | LAND | LT | NEQ | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv387 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 98 "parser.mly"
                                                    ( fun(l,r)-> EpNeq(l,r)                                     )
# 2025 "parser.ml"
             in
            
# 108 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2030 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv388)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv389 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv390)) : 'freshtv392)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQEQ | GT | LAND | LT | MINUS | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv393 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 93 "parser.mly"
                                                    ( fun(l,r)-> EpMinus(l,r)                                   )
# 2059 "parser.ml"
             in
            
# 108 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2064 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv395 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv396)) : 'freshtv398)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv403 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv399 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_value_info = 
# 132 "parser.mly"
                                                    ( _2                                                                    )
# 2107 "parser.ml"
             in
            _menhir_goto_value_info _menhir_env _menhir_stack _menhir_s _v) : 'freshtv400)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv401 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv402)) : 'freshtv404)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv409 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQEQ | GT | LAND | LT | NEQ | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv405 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 95 "parser.mly"
                                                    ( fun(l,r)-> EpLT(l,r)                                      )
# 2140 "parser.ml"
             in
            
# 108 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2145 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv406)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv407 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv408)) : 'freshtv410)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv415 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | LAND | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv411 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 99 "parser.mly"
                                                    ( fun(l,r)-> EpLAnd(l,r)                                    )
# 2186 "parser.ml"
             in
            
# 108 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2191 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv412)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv413 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv414)) : 'freshtv416)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv421 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQEQ | GT | LAND | LT | NEQ | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv417 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 96 "parser.mly"
                                                    ( fun(l,r)-> EpGT(l,r)                                      )
# 2224 "parser.ml"
             in
            
# 108 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2229 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv418)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv419 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv420)) : 'freshtv422)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv427 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EQEQ | GT | LAND | LT | NEQ | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv423 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = let _2 = 
# 97 "parser.mly"
                                                    ( fun(l,r)-> EpEq(l,r)                                      )
# 2262 "parser.ml"
             in
            
# 108 "parser.mly"
                                                    ( _2(_1,_3) ,                                                       ()  )
# 2267 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv424)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv425 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv426)) : 'freshtv428)
    | MenhirState151 | MenhirState71 | MenhirState75 | MenhirState116 | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv435 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv429 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | ADDRESS ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | BALANCE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | DECLIT256 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | DECLIT8 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | FALSE ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | ID _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | IF ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv430)
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv431 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2352 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv432)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv433 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv434)) : 'freshtv436)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv443 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv439 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv437 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = 
# 113 "parser.mly"
                                                    ( EpBalance _3,                                                     ()  )
# 2398 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv438)) : 'freshtv440)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv441 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv442)) : 'freshtv444)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv449 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv445 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | ADDRESS ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | BALANCE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | DECLIT256 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | DECLIT8 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | FALSE ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | ID _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | IF ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv446)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv447 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv448)) : 'freshtv450)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv455 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv451 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | ADDRESS ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | BALANCE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | DECLIT256 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | DECLIT8 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | FALSE ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | ID _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | IF ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv452)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv453 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv454)) : 'freshtv456)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv461 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv457 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_4 : 'tv_expr)), _, (_6 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = 
# 105 "parser.mly"
                                                    ( TmIf(_2,_4,_6),                                                   ()  )
# 2591 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv458)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv459 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv460)) : 'freshtv462)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv469 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv465 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv463 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = 
# 110 "parser.mly"
                                                    ( EpParen _2,                                                       ()  )
# 2637 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv464)) : 'freshtv466)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv467 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv468)) : 'freshtv470)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv475 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | RPAR | RSQBR | SEMI | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv471 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = 
# 109 "parser.mly"
                                                    ( EpNot _2,                                                         ()  )
# 2680 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv472)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv473 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv474)) : 'freshtv476)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv483 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv479 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv477 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 85 "parser.mly"
                                                    ( SmExpr _3                                                 )
# 2726 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv478)) : 'freshtv480)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv481 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv482)) : 'freshtv484)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv491 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv487 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv485 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 89 "parser.mly"
                                                    ( SmSlfDstrct _2                                            )
# 2772 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv486)) : 'freshtv488)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv489 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv490)) : 'freshtv492)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv499 * _menhir_state) * _menhir_state * 'tv_option_expr_))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv495 * _menhir_state) * _menhir_state * 'tv_option_expr_))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv493 * _menhir_state) * _menhir_state * 'tv_option_expr_))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_option_expr_)), _, (_5 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 82 "parser.mly"
                                                    ( SmReturn{ret_expr=_2; ret_cont=_5}                        )
# 2818 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)) : 'freshtv496)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv497 * _menhir_state) * _menhir_state * 'tv_option_expr_))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv498)) : 'freshtv500)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv505 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv501 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_option_expr_ = 
# 116 "<standard.mly>"
    ( Some x )
# 2861 "parser.ml"
             in
            _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv502)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv503 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv504)) : 'freshtv506)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv513 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 2876 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv509 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 2906 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv507 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 2913 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), (_2 : (
# 6 "parser.mly"
       (string)
# 2918 "parser.ml"
            ))), _, (_4 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 84 "parser.mly"
                                                    ( SmDecl{declTy=_1; declId=_2; declVal=_4}   )
# 2923 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv508)) : 'freshtv510)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv511 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 2933 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv512)) : 'freshtv514)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv521 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv517 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv515 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_lexpr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 83 "parser.mly"
                                                    ( SmAssign(_1,_3)                                           )
# 2973 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv516)) : 'freshtv518)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv519 * _menhir_state * 'tv_lexpr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv520)) : 'freshtv522)
    | MenhirState51 | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv527 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv523 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | ADDRESS ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | BALANCE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | DECLIT256 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
            | DECLIT8 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
            | FALSE ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | ID _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
            | IF ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | LPAR ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | NEW ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | NOT ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | NOW ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | SENDER ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | TRUE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | VALUE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169) : 'freshtv524)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv525 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv526)) : 'freshtv528)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv545 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LSQBR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | RSQBR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv541 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv537 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
                let _v : 'tv_lexpr = 
# 135 "parser.mly"
                                                    ( LEpArray{arrId=_1; arrIndex=_3}                                       )
# 3095 "parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv535) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_lexpr) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv533 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | EQ ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv529 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ABORT ->
                        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | ADDRESS ->
                        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | BALANCE ->
                        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | DECLIT256 _v ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
                    | DECLIT8 _v ->
                        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
                    | FALSE ->
                        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | ID _v ->
                        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
                    | IF ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | LPAR ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | NEW ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | NOT ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | NOW ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | SENDER ->
                        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | THIS ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | TRUE ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | VALUE ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState165
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165) : 'freshtv530)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv531 * _menhir_state * 'tv_lexpr) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv532)) : 'freshtv534)) : 'freshtv536)) : 'freshtv538)
            | DOT | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS ->
                _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv539 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv540)) : 'freshtv542)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv543 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv544)) : 'freshtv546)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_evnt_arg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv355) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_evnt_arg_) : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ = 
# 144 "<standard.mly>"
    ( x )
# 3190 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv354)) : 'freshtv356)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv359 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv357 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_evnt_arg_) : 'tv_separated_nonempty_list_COMMA_evnt_arg_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_evnt_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_evnt_arg_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3206 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv358)) : 'freshtv360)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_arg_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState45 | MenhirState36 | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv347) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv345) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_arg_) : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 144 "<standard.mly>"
    ( x )
# 3227 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)) : 'freshtv348)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv351 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv349 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_arg_) : 'tv_separated_nonempty_list_COMMA_arg_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_arg_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3243 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)) : 'freshtv352)
    | _ ->
        _menhir_fail ()

and _menhir_goto_mthd_head : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_mthd_head -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv343 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv339) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ABORT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | ADDRESS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | BALANCE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | BOOL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | BYTES32 ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | DECLIT256 _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | DECLIT8 _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | FALSE ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | ID _v ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | IF ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LOG ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LPAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NEW ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NOW ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | RETURN ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SELFDESTRUCT ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState51
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
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51) : 'freshtv340)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv341 * _menhir_state * 'tv_mthd_head) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv342)) : 'freshtv344)

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_mthd_ = 
# 211 "<standard.mly>"
    ( [] )
# 3330 "parser.ml"
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
        let (_menhir_stack : 'freshtv335 * _menhir_state) = Obj.magic _menhir_stack in
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
            let (_menhir_stack : ('freshtv333 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState33 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv329 * _menhir_state)) * _menhir_state) = Obj.magic _menhir_stack in
                let (_v : (
# 6 "parser.mly"
       (string)
# 3372 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv325 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 3383 "parser.ml"
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
                        _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv326)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv327 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 3413 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)) : 'freshtv330)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv331 * _menhir_state)) * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)) : 'freshtv334)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv336)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv337 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv338)

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv323) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_mthd_head = 
# 56 "parser.mly"
                                                    ( Default                                                   )
# 3445 "parser.ml"
     in
    _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | MSG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv315 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv311 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv309 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_expr = 
# 111 "parser.mly"
                                                    ( EpValue,                                                          ()  )
# 3477 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)) : 'freshtv312)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv313 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv314)) : 'freshtv316)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv317 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv318)) : 'freshtv320)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv321 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv322)

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv307) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 103 "parser.mly"
                                                    ( EpTrue,                                                           ()  )
# 3511 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv305) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 121 "parser.mly"
                                                    ( EpThis,                                                           ()  )
# 3524 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv306)

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv301 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MSG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv297 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv293 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv291 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_expr = 
# 112 "parser.mly"
                                                    ( EpSender,                                                         ()  )
# 3556 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv292)) : 'freshtv294)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv295 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)) : 'freshtv298)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv299 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)) : 'freshtv302)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv303 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv304)

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BLOCK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv283 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv279 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv277 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                let _v : 'tv_expr = 
# 114 "parser.mly"
                                                    ( EpNow,                                                            ()  )
# 3609 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv278)) : 'freshtv280)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv281 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv285 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv286)) : 'freshtv288)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState68
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
        let (_menhir_stack : 'freshtv273 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 6 "parser.mly"
       (string)
# 3689 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv269 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 3700 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ABORT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | ADDRESS ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | BALANCE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | DECLIT256 _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | DECLIT8 _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | FALSE ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | ID _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | IF ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState71
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
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv270)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv271 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 3750 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)) : 'freshtv274)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv275 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72
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

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABORT ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | ADDRESS ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | BALANCE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | DECLIT256 _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | DECLIT8 _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | FALSE ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | ID _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | IF ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LPAR ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NEW ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOT ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOW ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | SENDER ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TRUE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | VALUE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (string)
# 3851 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | DOT | ELSE | EQEQ | GT | LAND | LSQBR | LT | MINUS | MULT | NEQ | PLUS | RPAR | RSQBR | SEMI | THEN ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv267 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 3869 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv265) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 104 "parser.mly"
                                                    ( EpFalse,                                                          ()  )
# 3883 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (Big_int.big_int)
# 3890 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv263) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 8 "parser.mly"
       (Big_int.big_int)
# 3900 "parser.ml"
    )) : (
# 8 "parser.mly"
       (Big_int.big_int)
# 3904 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 107 "parser.mly"
                                                    ( EpDecLit8 _1,                                                     ()  )
# 3909 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv264)

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (Big_int.big_int)
# 3916 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv261) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 7 "parser.mly"
       (Big_int.big_int)
# 3926 "parser.ml"
    )) : (
# 7 "parser.mly"
       (Big_int.big_int)
# 3930 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 106 "parser.mly"
                                                    ( EpDecLit256 _1,                                                   ()  )
# 3935 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv262)

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv257 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ABORT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | ADDRESS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | BALANCE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DECLIT256 _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | DECLIT8 _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | FALSE ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | ID _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | IF ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LPAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NEW ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NOW ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SENDER ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | THIS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | TRUE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | VALUE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv258)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv259 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)

and _menhir_run81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv255 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv253) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 102 "parser.mly"
                                                    ( SmAbort,                                                          ()  )
# 4020 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv254)

and _menhir_goto_evnt_arg : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_evnt_arg -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv246)
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_evnt_arg)) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_evnt_arg_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4061 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_evnt_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv248)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_evnt_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv250)) : 'freshtv252)

and _menhir_run13 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_ty -> (
# 6 "parser.mly"
       (string)
# 4075 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
    let ((_2 : (
# 6 "parser.mly"
       (string)
# 4084 "parser.ml"
    )) : (
# 6 "parser.mly"
       (string)
# 4088 "parser.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s, (_1 : 'tv_ty)) = _menhir_stack in
    let _v : 'tv_arg = 
# 61 "parser.mly"
                                                    ( TyVar(_2,_1)                                             )
# 4094 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv241) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_arg) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv231 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv229 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_arg)) = _menhir_stack in
        let _v : 'tv_evnt_arg = 
# 64 "parser.mly"
                                                    ( tyEvntArg_of_arg _1 false                                  )
# 4111 "parser.ml"
         in
        _menhir_goto_evnt_arg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv230)) : 'freshtv232)
    | MenhirState26 | MenhirState45 | MenhirState41 | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv233 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv234)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv235 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_arg)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_arg_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4149 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv236)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv237 * _menhir_state * 'tv_arg) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)) : 'freshtv240)
    | _ ->
        _menhir_fail ()) : 'freshtv242)) : 'freshtv244)

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
    let (_menhir_stack : 'freshtv227 * _menhir_state * 'tv_cntrct) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONTRACT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | EVENT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | EOF ->
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182) : 'freshtv228)

and _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv201 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4217 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv197 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4227 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv193 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4237 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | DEFAULT ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | METHOD ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | RBRACE ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv194)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv195 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4259 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)) : 'freshtv198)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv199 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4270 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv213 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4279 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv209 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4289 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv205 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4299 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv203 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4306 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _), (_4 : (
# 6 "parser.mly"
       (string)
# 4311 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
                let _v : 'tv_mthd_head = let _5 =
                  let _1 =
                    let x = 
# 232 "<standard.mly>"
    ( xs )
# 4318 "parser.ml"
                     in
                    
# 200 "<standard.mly>"
    ( x )
# 4323 "parser.ml"
                    
                  in
                  
# 40 "parser.mly"
                                                    ( _1                                                        )
# 4329 "parser.ml"
                  
                in
                
# 58 "parser.mly"
                                                    ( Method{mthd_retTy=TyTuple[];mthd_id=_4; mthd_args=_5}   )
# 4335 "parser.ml"
                 in
                _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)) : 'freshtv206)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv207 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4345 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)) : 'freshtv210)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv211 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4356 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv225 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4365 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv221 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4375 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv217 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4385 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv215 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4392 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (_3 : 'tv_ty)), (_4 : (
# 6 "parser.mly"
       (string)
# 4397 "parser.ml"
                ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_arg__)) = _menhir_stack in
                let _v : 'tv_mthd_head = let _5 =
                  let _1 =
                    let x = 
# 232 "<standard.mly>"
    ( xs )
# 4404 "parser.ml"
                     in
                    
# 200 "<standard.mly>"
    ( x )
# 4409 "parser.ml"
                    
                  in
                  
# 40 "parser.mly"
                                                    ( _1                                                        )
# 4415 "parser.ml"
                  
                in
                
# 57 "parser.mly"
                                                    ( Method{mthd_retTy=_3;       mthd_id=_4; mthd_args=_5}   )
# 4421 "parser.ml"
                 in
                _menhir_goto_mthd_head _menhir_env _menhir_stack _menhir_s _v) : 'freshtv216)) : 'freshtv218)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv219 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4431 "parser.ml"
                ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)) : 'freshtv222)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv223 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4442 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)) : 'freshtv226)
    | _ ->
        _menhir_fail ()

and _menhir_reduce74 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 6 "parser.mly"
       (string)
# 4452 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 4458 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_ty = 
# 74 "parser.mly"
                                                    ( TyInstnce _1                                              )
# 4463 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _v
        | INDEXED ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv157 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv153 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
                let (_v : (
# 6 "parser.mly"
       (string)
# 4493 "parser.ml"
                )) = _v in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv151 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
                let ((_3 : (
# 6 "parser.mly"
       (string)
# 4501 "parser.ml"
                )) : (
# 6 "parser.mly"
       (string)
# 4505 "parser.ml"
                )) = _v in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_ty)) = _menhir_stack in
                let _v : 'tv_evnt_arg = 
# 65 "parser.mly"
                                                    ( {arg=TyVar(_3,_1); indexed=true}                        )
# 4511 "parser.ml"
                 in
                _menhir_goto_evnt_arg _menhir_env _menhir_stack _menhir_s _v) : 'freshtv152)) : 'freshtv154)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv155 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)) : 'freshtv158)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv159 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv167 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _ | INDEXED ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv163 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), _, (_3 : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_ty = 
# 73 "parser.mly"
                                                    ( TyMap(_1,_3)                                              )
# 4543 "parser.ml"
             in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv164)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv165 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
    | MenhirState45 | MenhirState41 | MenhirState36 | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
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
            let (_menhir_stack : 'freshtv169 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)) : 'freshtv172)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv181 * _menhir_state)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv177 * _menhir_state)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 6 "parser.mly"
       (string)
# 4584 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv173 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4595 "parser.ml"
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
                    _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState45
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv174)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv175 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4625 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)) : 'freshtv178)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv179 * _menhir_state)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)) : 'freshtv182)
    | MenhirState162 | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv187 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 6 "parser.mly"
       (string)
# 4650 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv183 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4661 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ABORT ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | ADDRESS ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | BALANCE ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | DECLIT256 _v ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | DECLIT8 _v ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | FALSE ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | ID _v ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | IF ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | LPAR ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | NEW ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | NOT ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | NOW ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | SENDER ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | THIS ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | TRUE ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | VALUE ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159) : 'freshtv184)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv185 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 4709 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)) : 'freshtv188)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv189 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)) : 'freshtv192)
    | _ ->
        _menhir_fail ()

and _menhir_reduce71 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : 'tv_ty = 
# 71 "parser.mly"
                                                    ( TyAddr                                                    )
# 4729 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_cntrct_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_cntrct_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv141 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv139 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_list_cntrct_)) = _menhir_stack in
            let _v : (
# 36 "parser.mly"
       (unit Syntax.toplevel list)
# 4752 "parser.ml"
            ) = 
# 43 "parser.mly"
                                                    ( _1                                                        )
# 4756 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv137) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 36 "parser.mly"
       (unit Syntax.toplevel list)
# 4764 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 36 "parser.mly"
       (unit Syntax.toplevel list)
# 4772 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv133) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 36 "parser.mly"
       (unit Syntax.toplevel list)
# 4780 "parser.ml"
            )) : (
# 36 "parser.mly"
       (unit Syntax.toplevel list)
# 4784 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv134)) : 'freshtv136)) : 'freshtv138)) : 'freshtv140)) : 'freshtv142)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv143 * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)) : 'freshtv146)
    | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv149 * _menhir_state * 'tv_cntrct) * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv147 * _menhir_state * 'tv_cntrct) * _menhir_state * 'tv_list_cntrct_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_cntrct)), _, (xs : 'tv_list_cntrct_)) = _menhir_stack in
        let _v : 'tv_list_cntrct_ = 
# 213 "<standard.mly>"
    ( x :: xs )
# 4803 "parser.ml"
         in
        _menhir_goto_list_cntrct_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv148)) : 'freshtv150)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_evnt_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv131 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4816 "parser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv127 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4826 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv123 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4836 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv121 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4843 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (_2 : (
# 6 "parser.mly"
       (string)
# 4848 "parser.ml"
            ))), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = _menhir_stack in
            let _v : 'tv_cntrct = let _3 =
              let _1 =
                let x = 
# 232 "<standard.mly>"
    ( xs )
# 4855 "parser.ml"
                 in
                
# 200 "<standard.mly>"
    ( x )
# 4860 "parser.ml"
                
              in
              
# 40 "parser.mly"
                                                    ( _1                                                        )
# 4866 "parser.ml"
              
            in
            
# 47 "parser.mly"
                                                    ( Event {                 id=_2;    tyEvArgs=_3}          )
# 4872 "parser.ml"
             in
            _menhir_goto_cntrct _menhir_env _menhir_stack _menhir_s _v) : 'freshtv122)) : 'freshtv124)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv125 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4882 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)) : 'freshtv128)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv129 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 4893 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_evnt_arg__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_arg__ = 
# 142 "<standard.mly>"
    ( [] )
# 4903 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 69 "parser.mly"
                                                    ( TyUint8                                                   )
# 4916 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv120)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 68 "parser.mly"
                                                    ( TyUint256                                                 )
# 4929 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv118)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (string)
# 4936 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 70 "parser.mly"
                                                    ( TyBytes32                                                 )
# 4952 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv116)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_ty = 
# 72 "parser.mly"
                                                    ( TyBool                                                    )
# 4965 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv114)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_cntrct) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_mthd) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state * 'tv_lexpr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv31 * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 5008 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv33 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5017 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv35 * _menhir_state) * _menhir_state * 'tv_option_expr_))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv41 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5041 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv43 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv45 * _menhir_state) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv47 * _menhir_state * 'tv_expr))))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv51 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv61 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 5095 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv63 * _menhir_state * 'tv_expr)) * (
# 6 "parser.mly"
       (string)
# 5104 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * (
# 6 "parser.mly"
       (string)
# 5148 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv85 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5167 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv92)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv93 * _menhir_state)) * _menhir_state * 'tv_ty) * (
# 6 "parser.mly"
       (string)
# 5190 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state * 'tv_arg)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv97 * _menhir_state)) * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5204 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv101 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5218 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_arg__))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv103 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5227 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state * 'tv_evnt_arg)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv109 * _menhir_state) * (
# 6 "parser.mly"
       (string)
# 5246 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv112)

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_cntrct_ = 
# 211 "<standard.mly>"
    ( [] )
# 5260 "parser.ml"
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
# 5276 "parser.ml"
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
# 5287 "parser.ml"
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
# 5311 "parser.ml"
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
# 5325 "parser.ml"
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
# 5349 "parser.ml"
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
# 5360 "parser.ml"
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
                _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState26
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
# 5390 "parser.ml"
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
# 5417 "parser.ml"
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
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 5445 "parser.ml"
