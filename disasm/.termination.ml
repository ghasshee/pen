
(* Termination *) 

type termination            =
                            | OnTheWay 
                            | ReturnBySize of int 
                            | JustStop

(* and codegen_if_then le ce ly cond stmts =
    let label   = fresh_label ()                                in
    let ce      = cond                         @> (R,le,ce,ly)in
    let ce      = if_0_GOTO label                 ce            in 
    let le,ce   = codegen_stmts stmts ly       le ce            in
    let ce      = JUMPDEST label                @>> ce            in
    le,ce *)

