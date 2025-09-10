

let sstore_var_args cnidx vm   =                               
    let loop    = fresh_label()                                 in
    let exit    = fresh_label()                                 in  (*                                                  alloc(size)   >>   size    >> .. *)  
    let vm      = PUSH (StorVarBegin cnidx)         @>> vm      in  (*                                          idx >>  alloc(size)   >>   size    >> .. *)
    let vm   = JUMPDEST loop                        @>> vm      in  (*                                          idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = DUP3                              @>> vm      in  (*                                 size >>  idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = if0GOTO exit                          vm      in  (* IF size == 0 GOTO exit                   idx >>  alloc(size)   >>   size    >> .. *)   
    let vm      = DUP2                              @>> vm      in  (*                            mem_start >>  idx >>  alloc(size)   >>   size    >> .. *) 
    let vm      = MLOAD                             @>> vm      in  (*                         M[mem_start] >>  idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = DUP2                              @>> vm      in  (*                  idx >> M[mem_start] >>  idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = SSTORE                            @>> vm      in  (* S[idx] := M[mem_start]                   idx >>  alloc(size)   >>   size    >> .. *)  
    let vm      = PUSH (Int 0x20)                   @>> vm      in  (*                                 0x20 >>  idx >>  alloc(size)   >>   size    >> .. *)
    let vm      = SWAP1                             @>> vm      in  (*                                  idx >> 0x20 >>  alloc(size)   >>   size    >> .. *)
    let vm      = SWAP3                             @>> vm      in  (*                                 size >> 0x20 >>  alloc(size)   >>   idx     >> .. *)
    let vm      = SUB                               @>> vm      in  (*                                   size- 0x20 >>  alloc(size)   >>   idx     >> .. *)
    let vm      = SWAP2                             @>> vm      in  (*                                          idx >>  alloc(size)   >> size-0x20 >> .. *) 
    let vm      = incr 1                                vm      in  (*                                        idx+1 >>  alloc(size)   >> size-0x20 >> .. *)
    let vm      = SWAP1                             @>> vm      in  (*                                    mem_start >>       idx+1    >> size-0x20 >> .. *)
    let vm      = incr 0x20                             vm      in  (*                               mem_start+0x20 >>       idx+1    >> size-0x20 >> .. *)
    let vm      = SWAP1                             @>> vm      in  (*                                        idx+1 >> mem_start+0x20 >> size-0x20 >> .. *)
    let vm      = goto loop                             vm      in  (*                                        idx+1 >> mem_start+0x20 >> size-0x20 >> .. *)
    let vm   = JUMPDEST exit                        @>> vm      in  (*                                          idx >>   mem_start    >>   size    >> .. *)
                  repeat POP 3                          vm          (*                                                                     .. *)

