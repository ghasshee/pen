(* M[64]  :=   the address of mem alloc     *) 
(* MSTORE :=   x=pop() ; y=pop() ; M[x]=y   *) 
(* MLOAD  :=   x=pop() ; push M[x]          *) 
(* CODECOPY  to from len :=  M[to .. to+len-1]=I_b[from .. from+len-1]  *)



open Label
open Big_int
open Printf 

open Misc
open Abstract
open CodegenEnv
open LocationEnv
open Evm
open Syntax
open IndexedList
open Contract

module Eth  = Ethereum 
module BL   = BatList
module L    = List
module SL   = StorLayout


(******************************************************)
(***     1. ERROR HANDLING                          ***)
(******************************************************)

let throw ce         = (* the same with solc. *)
    let ce      =   PUSH1(Int 2)                    >>ce    in
                    JUMP                            >>ce 

let throw_if_0 ce    =                                          (*                        i >> .. *)   
    let ce      =   DUP1                            >>ce    in  (*                   i >> i >> .. *)
    let ce      =   ISZERO                          >>ce    in  (*                   b >> i >> .. *)
    let ce      =   PUSH1(Int 2)                    >>ce    in  (*              0 >> b >> i >> .. *)
                    JUMPI                           >>ce        (* {GOTO 2 if b}          i >> .. *)
      
let throw_if_NEQ ce   =                                         (*                      a >> b >> .. *) 
    let ce      =   EQ                              >>ce    in  (*                        a==b >> .. *)
    let ce      =   ISZERO                          >>ce    in  (*                        a!=b >> .. *)    
    let ce      =   PUSH1(Int 2)                    >>ce    in  (*                   0 >> a!=b >> .. *)
                    JUMPI                           >>ce        (* {IF a!=b THEN GOTO 0}          .. *)

let if_0_GOTO lbl ce = 
    let ce      =   ISZERO                          >>ce    in 
    let ce      =   PUSH4(Label lbl)                >>ce    in 
                    JUMPI                           >>ce    

let goto la   ce    = 
    let ce      =   PUSH4(Label la)                 >>ce    in 
                    JUMP                            >>ce 

let whileLOOP ce cond body = 
    let label   = fresh_label()                             in 
    let exit    = fresh_label()                             in 
    let ce      = JUMPDEST label                    >>ce    in 
    let ce      = cond exit                           ce    in 
    let ce      = body                                ce    in 
    let ce      = goto label                          ce    in  (*                                 idx+1 >> mem_start+32 >> size-32 >> .. *)
                  JUMPDEST exit                     >>ce   
    


(******************************************************)
(***     2. STACK OPERATIONS                        ***)
(******************************************************)

let push_storRange ce (range : imm stor_range) =
    let i = match range.stor_size with | Big b -> string_of_big b | Int i -> string_of_int i in 
    printf "stor_size is %s\n" i ; 
    assert (is_const_int 1 range.stor_size) ; 
    let offset  = range.stor_start                          in
    let ce      =   PUSH32 offset                   >>ce    in
                    SLOAD                           >>ce 

let dup_nth_from_bottom n ce  =
    let diff   =(stack_size ce)-n in
    assert(diff>=0) ; 
                dup_succ diff                       >>ce 

let shiftRtop ce bits =
    assert (0 <= bits && bits < 256) ; 
    if bits=0 then ce else                                       (*                 x >> .. *) 
    let ce    = PUSH1 (Int bits)                    >>ce    in   (*         bits >> x >> .. *)
    let ce    = PUSH1 (Int 2)                       >>ce    in   (*    2 >> bits >> x >> .. *)
    let ce    = EXP                                 >>ce    in   (*      2**bits >> x >> .. *) 
    let ce    = SWAP1                               >>ce    in   (*      x >> 2**bits >> .. *) 
                DIV                                 >>ce         (*       x/(2**bits) >> .. *) 

let shiftLtop ce bits =
    assert (0 <= bits && bits < 256) ; 
    if bits=0 then ce else                                       (*                 x >> .. *)
    let ce    = PUSH1 (Int bits)                    >>ce    in   (*         bits >> x >> .. *)                   
    let ce    = PUSH1 (Int 2)                       >>ce    in   (*    2 >> bits >> x >> .. *) 
    let ce    = EXP                                 >>ce    in   (*      2**bits >> x >> .. *) 
                MUL                                 >>ce         (*       (2**bits)*x >> .. *) 

let incr_top (inc : int) ce =
    let ce    = PUSH32 (Int inc)                    >>ce    in
                ADD                                 >>ce      

let calldataload ce (range : calldata_range) =
    let start = range.calldata_start                        in 
    let size  = range.calldata_size                         in 
    assert (0 < size && size <= 32);
    let ce    = PUSH4 (Int start)                   >>ce    in
                CALLDATALOAD                        >>ce  

(* take a := 256bit , b := 256bit(=32byte) , 
 *      and return sha3(a++b) *)  
let keccak_cat ce =                                             (*                                               a >> b >> .. *) 
    let ce    = PUSH1 (Int 0x00)                    >>ce    in  (*                                       0x00 >> a >> b >> .. *)
    let ce    = MSTORE                              >>ce    in  (* M[0x00]=a                                          b >> .. *)
    let ce    = PUSH1 (Int 0x20)                    >>ce    in  (*                                            0x20 >> b >> .. *)
    let ce    = MSTORE                              >>ce    in  (* M[0x20]=b                                               .. *) 
    let ce    = PUSH1 (Int 0x40)                    >>ce    in  (*                                                 0x40 >> .. *)
    let ce    = PUSH1 (Int 0x00)                    >>ce    in  (*                                         0x0  >> 0x40 >> .. *)
                SHA3                                >>ce        (*                                  sha3(M[0x00..0x3F]) >> .. *)

(******************************************************)
(***     3. ALIGNMENT R ? L ?                       ***)
(******************************************************)
                
type alignment              = L 
                            | R

let align_bool ce aln   = assert(aln=R);  ce
let align_addr ce       = function 
    | R                 ->  ce
    | L                 ->  shiftLtop ce (12 * 8)
let align_to_L ce ty    = function 
    | R                 ->  ce
    | L                 ->  let size = size_of_ty ty in
                            assert (size <= 32) ;
                            shiftLtop ce ((32-size)*8) 

let push_loc ce aln ty  = function 
    | Code       _      ->  err "push_loc: Code"  
    | Calldata rng      ->  calldataload  ce rng
    | Stor     rng      ->  let ce = push_storRange ce rng in 
                            align_to_L ce ty aln  
    | Stack      n      ->  let ce = dup_nth_from_bottom n ce in 
                            align_to_L ce ty aln 

(******************************************************)
(***     4. PROGRAM COUNTER on STORAGE              ***)
(******************************************************)
                
let reset_PC ce   =
    let ce    = PUSH1 (Int 0)                       >>ce    in  (*                             0 >> .. *)
    let ce    = SLOAD                               >>ce    in  (*                          S[0] >> .. *)
    let ce    = PUSH1 (Int 0)                       >>ce    in  (*                     0 >> S[0] >> .. *)
    let ce    = DUP1                                >>ce    in  (*                0 >> 0 >> S[0] >> .. *)
                SSTORE                              >>ce        (* S'[0]=0                  S[0] >> .. *)

(** [restore_PC]   *)  
(*                                                 
 *     BEFORE             AFTER                    
 *    +--------+                                     
 *    | bkp_pc |                                     
 *  --+--------+--    --+--------+--  *)

let restore_PC ce       =                                        (*                   bkp_pc >> .. *)
    let ce    = PUSH1(Int 0)                        >>ce    in   (*              0 >> bkp_pc >> .. *)
                SSTORE                              >>ce         (* S'[0]=bkp_pc                .. *)             



(****************************************)
(***     5. MEMORY OPERATIONS         ***)
(****************************************)

(**  [mem_alloc]                              Addr     Val              Addr     Val     
 *                                         +--------+--------+       +--------+--------+
 *                                         |   64   |   a    |       |   64   | a+size |
 *      BEFORE            AFTER            +--------+--------+       +--------+--------+
 *                                         | ...    |  ...   |       | ...    |  ...   |
 *   +----------+      +----------+        +--------+--------+       +--------+--------+
 *   |   size   |      |    a     |      -->   a    |     0  |       |   a    |     0  |
 * --+----------+--  --+----------+--      +--------+--------+       +--------+--------+
 *                                         | ...    |     0  |       | ...    |     0  |
 *  mem_alloc :=                           +--------+--------+       +--------+--------+
 *      size := pop();                     | a+size |        |     --> a+size |        |
 *      a    := alloc(size);               +--------+--------+       +--------+--------+
 *      push(a)                                BEFORE MEM                AFTER MEM 
 *)

let mem_alloc ce    =                                       (*  STACK                                            len >> .. *)
    let ce    = PUSH1 (Int 64)                  >>ce    in  (*                                             64 >> len >> .. *)
    let ce    = DUP1                            >>ce    in  (*                                       64 >> 64 >> len >> .. *)
    let ce    = MLOAD                           >>ce    in  (*                                    M[64] >> 64 >> len >> .. *)
    let ce    = DUP1                            >>ce    in  (*                           M[64] >> M[64] >> 64 >> len >> .. *)
    let ce    = SWAP3                           >>ce    in  (*                           len >> M[64] >> 64 >> M[64] >> .. *)
    let ce    = ADD                             >>ce    in  (*                              M[64+len] >> 64 >> M[64] >> .. *)
    let ce    = SWAP1                           >>ce    in  (*                              64 >> M[64+len] >> M[64] >> .. *)
                MSTORE                          >>ce        (*                                                 M[64] >> .. *) 

let get_alloc ce    =
    let ce    = PUSH1 (Int 64)                  >>ce    in  (* 64    >> .. *) 
                MLOAD                           >>ce        (* M[64] >> .. *) 
      
let mstore_code ce =                                        (*                                            idx >> size >> .. *)
    let ce    = DUP2                            >>ce    in  (*                                    size >> idx >> size >> .. *)
    let ce    = mem_alloc                         ce    in  (*                             alloc(size) >> idx >> size >> .. *)
    let ce    = SWAP1                           >>ce    in  (*                             idx >> alloc(size) >> size >> .. *)
    let ce    = DUP3                            >>ce    in  (*                     size >> idx >> alloc(size) >> size >> .. *)
    let ce    = SWAP1                           >>ce    in  (*                     idx >> size >> alloc(size) >> size >> .. *)
    let ce    = DUP3                            >>ce    in  (*      alloc(size) >> idx >> size >> alloc(size) >> size >> .. *) 
                CODECOPY                        >>ce        (*                                    alloc(size) >> size >> .. *)  

let mstore_whole_code ce =
    let ce    = CODESIZE                        >>ce    in  (*                                                   size >> .. *)
    let ce    = DUP1                            >>ce    in  (*                                           size >> size >> .. *)
    let ce    = mem_alloc                         ce    in  (*                                    alloc(size) >> size >> .. *)
    let ce    = DUP2                            >>ce    in  (*                            size >> alloc(size) >> size >> .. *)
    let ce    = PUSH1(Int 0)                    >>ce    in  (*                      0  >> size >> alloc(size) >> size >> .. *)
    let ce    = DUP3                            >>ce    in  (*      alloc(size) >>  0  >> size >> alloc(size) >> size >> .. *)
                CODECOPY                        >>ce        (*        to           from           alloc(size) >> size >> .. *)

let push_mthd_hash m ce =
    let b     = Eth.(big_of_hex $ hash_ty_mthd)m        in  
                PUSH4(Big b)                    >>ce    

let push_evnt_hash ev ce =
    let b     = Eth.(big_of_hex $ hash_of_evnt)ev       in  
                PUSH4(Big b)                    >>ce             

let mstore_hash_of_mthd mthd ce =
    let ce    = PUSH1(Int 4)                    >>ce    in  (*                                                       4 >> .. *)
    let ce    = DUP1                            >>ce    in  (*                                                 4  >> 4 >> .. *)
    let ce    = mem_alloc                         ce    in  (*                                           alloc(4) >> 4 >> .. *)
    let ce    = push_mthd_hash mthd               ce    in  (*                                   hash >> alloc(4) >> 4 >> .. *)
    let ce    = DUP2                            >>ce    in  (*                       alloc(4) >> hash >> alloc(4) >> 4 >> .. *)
                MSTORE                          >>ce        (* M[alloc(4)] := hash                       alloc(4) >> 4 >> .. *)

type memoryPack             = TightPack   (* [Tight] uses [size_of_ty] bytes    on mem *) 
                            | ABIPack     (* [ABI]   uses multiples of 32 bytes on mem *) 






(*********************************************)
(***     6.    CODEGEN  EXPR               ***)
(*********************************************)

(* [mstore_new_instance] 
 *
 *              ADDRESS              MEMORY 
 *          +---------------------+-----------------+                         
 *          |                  0  | rntimeCode      |                                   
 *          |                ...  |  ...            |                                   
 *          |                ...  | RETURN          |                                   
 *          |               size  | cnstrctrCode    |
 *          |                ...  |  ...            |
 *          |                ...  | RETURN          |
 *          |         size+wsize  | arg1            |                                   
 *          |                     |  ...            |
 *          |                     | arg2            |
 *          |                     |  ...            |
 *          |---------------------|-----------------|
 *          | argsize+size+wsize  |                 |
 *          |                ...  |                 |                         
 *)                                                                 

let rec mstore_new_instance le ce n =
    let cn_name   =  n.new_head in
    let cn_idx    =  lookup_cn_of_ce ce cn_name in 
    let ce    = PUSH32(CnstrctrCodeSize cn_idx)     >>ce   in (*                                                             size >> .. *) 
    let ce    = PUSH32(RntimeCnstrctrOffset cn_idx) >>ce   in (*                                                  cn_idx  >> size >> .. *)
    let ce    = mstore_code                           ce   in (*                                              alloc(size) >> size >> .. *)
    let ce    = SWAP1                               >>ce   in (*                                              size >> alloc(size) >> .. *)
    let ce    = mstore_whole_code                     ce   in (*                     alloc(wsize) >> wsize >> size >> alloc(size) >> .. *)
    let ce    = mstore_mthd_args ABIPack n.new_args le ce  in (*         argssize >> alloc(wsize) >> wsize >> size >> alloc(size) >> .. *)
    let ce    = SWAP1                               >>ce   in (*         alloc(wsize) >> argssize >> wsize >> size >> alloc(size) >> .. *)
    let ce    = POP                                 >>ce   in (*                         argssize >> wsize >> size >> alloc(size) >> .. *)
    let ce    = ADD                                 >>ce   in (*                            argssize+wsize >> size >> alloc(size) >> .. *)
    let ce    = ADD                                 >>ce   in (*                               argssize+wsize+size >> alloc(size) >> .. *)
                SWAP1                               >>ce      (*                                       alloc(size) >>   totalsize >> .. *)

and codegen_call le ce align (fncall:ty _call) (reT:ty) = 
    match fncall.call_head with 
    | "pre_ecdsarecover"    ->  assert (align = R) ; 
                                codegen_ECDSArecover le ce fncall.call_args
    | "keccak256"           ->  assert (align = R) ; 
                                codegen_keccak256    le ce fncall.call_args    
    | "iszero"              ->  codegen_iszero le ce align fncall.call_args reT
    | _                     ->  err "codegen_call: Contract Function Call is Not supported yet."

and codegen_iszero le ce aln args reT = match args with
    | [arg] ->  assert (reT=TyBool) ; 
                let ce =  arg                   >>>>(aln,le,ce) in
                          ISZERO    >>         ce 
    | _     ->  err "codegen_iszero: Wrong number of args"
       
and codegen_keccak256 le ce args =
    let ce    = get_alloc ce                                    in  (* stack: [..., offset] *)
    let ce    = mstore_mthd_args TightPack args le ce           in  (* stack: [..., offset, size] *)
    let ce    = SWAP1                           >>ce            in  (* stack: [..., size, offset] *)
                SHA3                            >>ce              

and codegen_ECDSArecover le ce args = match args with
    | [h; v; r; s] ->
        let ce = PUSH1 (Int 32)                 >>ce            in  (* stack: [out size] *)
        let ce = DUP1                           >>ce            in  (* stack: [out size, out size] *)
        let ce = mem_alloc                        ce            in  (* stack: [out size, out address] *)
        let ce = DUP2                           >>ce            in  (* stack: [out size, out address, out size] *)
        let ce = DUP2                           >>ce            in  (* stack: [out size, out address, out size, out address] *)
        let ce = get_alloc                        ce            in
        let ce = mstore_mthd_args ABIPack args le ce            in  (* stack: [out size, out address, out size, out address, mem_offset, mem_total_size] *)
        let ce = SWAP1                          >>ce            in  (* stack: [out size, out address, out size, out address, in size, in offset] *)
        let ce = PUSH1 (Int 0)                  >>ce            in  (* stack: [out size, out address, out size, out address, in size, in offset, value] *)
        let ce = PUSH1 (Int 1)                  >>ce            in  (* stack: [out size, out address, out size, out address, in size, in offset, value, to] *)
        let ce = PUSH4 (Int 10000)              >>ce            in  (* stack: [out size, out address, out size, out offset, in size, in offset, value, to, gas] *)
        let ce = CALL                           >>ce            in  (* stack: [out size, out address, success?] *)
        let ce = throw_if_0 ce                                  in
        let ce = POP                            >>ce            in  (* stack: [out size, out address] *)
        let ce = SWAP1                          >>ce            in  (* stack: [out address, out size] *)
        let ce = POP                            >>ce            in  (* we know it's 32 *) (* stack: [out address] *)
                 MLOAD                          >>ce                (* stack: [output] *)
    | _ -> err "pre_ecdsarecover has a wrong number of args"

and codegen_new le ce n =    
    assert(is_throw_only n.new_msg.msg_reentrance) ;                (* This is NOT a REENTRANCE GUARD, which MUST be modified *) 
    let ce    = reset_PC ce                                     in  (*                                                          PCbkp >> .. *)
    let ce    = mstore_new_instance le ce n                     in  (*                                   alloc(size) >> size >> PCbkp >> .. *)
    let ce    = match n.new_msg.msg_value with                      (* value is the amount sent at the contract creation                    *) 
        | None     -> PUSH1 (Int 0)             >>ce                (*                                                                      *)
        | Some e   -> e                         >>>>(R,le,ce)   in  (*                          value >> alloc(size) >> size >> PCbkp >> .. *)
    let ce    = CREATE                          >>ce            in  (*                                          createResult >> PCbkp >> .. *)
    let ce    = throw_if_0                        ce            in  (*                                          createResult >> PCbkp >> .. *)
    let ce    = SWAP1                           >>ce            in  (*                                          PCbkp >> CreateResult >> .. *)
                restore_PC                        ce                (*                                                   CreateResult >> .. *)

and gen_array_storLoc le ce aa =
    let arr   = aa.arrIdent                                     in
    let idx   = aa.arrIndex                                     in
    let ce    = idx                             >>>>(R,le,ce)   in  (*                                 index >> .. *)    
    let ce    = arr                             >>>>(R,le,ce)   in  (*                    array_loc >> index >> .. *)
                keccak_cat ce                                       (*                sha3(array_loc++index) >> .. *) 
      
and codegen_array le ce (aa:ty array) =
    let ce    = gen_array_storLoc le ce aa in
                SLOAD                           >>ce 

(* if the stack top is zero, 
 *      then set up an array seed at aa, 
 *           replace the zero with the new seed *)
and setup_array_storLoc le ce aa =
    let label = fresh_label ()                                  in  (* stack: [result, result] *)
    let ce    = DUP1                            >>ce            in  (* stack: [result, result] *)
    let ce    = PUSH4(Label label)              >>ce            in  (* stack: [result, result, shortcut] *)
    let ce    = JUMPI                           >>ce            in  (* stack: [result] *)
    let ce    = POP                             >>ce            in  (* stack: [] *)
    let ce    = gen_array_storLoc le ce aa                      in  (* stack: [stor_index] *)
    let ce    = PUSH1 (Int 1)                   >>ce            in  (* stack: [stor_index, 1] *)
    let ce    = SLOAD                           >>ce            in  (* stack: [stor_index, orig_seed] *)
    let ce    = DUP1                            >>ce            in  (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce    = incr_top 1                        ce            in  (* stack: [stor_index, orig_seed, orig_seed + 1] *)
    let ce    = PUSH1 (Int 1)                   >>ce            in  (* stack: [stor_index, orig_seed, orig_seed + 1, 1] *)
    let ce    = SSTORE                          >>ce            in  (* stack: [stor_index, orig_seed] *)
    let ce    = DUP1                            >>ce            in  (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce    = SWAP2                           >>ce            in  (* stack: [orig_seed, orig_seed, stor_index] *)
    let ce    = SSTORE                          >>ce            in  (* stack: [orig_seed] *)
                JUMPDEST label                  >>ce                (* stack: [result] *)

(*   if the stack top is zero, 
 *      then  1.  set up an array seed at aa, 
 *            2.  replace the zero with the new seed *)
and setup_array_storLoc_of_loc le ce = function 
    | Stor storRange    -> 
    assert (storRange.stor_size = Int 1) ;
    let stor_idx = storRange.stor_start                        in       
    let label = fresh_label ()                                 in    (* stack: [result, result] *)
    let ce    = DUP1                            >>ce           in    (* stack: [result, result] *)
    let ce    = PUSH4(Label label)              >>ce           in    (* stack: [result, result, shortcut] *)
    let ce    = JUMPI                           >>ce           in    (* stack: [result] *)
    let ce    = POP                             >>ce           in    (* stack: [] *)
    let ce    = PUSH32 stor_idx                 >>ce           in    (* stack: [stor_index] *)
    let ce    = PUSH1(Int 1)                    >>ce           in    (* stack: [stor_index, 1] *)
    let ce    = SLOAD                           >>ce           in    (* stack: [stor_index, orig_seed] *)
    let ce    = DUP1                            >>ce           in    (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce    = incr_top 1                        ce           in    (* stack: [stor_index, orig_seed, orig_seed + 1] *)
    let ce    = PUSH1(Int 1)                    >>ce           in    (* stack: [stor_index, orig_seed, orig_seed + 1, 1] *)
    let ce    = SSTORE                          >>ce           in    (* stack: [stor_index, orig_seed] *)
    let ce    = DUP1                            >>ce           in    (* stack: [stor_index, orig_seed, orig_seed] *)
    let ce    = SWAP2                           >>ce           in    (* stack: [orig_seed, orig_seed, stor_index] *)
    let ce    = SSTORE                          >>ce           in    (* stack: [orig_seed] *)
                JUMPDEST label                  >>ce                 (* stack: [result] *)

(* le is not updated here.  It can be only updated in
 * a variable initialization *)
and codegen_expr le ce aln      = function 
    | EpAddr(c,TyInstnce i),TyAddr  ->                  (c,TyInstnce i)         >>>>(aln,le,ce) 
    | EpValue       ,TyUint256      ->                  CALLVALUE               >>ce      (* Value (wei) Transferred to the account *) 
    | EpSender      ,TyAddr         ->  let ce      =   CALLER                  >>ce        in
                                                        align_addr ce aln
    | EpArray a     ,ty             ->  assert(aln=R); 
                                        let ce      =   codegen_array le ce (read_array a)  in
                                        begin match ty with
                                        | TyMap _   ->  setup_array_storLoc le ce (read_array a)
                                        | _         ->  ce                                  end 
    | EpThis        ,_              ->  let ce      =   ADDRESS                 >>ce        in
                                                        align_addr ce aln     
    | EpIdent id    ,ty             ->  (match lookup le id with
                                        | Some loc  ->  let ce = push_loc ce aln ty loc in
                                                        begin match ty with
                                                        | TyMap _   -> setup_array_storLoc_of_loc le ce loc
                                                        | _         -> ce                       end
                                        | None      ->  err ("codegen_expr: identifier's location not found: "^id) )
    | EpFnCall call ,retTy          ->                  codegen_call le ce aln call retTy
    | EpSend s      ,_              ->  assert(aln=R);  codegen_send le ce s
    | EpNew n       ,TyInstnce _    ->  assert(aln=R);  codegen_new  le ce n 
    | EpFalse       ,TyBool         ->  assert(aln=R);  PUSH1(Big big_0)        >>ce  
    | EpTrue        ,TyBool         ->  assert(aln=R);  PUSH1(Big big_1)        >>ce  
    | EpDecLit256 d ,TyUint256      ->  assert(aln=R);  PUSH32(Big d)           >>ce  
    | EpDecLit8 d   ,TyUint8        ->  assert(aln=R);  PUSH1(Big d)            >>ce  
    | EpLAnd(l,r)   ,TyBool         ->  assert(aln=R); 
                                        let la      =   fresh_label ()                          in (*                         .. *)
                                        let ce      =   l                       >>>>(R,le,ce)   in (*                    l >> .. *)
                                        let ce      =   DUP1                    >>ce            in (*               l >> l >> .. *)
                                        let ce      =   if_0_GOTO la              ce            in (*                    l >> .. *)
                                        let ce      =   POP                     >>ce            in (*                         .. *)
                                        let ce      =   r                       >>>>(R,le,ce)   in (*                    r >> .. *)
                                        let ce      = JUMPDEST la               >>ce            in (*                    r >> .. *)
                                        let ce      =   ISZERO                  >>ce            in (*             iszero r >> .. *) 
                                                        ISZERO                  >>ce               (*                 l&&r >> .. *)
    | EpNot expr, TyBool            ->  let ce      =   expr                    >>>>(aln,le,ce) in
                                        let ce      =   ISZERO                  >>ce            in 
                                                        align_bool ce aln   
    | EpNow,TyUint256               ->                  TIMESTAMP               >>ce 
    | EpNeq(l,r),TyBool             ->  let ce      =   r                       >>>>(R,le,ce)   in
                                        let ce      =   l                       >>>>(R,le,ce)   in
                                        let ce      =   EQ                      >>ce            in
                                        let ce      =   ISZERO                  >>ce            in
                                                        align_bool ce aln  
    | EpLt(l,r),TyBool              ->  let ce      =   r                       >>>>(R,le,ce)   in
                                        let ce      =   l                       >>>>(R,le,ce)   in
                                        let ce      =   LT                      >>ce            in
                                                        align_bool ce aln            
    | EpPlus(l,r),TyUint256         ->  let ce      =   r                       >>>>(R,le,ce)   in
                                        let ce      =   l                       >>>>(R,le,ce)   in
                                                        ADD                     >>ce 
    | EpPlus(l,r),TyUint8           ->  let ce      =   r                       >>>>(R,le,ce)   in
                                        let ce      =   l                       >>>>(R,le,ce)   in
                                                        ADD                     >>ce 
    | EpMinus(l,r),TyUint256        ->  let ce      =   r                       >>>>(R,le,ce)   in
                                        let ce      =   l                       >>>>(R,le,ce)   in
                                                        SUB                     >>ce 
    | EpMinus(l,r),TyUint8          ->  let ce      =   r                       >>>>(R,le,ce)   in
                                        let ce      =   l                       >>>>(R,le,ce)   in
                                                        SUB                     >>ce 
    | EpMult(l,r),TyUint256         ->  let ce      =   r                       >>>>(R,le,ce)   in
                                        let ce      =   l                       >>>>(R,le,ce)   in
                                                        MUL                     >>ce
    | EpMult(l,r),TyUint8           ->  let ce      =   r                       >>>>(R,le,ce)   in
                                        let ce      =   l                       >>>>(R,le,ce)   in
                                                        MUL                     >>ce
    | EpGt(l,r),TyBool              ->  let ce      =   r                       >>>>(R,le,ce)   in
                                        let ce      =   l                       >>>>(R,le,ce)   in
                                        let ce      =   GT                      >>ce            in
                                                        align_bool ce aln   
    | EpBalance e,TyUint256         ->  let ce      =   e                       >>>>(R,le,ce)   in
                                                        BALANCE                 >>ce
    | EpEq(l,r),TyBool              ->  let ce      =   r                       >>>>(R,le,ce)   in
                                        let ce      =   l                       >>>>(R,le,ce)   in
                                        let ce      =   EQ                      >>ce            in
                                                        align_bool ce aln      
    | EpDeref(ref,tyR),ty           ->  let size    =   size_of_ty ty                           in
                                        assert (size<=32 && tyR=TyRef ty && aln=R) ;   (* assuming word-size *)
                                        let ce      =   (ref,tyR)               >>>>(R,le,ce)   in (* pushes the pointer *)
                                                        MLOAD                   >>ce 
    | _, _                          ->  errc"codegen_expr: Error"

and (>>>>) expr (aln,le,ce)  = codegen_expr le ce aln expr 

and mstore_mthd_args pack (args:ty expr_ty list) le ce =
    let ce    = PUSH1(Int 0)           >>ce                 in  (*                  0 >> ..   *)
                foldl(mstore_mthd_arg le pack)ce args           (*            sumsize >> ..   *) 

and mstore_mthd_arg le pack ce (arg:ty expr_ty) =
    let ty = snd arg in 
    assert (fits_in_one_stor_slot ty) ; 
    let i,a     = match pack with 
                   | ABIPack   -> 32           , R
                   | TightPack -> size_of_ty ty, L              in  (*                                                 sum >> .. *)
    let ce      = PUSH1 (Int i)                 >>ce            in  (*                                         size >> sum >> .. *)
    let ce      = arg                           >>>>(a,le,ce)   in  (*                                  arg >> size >> sum >> .. *)
    let ce      = DUP2                          >>ce            in  (*                          size >> arg >> size >> sum >> .. *)
    let ce      = mem_alloc                       ce            in  (*                   alloc(size) >> arg >> size >> sum >> .. *)
    let ce      = MSTORE                        >>ce            in  (* M[alloc(size)] := arg                   size >> sum >> .. *)
                  ADD                           >>ce                (*                                            size+sum >> .. *)

and mstore_args args le ce =
    let ce      = PUSH1 (Int 0)                 >>ce            in
                  foldl(mstore_arg le)ce args   

and mstore_arg le ce arg =   
    let size    = size_of_ty(snd arg)                           in
    assert(size=32);                                                (*                                                 sum >> .. *)
    let ce      = PUSH1 (Int size)              >>ce            in  (*                                         size >> sum >> .. *)
    let ce      = arg                           >>>>(R,le,ce)   in  (*                                  arg >> size >> sum >> .. *)
    let ce      = DUP2                          >>ce            in  (*                          size >> arg >> size >> sum >> .. *)
    let ce      = mem_alloc                       ce            in  (*                   alloc(size) >> arg >> size >> sum >> .. *)
    let ce      = MSTORE                        >>ce            in  (* M[alloc(size)]:= arg                    size >> sum >> .. *)
                  ADD                           >>ce                (*                                            size+sum >> .. *)

and prepare_input_in_mem le ce s mthd =
    let args    = s.send_args                                   in  (*                                                     >> .. *)   
    let ce      = mstore_hash_of_mthd mthd        ce            in  (*                                         &mhash >> 4 >> .. *)
    let ce      = mstore_args args             le ce            in  (*                              argsize >> &mhash >> 4 >> .. *)
    let ce      = SWAP1                         >>ce            in  (*                              4 >> argsize >> &mhash >> .. *)
    let ce      = SWAP2                         >>ce            in  (*                              4 >> argsize >> &mhash >> .. *)
    let ce      = ADD                           >>ce            in  (*                                 argsize+4 >> &mhash >> .. *)
                  SWAP1                         >>ce                (*                                 &mhash >> argsize+4 >> .. *)

and obtain_ret_values_from_mem ce =                                 (* stack : out size, out offset *)
    let ce      = DUP2                          >>ce            in  (* stack : out size, out offset, out size *)
    let ce      = PUSH1 (Int 32)                >>ce            in  (* stack : out size, out offset, out size, 32 *)
    let ce      = throw_if_NEQ                    ce            in 
    let ce      = MLOAD                         >>ce            in  (* stack : out size, out *)
    let ce      = SWAP1                         >>ce            in  (* stack : out, out size *)
                  POP                           >>ce                (* stack : out *)
      
and codegen_send le ce (s:ty _send) =
    let msg     = s.send_msg    in 
    let cn      = s.send_cntrct in 
    let m       = s.send_mthd   in 
    assert (is_throw_only msg.msg_reentrance    ) ; 
    match snd cn with
    | TyInstnce cnname -> 
        let idx     = lookup_cn_of_ce ce cnname                 in 
        let callee  = cntrct_lookup ce idx                      in
        begin match m with | Some name  -> 
        let m       = lookup_mthd_info ce callee name           in
        let retSize = size_of_ty m.mthd_retTy                   in  (* [pc_bkp] *)
        let ce  = reset_PC                        ce            in  (* [pc_bkp] *)
        let ce  = PUSH1(Int retSize)            >>ce            in  (* [pc_bkp, retsize] *)
        let ce  = DUP1                          >>ce            in  (* [pc_bkp, retsize, retsize] *)
        let ce  = mem_alloc                       ce            in  (* [pc_bkp, retsize, alloc(retsize)] *)
        let ce  = DUP2                          >>ce            in  (* [pc_bkp, retsize, alloc(retsize), retsize] *)
        let ce  = DUP2                          >>ce            in  (* [pc_bkp, retsize, alloc(retsize), retsize, alloc(retsize)] *)
        let ce  = prepare_input_in_mem le ce     s m            in  (* [pc_bkp, retsize, alloc(retsize), retsize, alloc(retsize), insize, alloc(insize)] *)
        let ce  = push_msg_and_gas s           le ce            in  (* [alloc(retsize), retsize] *)
        let ce  = call_and_restore_PC             ce            in  
        let ce  = SWAP1                         >>ce            in  (* [retsize,alloc(retsize)] *)
                  obtain_ret_values_from_mem      ce            end (* [ret] *)
    | TyAddr        ->  
        let retSize    = 0                                      in  
        let ce  = reset_PC                        ce            in  (* [pc_bkp] *)
        let ce  = PUSH1(Int retSize)            >>ce            in  (* [pc_bkp, 0] *)
        let ce  = DUP1                          >>ce            in  (* [pc_bkp, 0, 0] *)
        let ce  = DUP2                          >>ce            in  (* [pc_bkp, 0, 0, 0] *)
        let ce  = DUP2                          >>ce            in  (* [pc_bkp, 0, 0, 0, 0] *)
        let ce  = DUP2                          >>ce            in  (* [pc_bkp, 0, 0, 0, 0, 0] *)
        let ce  = DUP2                          >>ce            in  (* [pc_bkp, 0, 0, 0, 0, 0, 0] *)
        let ce  = push_msg_and_gas s           le ce            in  
        let ce  = call_and_restore_PC             ce            in  
                  POP                           >>ce                (* [0] *)
    | _             -> err "send expr with Wrong type"

and push_msg_and_gas s le ce = 
    let ce = match s.send_msg.msg_value with     
        | None      -> PUSH1(Int 0)             >>ce 
        | Some e    -> e                        >>>>(R,le,ce)   in 
    let ce      = s.send_cntrct                 >>>>(R,le,ce)   in
    let ce      = PUSH4(Int 3000)               >>ce            in
    let ce      = GAS                           >>ce            in
                  SUB                           >>ce           

and call_and_restore_PC ce = 
    let ce      = CALL                          >>ce            in 
    let ce      = PUSH1(Int 0)                  >>ce            in 
    let ce      = JUMPI                         >>ce            in 
    let ce      = SWAP2                         >>ce            in 
                  restore_PC ce 







(*****************************************)
(***     7. CONTRACT CREATION          ***)
(*****************************************)

(*  7.1. Init MemAlloc           *)

let init_mem_alloc ce =                         (* initialize as M[64] := 96 *)
    let ce = PUSH1 (Int 96)                         >>ce    in
    let ce = PUSH1 (Int 64)                         >>ce    in
    let ce = MSTORE                                 >>ce    in
    ce

(*  7.2. CONTRACT PC         *) 

let set_cntrct_pc ce idx =                                      (*                                                       .. *)
    let ce = PUSH32(RntimeCntrctOffset idx)         >>ce    in  (*                                       rn_cn_offset >> .. *) 
    let ce = PUSH32 StorPCIndex                     >>ce    in  (*                             storPC >> rn_cn_offset >> .. *) 
    let ce = SSTORE                                 >>ce    in  (* S[storPC] := rn_cn_offset                             .. *) 
    ce

let get_cntrct_pc ce =
    let ce = PUSH32 StorPCIndex                     >>ce    in
             SLOAD                                  >>ce 

(*  7.3. setup ARGS *) 

(** [mstore_cnstrArgs] copies cnstrArgs at the end of the bytecode into the memory.  
 *  M[0x40] (==M[64]) is increased accordingly.
 *  returns [ memBegin >> memSize >> .. ]            *)
let mstore_cnstrArgs ce cn      =
    let args    = L.map snd (Eth.argTys_of_cntrct cn)       in 
    let size    = Eth.total_size_of_argTys args             in  (*                                                                     .. *)
    let ce      = PUSH32(Int size)                  >>ce    in  (*                                                             size >> .. *)
    let ce      = DUP1                              >>ce    in  (*                                                     size >> size >> .. *)
    let ce      = mem_alloc                           ce    in  (*                                              alloc(size) >> size >> .. *)
    let ce      = DUP2                              >>ce    in  (*                                      size >> alloc(size) >> size >> .. *)
    let ce      = DUP1                              >>ce    in  (*                              size >> size >> alloc(size) >> size >> .. *)
    let ce      = CODESIZE                          >>ce    in  (*                  codesize >> size >> size >> alloc(size) >> size >> .. *)
    let ce      = SUB                               >>ce    in  (*                     codesize-size >> size >> alloc(size) >> size >> .. *)
    let ce      = DUP3                              >>ce    in  (*      alloc(size) >> codesize-size >> size >> alloc(size) >> size >> .. *)
                  CODECOPY                          >>ce        (*          to             from                 alloc(size) >> size >> .. *)

let check_codesize cnidx ce     =  
    let ce      = PUSH32(InitDataSize cnidx)        >>ce    in  (*                                    datasize >> mem_start >> size >> .. *)
    let ce      = CODESIZE                          >>ce    in  (*                        codesize >> datasize >> mem_start >> size >> .. *) 
                  throw_if_NEQ                        ce        (* IF not eq THEN error *) 

let sstore_cnstrArgs ce idx     =                               
    let label   = fresh_label()                             in
    let exit    = fresh_label()                             in  (*                                                mem_start >> size >> .. *)  
    let ce      = check_codesize idx                  ce    in 
    let ce      = PUSH32(StorCnstrctrArgsBegin idx) >>ce    in  (*                                         idx >> mem_start >> size >> .. *)
    let ce   = JUMPDEST label                       >>ce    in  (*                                         idx >> mem_start >> size >> .. *)
    let ce      = DUP3                              >>ce    in  (*                                 size >> idx >> mem_start >> size >> .. *)
    let ce      = if_0_GOTO exit                      ce    in  (*                                         idx >> mem_start >> size >> .. *)   
    let ce      = DUP2                              >>ce    in  (*                            mem_start >> idx >> mem_start >> size >> .. *) 
    let ce      = MLOAD                             >>ce    in  (*                         M[mem_start] >> idx >> mem_start >> size >> .. *)
    let ce      = DUP2                              >>ce    in  (*                  idx >> M[mem_start] >> idx >> mem_start >> size >> .. *)
    let ce      = SSTORE                            >>ce    in  (*S[idx]=M[mem_start]                      idx >> mem_start >> size >> .. *)  
    let ce      = PUSH32(Int 32)                    >>ce    in  (*                                   32 >> idx >> mem_start >> size >> .. *)
    let ce      = SWAP1                             >>ce    in  (*                                   idx >> 32 >> mem_start >> size >> .. *)
    let ce      = SWAP3                             >>ce    in  (*                                   size >> 32 >> mem_start >> idx >> .. *)
    let ce      = SUB                               >>ce    in  (*                                      size-32 >> mem_start >> idx >> .. *)
    let ce      = SWAP2                             >>ce    in  (*                                      idx >> mem_start >> size-32 >> .. *) 
    let ce      = incr_top 1(*word*)                  ce    in  (*                                    idx+1 >> mem_start >> size-32 >> .. *)
    let ce      = SWAP1                             >>ce    in  (*                                    mem_start >> idx+1 >> size-32 >> .. *)
    let ce      = incr_top 32                         ce    in  (*                                 mem_start+32 >> idx+1 >> size-32 >> .. *)
    let ce      = SWAP1                             >>ce    in  (*                                 idx+1 >> mem_start+32 >> size-32 >> .. *)
    let ce      = goto label                          ce    in  (*                                 idx+1 >> mem_start+32 >> size-32 >> .. *)
    let ce   = JUMPDEST exit                        >>ce    in  (*                                         idx >> mem_start >> size >> .. *)
    let ce      = POP                               >>ce    in  (*                                                mem_start >> size >> .. *) 
    let ce      = POP                               >>ce    in  (*                                                             size >> .. *) 
    let ce      = POP                               >>ce    in  (*                                                                     .. *)
    ce
(*  S[0]   : PC                                                               *)  
(*  S[1]   := m   <- array seed                                               *)  
(*  S[2]   := the value of arg1 is stored in message call                     *)  
(*  S[3]   := the value of arg2 is stored in message call                     *)  
(*            ..                                                              *)  
(*  S[k+1] := the value of argk is stored in message call                     *)  
(*  S[k+2] := 1   <- array1                                                   *)  
(*  S[k+3] := 2                                                               *)  
(*   ..                                                                       *)  
(*  S[n+1] := m                                                               *)  






(*  7.4. setup ARRAYs *)                   

(* S[1]     := ARRAY SEED COUNTER   *) 
(* S[loc]   := old seed             *)
(* S[1]     := S[1] + 1             *)
let setup_arraySeed ce (arrLoc: int) =
    let label   = fresh_label()                             in  (*                                                .. *) 
    let ce      = PUSH4 (Int arrLoc)                >>ce    in  (*                                         loc >> .. *)
    let ce      = PUSH4 (Label label)               >>ce    in  (*                                label >> loc >> .. *)
    let ce      = JUMPI                             >>ce    in  (* IF loc != 0 GOTO label                            *) 
    let ce      = PUSH1 (Int 1)                     >>ce    in  (*                                          1  >> .. *)
    let ce      = SLOAD                             >>ce    in  (*                                        S[1] >> .. *)
    let ce      = DUP1                              >>ce    in  (*                                S[1] >> S[1] >> .. *)
    let ce      = PUSH4 (Int arrLoc)                >>ce    in  (*                         loc >> S[1] >> S[1] >> .. *)
    let ce      = SSTORE                            >>ce    in  (* S[loc]:=S[1]                           S[1] >> .. *)
    let ce      = incr_top 1                          ce    in  (*                                      S[1]+1 >> .. *)
    let ce      = PUSH1 (Int 1)                     >>ce    in  (*                                1  >> S[1]+1 >> .. *)
    let ce      = SSTORE                            >>ce    in  (* S[1]:=S[1]+1                                   .. *)
                  JUMPDEST label                    >>ce        (*                                                .. *)

let reset_arraySeed ce = 
    let ce      = PUSH1 (Int 1)                     >>ce    in  (*                                      1 >> .. *)
    let ce      = DUP1                              >>ce    in  (*                                 1 >> 1 >> .. *)
                  SSTORE                            >>ce        (* S[1]:=1                                   .. *) 

let setup_arrays ce cn =
    let ce            = reset_arraySeed ce                  in   
    let arrLocs       = SL.array_locations cn               in  
                        foldl setup_arraySeed ce arrLocs 

(*  S[0]   : PC                                                               *)  
(*  S[1]   := m   <----- array seed                                           *)  
(*  S[2]   :      <- arg1                                                     *)  
(*            ..                                                              *)  
(*  S[k+1] :      <- argk                                                     *)  
(*  S[k+2] := 1    ----+                                                      *)  
(*  S[k+3] := 2        |                                                      *)  
(*  S[k+4] := 3        | arrays                                               *)  
(*   ..                |                                                      *)  
(*  S[n+1] := m    ----+                                                      *)  


(*  7.5. CODECOPY *) 

(** [copy_rntimeCode_to_mem ce cntrcts idx]
 * /adds opcodes to [ce] so that in the final state the memory contains the rntime code
 * for all cntrcts that are reachable from [idx] in the
 * list [cntrcts] in the addresses [code_start, code_start + code_size).
 * returns   [..., code_len, code_start) *)

let mstore_rntimeCode ce idx =                              (*                                                              *)
    let ce    = PUSH32(RntimeCodeSize)          >>ce    in  (*                                                   size >> .. *)
    let ce    = DUP1                            >>ce    in  (*                                           size >> size >> .. *)  
    let ce    = mem_alloc                         ce    in  (*                                    alloc(size) >> size >> .. *)
    let ce    = DUP2                            >>ce    in  (*                            size >> alloc(size) >> size >> .. *)
    let ce    = PUSH32(RntimeCodeOffset idx)    >>ce    in  (*                     idx >> size >> alloc(size) >> size >> .. *)
    let ce    = DUP3                            >>ce    in  (*      alloc(size) >> idx >> size >> alloc(size) >> size >> .. *)
                CODECOPY                        >>ce        (*                                    alloc(size) >> size >> .. *)



(***  7.6.  CONTRACT CREATION          ***)
                            
type cnstrctrCode      =    { cnstrctr_ce           : ce
                            ; cnstrctr_ty           : tyCntrct
                            ; cnstrctr_cn           : ty cntrct         }

let ce_of_cc cc         =   cc.cnstrctr_ce
let program_of_cc       =   extract_program $ ce_of_cc  

let codegen_cnstrctr_bytecode cns idx = (* return ce which contains the program *) 
    let cn      = lookup_index idx cns                      in 
    let ce      = empty_ce (lookup_cn_of_cns cns) cns       in (*                                                                                 *)
    let ce      = init_mem_alloc          ce                in (* M[64] := 96                                                                     *)
    let ce      = mstore_cnstrArgs        ce cn             in (*                                            alloc(argssize) << argssize << ..    *)
    let ce      = sstore_cnstrArgs        ce idx            in (* S[i..i+sz-1]:= argCodes               i << alloc(argssize) << argssize << ..    *)
    let ce      = setup_arrays            ce cn             in (* S[1]        := #array                 i << alloc(argssize) << argssize << ..    *)
    let ce      = set_cntrct_pc           ce idx            in (* S[PC]       := rntime_cn_offst (returned body)                                  *)
    let ce      = mstore_rntimeCode       ce idx            in (*                                      alloc(codesize) << codesize << i <<  ..    *)
                  RETURN                >>ce                   (*                                                                     i <<  ..    *)

(*  S[0]   := runtime_cn_offset (returned body)                               *)  
(*  S[1]   := m   <----- array Seed                                           *)  
(*  S[2]   :      <- arg1                                                     *)  
(*            ..                                                              *)  
(*  S[k+1] :      <- argk                                                     *)  
(*  S[k+2] := 1    ----+                                                      *)  
(*  S[k+3] := 2        |                                                      *)  
(*  S[k+4] := 3        | arrays                                               *)  
(*   ..                |                                                      *)  
(*  S[n+1] := m    ----+                                                      *)  

let compile_cnstrctr cns idx  : cnstrctrCode =
    let cn      = L.assoc idx cns in 
    { cnstrctr_ce           = codegen_cnstrctr_bytecode cns idx
    ; cnstrctr_ty           = typeof_cntrct cn 
    ; cnstrctr_cn           = cn                                }

let compile_cnstrctrs cns : cnstrctrCode idx_list =
    idxmap (compile_cnstrctr cns) cns



(***************************************)
(***     8.    RUNTIME               ***)
(***************************************)
type rntimeCode            =                                              (* what form should the cnstrctr code be encoded? *)
                            { rntime_ce             : ce                   (* 1. pseudo program.      easy                   *)
                            ; rntime_cn_offsets     : int idx_list      }  (* 2. pseudo codegen_env.  maybe uniform          *)

let empty_rntimeCode lookup_cn layouts =
    { rntime_ce             = empty_ce lookup_cn layouts
    ; rntime_cn_offsets     = []                                    }

let init_rntimeCode lookup_cn layouts : rntimeCode =
    let ce    = empty_ce lookup_cn layouts                      in
    let ce    = get_cntrct_pc                             ce    in
    let ce    = JUMP                                    >>ce    in
    { rntime_ce             = ce
    ; rntime_cn_offsets     = [] }



(***************************************)
(***     9.  DISPATHER               ***)
(***************************************)

let dispatcher_usualMthd idx le ce m =                              (*                                           ABCD >> .. *)  
    let ce      =   DUP1                                    >>ce    in  (*                                   ABCD >> ABCD >> .. *)
    let ce      =   push_mthd_hash m                          ce    in  (*                              m >> ABCD >> ABCD >> .. *)
    let ce      =   EQ                                      >>ce    in  (*                             m=ABCD?1:0 >> ABCD >> .. *)
    let ce      =   PUSH32(RntimeMthdLabel(idx,Method m))   >>ce    in  (*                Rntime(m) >> m=ABCD?1:0 >> ABCD >> .. *)
                    JUMPI                                   >>ce        (* if m=ABCD then GOTO Rntime(m)             ABCD >> .. *)

let dispatcher_defaultMthd idx le ce =
    let ce      =   PUSH32(RntimeMthdLabel(idx,Default))    >>ce    in
                    JUMP                                    >>ce     

let push_inputdata32_from b ce =
    let ce      =   PUSH32 b                                >>ce    in
                    CALLDATALOAD                            >>ce

let dispatcher le ce idx cntrct =
    let tyMthds =   L.map (fun x -> x.mthd_head) cntrct.mthds       in
    let uMthds  =   filter_usualMthd tyMthds                        in 
    let ce      =   push_inputdata32_from(Int 0)              ce    in  (*               ABCDxxxxxxxxxxxxxxxxxxxxxxxxxxxxx >> .. *)
    let ce      =   shiftRtop ce Eth.(word_bits-sig_bits)           in  (*                                            ABCD >> .. *)                             
    let ce      =   foldl(dispatcher_usualMthd idx le)ce uMthds     in  (* JUMP to Method ABCD                                   *)   
    let ce      =   POP                                     >>ce    in  (*                                                    .. *)
    let ce      =   if  default_exists tyMthds
                        then dispatcher_defaultMthd idx le    ce        (* JUMP to Default Method                             .. *) 
                        else throw ce                               in  (* JUMP to error                                      .. *) 
    le,ce



(***************************************)
(***     10. SSTORE ARGS             ***)
(***************************************)

let push_args le ce args =
    let ce      =   L.fold_right (fun arg ce -> arg >>>>(R,le,ce)) args ce in 
    le,ce

let sstore_word_to ce stor_loc =
    let ce      =   PUSH32 (Int stor_loc)      >>ce     in
                    SSTORE                     >>ce       

let sstore_words_to ce stor_locs = foldl sstore_word_to ce stor_locs

let sstore_args le ce offset idx args = 
    let cntrct  =   cntrct_lookup ce idx                in 
    let arglocs =   SL.arg_locations offset cntrct      in
    assert(L.length arglocs=L.length args) ; 
    let le,ce   =   push_args       le ce args          in  (*                                              *)
    let ce      =   sstore_words_to ce arglocs          in  (*                                              *)
    le,ce

let set_cont_to_fncall le ce (lyt:SL.storLayout) (fncall, ty_expr) =
    let hd      =   fncall.call_head                    in
    let args    =   fncall.call_args                    in
    let idx     =   lookup_cn_of_ce ce hd               in 
    let offset  =   lyt.stor_cnstrctrArgs_begin idx    in  
    let ce      =   set_cntrct_pc ce idx                in  (* S[PC] := rntime_offset_of_cntrct *) 
                    sstore_args le ce offset idx args

(* set_cont sets the storage contents.
 * So that the next message call would start from the continuation. *)
let set_cont le ce layout (cont, ty) = match cont with
    | EpFnCall fncall    -> set_cont_to_fncall le ce layout (fncall,ty)
    | _                  -> err "strange continuation" 




(*************************************)
(**      11. CODEGEN STMT           **)
(*************************************)

(*       mstore_word ty 
 *
 *     BEFORE           AFTER               
 *
 *                   +---------+            
 *                   |alloc(32)|            
 *   +---------+     +---------+            
 *   |  value  |     |    32   |            
 * --+---------+-- --+---------+--    *)

let mstore_word ty ce =
    assert (size_of_ty ty <= 32)  ;                             (*                                   val >> .. *)
    let ce      = PUSH1(Int 32)             >>ce            in  (*                             32 >> val >> .. *)
    let ce      = DUP1                      >>ce            in  (*                       32 >> 32 >> val >> .. *)
    let ce      = mem_alloc                   ce            in  (*                alloc(32) >> 32 >> val >> .. *)
    let ce      = SWAP2                     >>ce            in  (*                val >> 32 >> alloc(32) >> .. *)
    let ce      = DUP3                      >>ce            in  (*   alloc(32) >> val >> 32 >> alloc(32) >> .. *)
    let ce      = MSTORE                    >>ce            in  (*                       32 >> alloc(32) >> .. *)
                  SWAP1                     >>ce                (*                       alloc(32) >> 32 >> .. *)

let mstore_expr le ce pack (e,ty) =
    let a       = match pack with | ABIPack   -> R
                                  | TightPack -> L          in
    let ce      = (e,ty)                    >>>>(a,le,ce)   in  (*                                value >> .. *)
    let ce      = mstore_word ty ce                         in  (*                      alloc(32) >> 32 >> .. *)
    le,ce

let rec mstore_exprs le ce pack = function 
    | []        ->  let ce    = PUSH1(Int 0)        >>ce    in
                    let ce    = PUSH1(Int 0)        >>ce    in
                    le, ce
    | e::es     ->  let le,ce = mstore_expr le ce pack e    in (*                                      alloc(size) >> size >> .. *)
                    let ce    = SWAP1               >>ce    in (*                                      size >> alloc(size) >> .. *)
                    let le,ce = mstore_exprs le ce pack es  in (*   0 >> 0 >> size' >> alloc(size') >> size >> alloc(size) >> .. *)
                    let ce    = POP                 >>ce    in (*        0 >> size' >> alloc(size') >> size >> alloc(size) >> .. *)       
                    let ce    = ADD                 >>ce    in (*             size' >> alloc(size') >> size >> alloc(size) >> .. *)    
                    let ce    = SWAP1               >>ce    in (*             alloc(size') >> size' >> size >> alloc(size) >> .. *)     
                    le, ce   (* POP                                                           size' >> size >> alloc(size) >> .. *) 
                             (* ADD                                                             size+size'  >> alloc(size) >> .. *) 
                             (* SWAP1                                                           alloc(size) >> size+size'  >> .. *)
                    
let codegen_return le ce layt ret =
    let le,ce   = set_cont le ce layt ret.ret_cont          in
    let ce      = match ret.ret_expr with
    | Some e    ->  let le,ce = mstore_expr le ce ABIPack e in
                                RETURN              >>ce 
    | None      ->              STOP                >>ce    in
    le,ce

let sstore_to_array le ce layt aa = 
    let arr     = aa.arrIdent                               in
    let idx     = aa.arrIndex                               in
    let ce      = idx                       >>>>(R,le,ce)   in   (* stack : [value, index] *)
    let ce      = arr                       >>>>(R,le,ce)   in   (* stack : [value, index, array_seed] *)
    let ce      = keccak_cat                  ce            in   (* stack : [value, kec(array_seed ^ index)] *)
                  SSTORE                    >>ce                

let sstore_to_lexpr le ce layt = function 
    |LEpArray a-> sstore_to_array le ce layt a 

let codegen_assign le ce layt l r =
    let ce      = r                         >>>>(R,le,ce)   in
    let ce      = sstore_to_lexpr le ce layt l              in
    le, ce

let codegen_decl le ce i  = 
    let pos     = stack_size ce                             in
    let name    = i.declId                                  in
    let ce      = i.declVal                 >>>>(R,le,ce)   in
    let le      = add_loc le(name, Stack(pos+1))            in
    le, ce

let rec codegen_if_then le ce layt cond stmts =
    let label   = fresh_label ()                            in
    let ce      = cond                      >>>>(R,le,ce)   in
    let ce      = if_0_GOTO label             ce            in 
    let le,ce   = codegen_stmts stmts layt le ce            in
    let ce      = JUMPDEST label            >>ce            in
    le,ce

and codegen_if le ce layt cond ss1 ss2 =
    let next    = fresh_label()                             in
    let endif   = fresh_label()                             in
    let ce      = cond                      >>>>(R,le,ce)   in
    let ce      = if_0_GOTO next              ce            in 
    let _,ce    = codegen_stmts ss1 layt   le ce            in (* location env needs to be discarded *)
    let ce      = goto endif                  ce            in
    let ce      = JUMPDEST next             >>ce            in
    let _,ce    = codegen_stmts ss2 layt   le ce            in (* location env needs to be discarded *)
    let ce      = JUMPDEST endif            >>ce            in
    le,ce

and codegen_stmts stmts lyt le ce   = foldl (codegen_stmt lyt) (le,ce) stmts
and codegen_stmt layt (le,ce)       = function 
    | SmAbort                       ->  le, throw ce
    | SmReturn ret                  ->  codegen_return      le ce layt ret
    | SmAssign (l,r)                ->  codegen_assign      le ce layt l r
    | SmDecl i                      ->  codegen_decl        le ce i
    | SmIfThen(cond,e)              ->  codegen_if_then     le ce layt cond e 
    | SmIf(cond,e1,e2)              ->  codegen_if          le ce layt cond e1 e2
    | SmSlfDstrct expr              ->  codegen_selfDstrct  le ce expr
    | SmExpr expr                   ->  codegen_expr_stmt   le ce expr
    | SmLog(name,args,Some ev)      ->  codegen_log_stmt    le ce name args ev
    | SmLog(name,args,None)         ->  err "add_stmt: type check first"

and codegen_log_stmt le ce name args evnt =
    let idxArgs,args= split_evnt_args evnt args             in
    let le,ce   = push_args le ce idxArgs                   in
    let ce      = push_evnt_hash  evnt        ce            in
    let le,ce   = mstore_exprs le ce ABIPack args           in (* stack : [..., size, offset] *)
    let n       = L.length idxArgs + 1                      in
    let ce      = log n                     >>ce            in  (* deindexee N in logN *)
    le, ce

and codegen_expr_stmt le ce expr =
    let ce      = expr                      >>>>(R,le,ce)   in
    let ce      = POP                       >>ce            in
    le, ce

and codegen_selfDstrct le ce expr =    
    let ce      = expr                      >>>>(R,le,ce)   in
    let ce      = SELFDESTRUCT              >>ce            in
    le, ce


(********************************************)
(***     12. CODEGEN CONTRACT             ***)
(********************************************)

let label_mthd idx (m:mthd_head) ce =
    let label   =   fresh_label()                               in
    register_entry(Mthd(idx,m))label ; 
                    JUMPDEST label             >>ce      

let calldatasize m =
    let args    = m.mthd_args                                   in
    4 (* for signature *) + Eth.total_size_of_args args   

let codegen_mthd_argLen_chk m ce = match m with  
    | Default       -> ce
    | Method  m     ->
    let ce      = PUSH4(Int(calldatasize m))    >>ce            in
    let ce      = CALLDATASIZE                  >>ce            in
                  throw_if_NEQ                  ce    

let codegen_mthd layout idx (le,ce) m =
    let ce      = label_mthd  idx m.mthd_head         ce        in
    let ce      = codegen_mthd_argLen_chk m.mthd_head ce        in
    let le      = add_emptyEnv                     le           in
    let le      = add_mthd_argLocs    m            le           in
    let le,ce   = codegen_stmts m.mthd_body layout le ce        in
    le,ce

let codegen_mthds layout    = ($$$) foldl codegen_mthd layout 

let codegen_cntrct le ce layt (idx,cntrct) =
    let label   = fresh_label ()                                in      
    register_entry (Cntrct idx) label ;                    
    let ce      = JUMPDEST label                >>ce            in     
    let ce      = init_mem_alloc                  ce            in  (* M[64] := 96                                          *)                                  
    let le,ce   = dispatcher le ce idx cntrct                   in  (*                                                      *)
    let le,ce   = codegen_mthds layt idx (le,ce) cntrct.mthds   in  
    ce


(********************************************)
(***     13. MAKE BYTECODE                ***)
(********************************************)

let append_rntime lyt rc (idx,cn)   =
    { rntime_ce             = codegen_cntrct(rntime_init_le cn)rc.rntime_ce lyt(idx,cn)
    ; rntime_cn_offsets     = insert idx(code_len rc.rntime_ce)rc.rntime_cn_offsets    }

let compile_rntime lyt cns          = 
    let init_rc = init_rntimeCode (lookup_cn_of_cns cns) cns                        in 
    foldl (append_rntime lyt) init_rc cns

let storLayout_of_cnstrctrCode cc   = 
    SL.storLayout_of_cntrct cc.cnstrctr_cn (program_of_cc cc)

let sizes_of_cnstrctrCodes ccs      =
    let lengths = map (code_len $ ce_of_cc) ccs                                     in
    let lengths = idx_sort lengths                                                  in
    L.map snd lengths

let offsets_of_sizes init l         =
    let rec loop offsets current        = function 
        | []            -> L.rev offsets
        | size::rest    -> loop(current::offsets)(current+size)rest                 in 
    loop [] init l 

let storLayout_of_rntimeCode rc ccs =
    let ccs_sizes           = sizes_of_cnstrctrCodes ccs                            in
    let ccs_offsets         = offsets_of_sizes (code_len rc.rntime_ce) ccs_sizes    in
    let ccs_totalsize       = BL.sum ccs_sizes                                      in
    SL. { rn_codesize           = ccs_totalsize + code_len rc.rntime_ce
        ; rn_cn_offsets         = rc.rntime_cn_offsets
        ; rn_cnstrctr_sizes     = to_idx_list ccs_sizes
        ; rn_cnstrctr_offsets   = to_idx_list ccs_offsets }

let concat_programs_rev programs    =
    let rev_programs        = L.rev programs                                        in
    L.concat rev_programs

(** cnstrctrs_packed concatenates cnstrctr code.
 *  Since the code is stored in the reverse order, the concatenation is also reversed. *)
let program_of_cnstrctrs ccs        =
    let programs            = map program_of_cc ccs                                 in
    let programs            = idx_sort  programs                                    in
    let programs            = L.map snd programs                                    in
    concat_programs_rev programs

let compose_bytecode ccs rc idx : big_int Evm.program =
    let cnLayouts           = map storLayout_of_cnstrctrCode  ccs                   in
    let rnLayout            =     storLayout_of_rntimeCode rc ccs                   in
    let layt                = SL.cnstrct_post_storLayout cnLayouts rnLayout         in
    let _cnstrctr           = lookup_index idx ccs                                  in
    let imm_cnstrctr        = SL.realize_program layt idx(program_of_cc _cnstrctr)  in
    let cns_program         = program_of_cnstrctrs ccs                              in 
    let rn_program          = extract_program rc.rntime_ce                          in
    let imm_rntime          = SL.realize_program layt idx(cns_program @ rn_program) in
    (* the code is stored in the reverse order *)
    imm_rntime @ imm_cnstrctr
