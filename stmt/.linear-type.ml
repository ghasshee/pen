(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*         ------------------------------------------                                                                                           *)
(*              sgm*pi           sgm                                                                                                            *)
(*              x : Addr   |-  cn : Addr                                                                                                        *)
(*           -----------------------------------------                                                                                          *)
(*                    pi       sgm  pi                                  sgm'                                                                    *)
(*              |-  \x:Addr.cn  : (x:Addr)->Addr               |- myaddr : Addr                                                                 *)
(*            ----------------------------------------------------------------------------------------                                          *)
(*                                  pi              sgm                                                                                         *)
(*                            |- (\x:Addr.cn) myaddr : Addr                                                                                     *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*            -------------------------------------------                                                                                       *)
(*               the function has the reentrance guard                                                                                          *)
(*           --------------------------------------------------                                                                                 *)
(*               the function must be used once                                                                                                 *)
(*         ------------------------------------------                                                                                           *)
(*              sgm*1            sgm                                                                                                            *)
(*              x : Addr   |-  cn : Addr                                                                                                        *)
(*           -----------------------------------------                                                                                          *)
(*                    1        sgm  1                                   sgm'                                                                    *)
(*              |-  \x:Addr.cn  : (x:Addr)->Addr               |- myaddr : Addr                                                                 *)
(*            ----------------------------------------------------------------------------------------                                          *)
(*                                  1               sgm                                                                                         *)
(*                            |- (\x:Addr.cn) myaddr : Addr                                                                                     *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(* contract WalletLibrary {                                                                                                                     *)
(*      address owner;                                                                                                                          *)
(*                                                                                                                                              *)
(*      // called by constructor                                                                                                                *)
(*      fun{1} initWallet(address _owner) {      /// `fun{n} foo(){}`  means " function foo can be called at most n times "                     *)
(*          owner = _owner;                                                                                                                     *)
(*          // ... more setup ...                                                                                                               *)
(*      }                                                                                                                                       *)
(*                                                                                                                                              *)
(*      fun changeOwner(address _new_owner) external {  // we can use this function only twice                                                  *)
(*          if (msg.sender == owner) {                                                                                                          *)
(*              owner = _new_owner;                                                                                                             *)
(*          }                                                                                                                                   *)
(*      }                                                                                                                                       *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(* contract WalletLibrary {                                                                                                                     *)
(*      address owner;                                                                                                                          *)
(*                                                                                                                                              *)
(*      // called by constructor                                                                                                                *)
(*      function initWallet(address _owner) {                                                                                                   *)
(*          owner = _owner;                                                                                                                     *)
(*          // ... more setup ...                                                                                                               *)
(*      }                                                                                                                                       *)
(*                                                                                                                                              *)
(*      function changeOwner(address _new_owner) external {                                                                                     *)
(*          if (msg.sender == owner) {                                                                                                          *)
(*              owner = _new_owner;                                                                                                             *)
(*          }                                                                                                                                   *)
(*      }                                                                                                                                       *)
(*                                                                                                                                              *)
(*      function () payable {                                                                                                                   *)
(*          // ... receive money, log events, ...                                                                                               *)
(*      }                                                                                                                                       *)
(*                                                                                                                                              *)
(*      function withdraw(uint amount) external returns (bool success) {                                                                        *)
(*          if (msg.sender == owner) {                                                                                                          *)
(*              return owner.send(amount);                                                                                                      *)
(*          } else {                                                                                                                            *)
(*              return false;                                                                                                                   *)
(*          }                                                                                                                                   *)
(*      }                                                                                                                                       *)
(* }                                                                                                                                            *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
open Misc
open Codegen 

let reentrance_guard n mhash ce = 
    let ce = PUSH4(Int n)           >> ce in  (*                                              n >> .. *)
    let ce = PUSH4(Int mhash)       >> ce in  (*                                        3 >>  n >> .. *)
    let ce = SSTORE                 >> ce in  (* S[mhash] := n                                  .. *)
    ce ;;

let dispatch_to_mthd mhash ce = 
    let ce = PUSH(Int mhash)        >> ce in 
    let ce = sdecr   mhash             ce in  (* S[mhash]--                                     >> .. *)
    let ce = if_zero_throw             ce in 
    ce 

let sdecr n ce =
    (* if n = infty (special value e.g. -1 )  then cannot decrement n *) 
    (* if n = 0                               then cannot decrement n *)  
    (* if 0 < n && n < 2^16                   then n--                *) 
    ce 

let whileLOOP ce cond body = 
    let label   = fresh_label()                             in 
    let exit    = fresh_label()                             in 
    let ce      = JUMPDEST label                    >>ce    in 
    let ce      = cond exit                           ce    in 
    let ce      = body                                ce    in 
    let ce      = goto label                          ce    in  (*                                 idx+1 >> mem_start+32 >> size-32 >> .. *)
                  JUMPDEST exit                     >>ce   
    

(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)
(*                                                                                                                                              *)