module ABI where 


import AST
import Type 
import Utils


ty2ABI :: Ty -> String
ty2ABI TyU256           =   "uint256"
ty2ABI TyU8             =   "uint8"  
ty2ABI TyADDR           =   "address"
ty2ABI TyB32            =   "bytes32"
ty2ABI TyBOOL           =   "bool"   
ty2ABI TyUNIT           =   "void"   
ty2ABI e                =   err $ show e 



fallback2ABI :: String 
fallback2ABI = 
    "{"                                                 ++ "\n" ++
    "\t" ++ "\"inputs\"             : [],"              ++ "\n" ++
    "\t" ++ "\"outputs\"            : [],"              ++ "\n" ++
    "\t" ++ "\"stateMutability\"    : \"payable\","     ++ "\n" ++
    "\t" ++ "\"type\"               : \"fallback\""     ++ "\n" ++
    "}"


param2ABI :: Param -> String
param2ABI (id,ty) =  
    "{"                                                        ++ "\n" ++  
    "\t" ++ "\"name\"               : \" ++ id        ++ "\"," ++ "\n" ++  
    "\t" ++ "\"type\"               : \" ++ ty2ABI ty ++ "\""  ++ "\n" ++ 
    "}"


params2ABI :: Params -> String 
params2ABI []         = [] 
params2ABI [p]        = param2ABI p   
params2ABI (p:ps)     = param2ABI p ++ "," ++ "\n" ++ 
                        params2ABI ps 


ret2ABI :: Ty -> String
ret2ABI ty = 
    "{"                                                         ++ "\n" ++  
    "\t" ++ "\"name\"               : \"\""            ++ "\"," ++ "\n" ++  
    "\t" ++ "\"type\"               : \"" ++ ty2ABI ty ++ "\"," ++ "\n" ++ 
    "}"


top2ABI :: TOP -> String 
top2ABI (MT __end__ _ _ _) = JUST fallback2ABI
top2ABI (MT id ty ps bd)   = JUST $ 
    "{" 
    "\t" ++ "\"name\"               : \"" ++ id             ++ "\"," ++ "\n" ++
    "\t" ++ "\"inputs\"             : [" ++ params2ABI ps   ++ "],"  ++ "\n" ++ 
    "\t" ++ "\"outputs\"            : [" ++ ret2ABI ty      ++ "],"  ++ "\n" ++ 
    "\t" ++ "\"stateMutability\"    : \"payable\""          ++ ","   ++ "\n" ++
    "\t" ++ "\"type\"               : \"function\""                  ++ "\n" ++ 
    "}" 
top2ABI (EV id ty )         = Nothing 
top2ABI _                   = Nothing 



tops2STs :: [TOP] -> [TOP] 
tops2STs []              = [] 
tops2STs (ST id ty:tops) = ST id ty: tops2STs tops 
tops2STs (top:tops)      = tops2STs tops 

cr2ABI (CN id tops) = 
    "{" 
    "\t" ++ "\"name\"               : \"" ++ id             ++ "\"," ++ "\n" ++
    "\t" ++ "\"inputs\"             : [" ++ params2ABI ps   ++ "],"  ++ "\n" ++ 
    "\t" ++ "\"outputs\"            : [" ++ ret2ABI ty      ++ "],"  ++ "\n" ++ 
    "\t" ++ "\"stateMutability\"    : \"payable\""          ++ ","   ++ "\n" ++
    "\t" ++ "\"type\"               : \"function\""                  ++ "\n" ++ 
    "}"
    


    {--




let prABI_cnstrctr (TmCn(id,flds,_)) =
    sprintf "{
\"inputs\"  : [%s], 
\"name\"    : \"%s\", 
\"outputs\" : [], 
\"payable\" : true,
\"type\"    : \"constructor\"
}" (prABI_inputs (L.filter non_mapping_arg flds)) (id)

let prABI_cntrct seen_cnstrctr (TmCn(id,flds,mthds)) = 
    let strs : str list  =   L.map prABI_mthd mthds in
    let strs                =   if !seen_cnstrctr then strs else prABI_cnstrctr (TmCn(id,flds,mthds)) :: strs in
    seen_cnstrctr := true; 
    BS.concat "," strs


let prABI_evnt_arg = function TyEvVar(id,ty,visible) ->  
    sprintf "{\"name\":\"%s\",\"type\":\"%s\",\"indexed\":%s}"
                 id (abi_str_of_ty ty) (str_of_bool visible)

let prABI_evnt_inputs (is:ty list) : str =
    let strs : str list  = L.map prABI_evnt_arg is in
    BS.concat "," strs

let prABI_evnt = function TyEv(id,tyEvArgs) -> 
    sprintf "{\"type\":\"evnt\",\"inputs\":[%s],\"name\":\"%s\"}"
        (prABI_evnt_inputs tyEvArgs) id

let prABI_toplevel seen_cnstrctr = function 
    | TmCn(id,fs,ms)           -> prABI_cntrct seen_cnstrctr (TmCn(id,fs,ms))
    | TmEv e                   -> prABI_evnt e

let prABI (tops : ty toplevel ilist) : unit =
    let seen_cnstrctr    = ref false in
    let ()                  = printf "[" in
    let strs : str list  = L.map (prABI_toplevel seen_cnstrctr) (values tops) in
    let ()                  = printf "%s" (BS.concat "," strs) in
    printf "]"
--}
