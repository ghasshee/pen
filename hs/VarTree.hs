-- Program Graph 


module VarTree where 

-- Variable Tree technique is used for decomposing AST 
-- AST is transformed into a tree whose branch is always a node or variable. 
-- The branch of the tree is tagged by a variable, and variables constst of 3 sorts.
--  * ordinal  variable   -- non-recursive variable
--  * recursive variable  -- 
--  * hidden variable     -- 
--  hidden variable, is not variable in the source code, 
--  as is in the decomposition into the three-address code. 



import GCLL hiding (S,M,Var)
import Node 
import Type
import Term hiding (Bind) 
import Tree
import AST
import Hex
import Data
import Variable


--------------------------
--- DATA DEFINITION    ---
--------------------------


data Symbol     = LETIN
                | LAM
                | COND
                | APP
                | CNR 
                | MTD 
                | DEF 
                | BDY 
                deriving (Show) 

                
data VarNode    = Var Variable 
                | BOp  String
                | UOp  String 
                | Val Integer 
                | Null
                deriving (Show) 



data VarTree    =  VBr Variable Symbol [VarTree] 
                |  VLf VarNode

instance Show VarTree where 
    show vt = "\n" ++ showF "    " [vt] where 
        showF s []                  = ""
        showF s (VLf t:vts)         = s ++ "+-  " ++ show t   ++ "\n" ++ 
                                      showF s vts 
        showF s [VBr x sym vts]     = s ++ "+-  " ++ show x   ++ "\n" ++ 
                                      s ++ "    " ++ "+-  "   ++ show sym ++ "\n" ++ 
                                      showF (s ++ "    ") vts 
        showF s (VBr x sym vs:vts)  = s ++ "+-  " ++ show x   ++ "\n" ++ 
                                      s ++ "|   " ++ "+-  "   ++ show sym ++ "\n" ++ 
                                      showF (s ++ "|   ") vs  ++ 
                                      showF s vts 
                                
            

data  Operator = Undefined 
                deriving (Show)


type NewVar     = Int
type Bind       = [(String, Variable)]  
type Ctx        = (NewVar,Bind) 
type VT         = (VarTree, Ctx)    
type VTs        = ([VarTree], Ctx)   

--mkPG :: CONTRACT -> Edges
--mkPG cn = pgCN cn initialConfig  



initialCtx = (0,[]) 

mkVT  :: CONTRACT -> VT 
mkVT cn = vtCN cn initialCtx


-------------------------
--    Program Graph    --  
-------------------------

-- CN -> vt
vtCN    :: CONTRACT -> Ctx -> VT
vtCN (CN id tops) (i,bind)  = (VBr (H i) CNR vtops, ctx') where 
                                (vtops,ctx') = vtTOPs tops (i+1,bind)

-- [TOP] -> vt 
vtTOPs  :: [TOP] -> Ctx -> VTs
vtTOPs []           ctx     = ([], ctx)
vtTOPs (top:tops)   ctx     = (vtop:vtops, ctx'') where 
                                    (vtop, ctx')    = vtTOP top ctx
                                    (vtops,ctx'')   = vtTOPs tops ctx'

-- TOP -> vt
vtTOP  :: TOP -> Ctx -> VT
vtTOP e@(EV id ty      ) ctx    = undefined 
vtTOP s@(SV id ty      ) (i,b)  = (VLf (Var(S i)), (i+1,(id, S i):b))
vtTOP m@(MT id ty ps bd) (i,b)  = (VBr (M i)MTD(vps ++ [vbd]), ctx'') where
                                    (vps,ctx')  = vtParams ps (i+1,(id,M i):b) 
                                    (vbd,ctx'') = vtBODY bd ctx' 

vtParams :: [Param] -> Ctx -> VTs 
vtParams []      ctx            = ([],ctx)
vtParams (p:ps)  ctx            = (vp:vps, ctx'') where 
                                    (vp, ctx')      = vtParam p ctx
                                    (vps,ctx'')     = vtParams ps ctx'

vtParam  :: Param -> Ctx -> VT 
vtParam (id,ty) (i,b)           = (VLf (Var(X i)), (i+1,(id,X i):b))

vtBODY   :: BODY -> Ctx -> VT 
vtBODY (BODY _ ds tm _) (i,b)   = (VBr (H i)BDY(vds++[vtm]), ctx'') where
                                    (vds, ctx')     = vtDecls ds (i+1,b)
                                    (vtm, ctx'')    = vtTerm  tm ctx'

vtDecls :: [Decl] -> Ctx -> VTs
vtDecls []      ctx             = ([],ctx)
vtDecls (d:ds)  ctx@(i,b)       = case d of 
    FLET id ps tm fm            -> (VBr (H i) LETIN [vf]  : vds , ctx'') where 
                                    (vf ,ctx' )     = vtFUN id ps tm fm (i+1,b)
                                    (vds,ctx'')     = vtDecls ds ctx'
    DATA id is ds               -> undefined 
    SLET id tm _                -> (VBr  s    LETIN [vtm] : vds , ctx'') where 
                                    s               = searchSTO id b 
                                    (vtm,ctx')      = vtTerm  tm ctx
                                    (vds,ctx'')     = vtDecls ds ctx'
    LET  id tm fm               -> (VBr (H i) LETIN [vlet] : vds, ctx'') where 
                                    (i',b')         = (i+2,(id,X(i+1)):b) 
                                    (vtm,(i'',b'')) = vtTerm tm (i',b') 
                                    vlet            = VBr (X(i+1)) DEF [vtm] 
                                    (vds,ctx'')     = vtDecls ds (i'',b') 

vtFUN id ps tm fm ctx@(i,b)     = case recursive (length ps) tm of 
    True                            -> (VBr (R i) LAM (vps++[vtm]), (i'',b')) where
                                            (i',b')         = (i+1,(id,R i):b) 
                                            (vps,ctx'')     = vtParams ps (i',b')
                                            (vtm,(i'',_))   = vtTerm   tm ctx''
    False                           -> undefined 

searchSTO id []             = error (id ++ " is not initialized as a storage variable. ") 
searchSTO id ((s,v):bind)   = if s==id then v else searchSTO id bind 




recursive paramlen tr =
    case tr of 
    RED (TmVAR n) []        -> paramlen == n
    RED (TmAPP  ) [t1,t2]   -> recursive paramlen t1 || recursive paramlen t2 
    RED (TmIF   ) [c,t1,t2] -> recursive paramlen c  || recursive paramlen t1 || recursive paramlen t2
    RED (TmBOP o) [t1,t2]   -> recursive paramlen t1 || recursive paramlen t2
    _                       -> False





vtTerm :: Term -> Ctx -> VT
vtTerm tr ctx@(i,b)                 = case tr of 
    RED TmAPP [t1,t2]           ->  (VBr (H i) APP [vt1, vt2], ctx'') where 
                                        (vt1,ctx')  = vtTerm t1 (i+1,b) 
                                        (vt2,ctx'') = vtTerm t2 ctx' 
    RED (TmVAR  n) []           ->  (VLf (Var (snd $ b !! n)), ctx  ) 
    RED (TmSTO  n) []           ->  (VLf (Var (snd $ b !! n)), ctx  ) 
    RED (TmU256 n) []           ->  (VLf (Val n             ), ctx  )  
    RED (TmDATA d) []           ->  (VLf (Val (data2nat d)  ), ctx  )
    RED (TmIF    ) [c,t1,t2]    ->  (VBr (H i) COND [vb, vt1, vt2], ctx''') where 
                                        (vb ,ctx'  )    = vtTerm c (i+1,b) 
                                        (vt1,ctx'' )    = vtTerm t1 ctx' 
                                        (vt2,ctx''')    = vtTerm t2 ctx''
    RED (TmBOP  o) [t1,t2]      ->  (VBr (H i) APP [vop, vt1, vt2], ctx'') where 
                                        vop             = VLf (BOp o)
                                        (vt1,ctx' )     = vtTerm t1 (i+1,b) 
                                        (vt2,ctx'')     = vtTerm t2 ctx'
    _                           ->  undefined 



    
