{-# LANGUAGE BinaryLiterals #-} 


module ExtraData where 



import Data 

import Control.Monad

import Data.Word
import Data.Map.Strict as Map
import Data.Binary.Put
import Data.Binary.Get 



-- 2 bit header : type of extraData 
-- 0b00 : Creation Code 
-- 0b01 : Datatype Definition 
-- 0b10 : --
-- 0b11 : Other Byte Data 


data ExtraData  = EDCr      -- Creation Code 
                | EDDT      -- Datatype Def 
                | EDOther   -- Other Byte Data

data DataStructure  = Pointer Int
                    | Undefnd  

ed2binary EDCr      = "00"
ed2binary EDDT      = "01" 
ed2binary EDOther   = "11"



data EDDatatype = Undefined 









data FieldKind  = FKPtr     -- pointer to anther closure 
                | FKInt     -- immediate integer word
                deriving (Eq, Show) 

data InfoTable  = InfoTable 
    { itNmae    :: !String          
    , itTag     :: !Word8   
    , itFields  :: ![FieldKind]     -- layout of payload
    } deriving (Eq, Show) 

-- A Closure on the heap: pointer to InfoTable + payload words
-- We'll represent payload words uniformly as Word64;
-- pointers are Word64

data Closure = Closure 
    { clInfo :: !InfoTable
    , clPayload :: ![Word64] -- either ptrs or immediates
    } deriving (Eq, Show) 



data Heap = Heap 
    { hMap :: !(Map Word64 Closure) 
    , hNext :: !Word64
    } deriving (Eq, Show) 


emptyHeap :: Heap 
emptyHeap = Heap Map.empty 1


allocClosure :: Heap -> Closure -> (Heap, Word64) 
allocClosure h c = 
    let i = hNext h 
        m' = Map.insert i c (hMap h)
    in (h {hMap = m', hNext = i + 1}, i) 



lookupClosure :: Heap -> Word64 -> Maybe Closure 
lookupClosure h i = Map.lookup i (hMap h) 


sizeOfClosure :: Closure -> Int
sizeOfClosure cl = 1 + 1 + (8 * length (clPayload cl))



putInfoTable :: InfoTable -> Put 
putInfoTable it = do 
    putWord8 (itTag it)
    putWord8 (fromIntegral $ length (itFields it))
    mapM_ (\k -> putWord8 $ case k of FKPtr -> 0; FKInt -> 1)(itFields it) 



getInfoTable = do
    tag <- getWord8 
    fc  <- getWord8 
    fks <- replicateM (fromIntegral fc) 
            (getWord8 >>= \b -> return $ if b==0 then FKPtr else FKInt)
    return $ InfoTable ("ctor_" ++ show tag) tag fks 





type Tag        = Word8 

data IRType     = IRInt Int
                | IRPtr IRType
                | IRStruct [IRType]
                | IRTaggedUnion [(Tag, [IRType])]
                | IRTypeVar String 
                | Self
                deriving (Show, Eq) 


natIR = IRTaggedUnion [(0,[]), (1,[IRPtr (Self)])] 


-- data Tree a = Leaf a | Node (Tree a) a (Tree a)
treeIR = IRTaggedUnion 
            [(0, [IRTypeVar "a"])
            ,(1, [IRPtr Self, IRTypeVar "a", IRPtr Self]) 
            ]


type TypeEnv = Map String IRType 

checkConstructor :: TypeEnv -> (Tag, [IRType]) -> (Tag, [IRType]) -> Bool
checkConstructor env (tagDef, fldsDef) (tagUse, fldsUse) 
    | tagDef /= tagUse  = False
    | otherwise         = and $ zipWith (checkType env) fldsDef fldsUse 

checkType :: TypeEnv -> IRType -> IRType -> Bool
checkType env (IRTypeVar v) t   = maybe False (==t) (Map.lookup v env)
checkType env Self Self         = True
checkType env (IRPtr t)(IRPtr u)= checkType env t u
checkType _     t t'            = t == t'  



