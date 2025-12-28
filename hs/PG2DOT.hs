{-# LANGUAGE OverloadedStrings #-} 
module PG2DOT where 

import PG 
import OR 
import Mat
import Edge
import Utils 
import Action
import Semiring 
import Data.GraphViz
import Data.GraphViz.Attributes.Complete 
import Data.GraphViz.Printing 
import Data.GraphViz.Commands.IO
import Data.GraphViz.Commands 
import Data.Text.Lazy (Text, pack, unpack ) 



-- Matrix (Edge Int (OR Action)) -> DOT 

    
pg2dot :: Matrix (OR Action) -> DotGraph Int 
pg2dot a@(M n m _ _ _ _) = graphElemsToDot params ns es' where 
    ns  =  [ (i, nodeAttrs i) | i <- [1..n]]  
    es  =  [ (i,j, a!(i,j) ) | i <- [1..n] , j <- [1..n] ]
    es' =  filter (not . iszero . thd3 ) es 
    params = nonClusteredParams 
        { fmtNode = (\(n,l)       -> nodeAttrs n)  
        , fmtEdge = (\(i,j,el)    -> edgeAttrs el)  
        , globalAttributes = 
            [ GraphAttrs [RankDir FromTop] ] } 






nodeAttrs :: Int -> [Attribute] 
nodeAttrs 1 = [shape Circle, textLabel "Entry"]
nodeAttrs 2 = [shape DoubleCircle, textLabel "Exit"] 
nodeAttrs n = [shape Circle, textLabel $ pack $ show n] 


edgeAttrs :: OR Action -> [Attribute] 
edgeAttrs a = [textLabel $ pack (show a)] 



renderPG :: Matrix (OR Action) -> String
renderPG m = 
          unpack
        . renderDot
        . toDot 
        $ pg2dot m 

writeDot :: FilePath -> Matrix (OR Action) -> IO FilePath 
writeDot f a = do 
    runGraphviz (pg2dot a) (Jpeg) f 
 
