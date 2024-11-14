module Bifurcation where 


import Edge 


data Bifurcation node a = Branch (Path node a) (Bifurcation node a) 
                        | Path   (Path node a)  


type Path node a = [Edge node a] 










