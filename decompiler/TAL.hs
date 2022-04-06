module TAL where 

import Asm 
import Tree



data Data   = INT   Integer 
            | PTR   Integer 
            | LBL   Integer 

