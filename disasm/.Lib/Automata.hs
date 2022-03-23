

data State = State Int 

ini = State 0 

final :: State -> Bool 
final (State 1) = True
final (State 0) = False 

del :: State -> Char -> State 
del (State 0) 'a' = State 0
del (State 0) 'b' = State 1 
del (State 1) 'a' = State 0
del (State 1) 'b' = State 1 


data Automata a' = Automata {
    states  :: [State] ,
    initial :: State , 
    delta   :: State -> a' -> State } 


a = Automata 
        { states = [State 0, State 1] 
        , initial = ini 
        , delta = del } 
