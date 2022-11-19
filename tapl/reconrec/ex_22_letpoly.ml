let f0 = fun x -> (x,x) in 
let f1 = fun y -> f0 (f0 y) in 
let f2 = fun y -> f1 (f1 y) in 
let f3 = fun y -> f2 (f1 y) in 
let f4 = fun y -> f3 (f1 y) in 
let f5 = fun y -> f4 (f1 y) in 
f5 (fun z -> z) 
