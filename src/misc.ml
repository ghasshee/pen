open Printf 





let rec fst_some f = function 
  | []      ->  None
  | h::t    -> ( match f h with
     | None     -> fst_some f t
     | Some x   -> Some x ) 

let rec change_fst f = function 
  | []      -> None
  | h :: t  -> (match f h with
     | None     -> BatOption.map (fun xs-> h::xs)(change_fst f t)
     | Some n   -> Some (n :: t) ) 






let ($) f g x = f (g x) 


