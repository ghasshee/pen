open Printf 

type contract_id                = int
type 'a contract_id_assoc       = (contract_id * 'a) list

let list_to_contract_id_assoc   = function 
    | []        -> [] 
    | l         -> List.combine BatList.(range 0 `To (List.length l - 1)) l  

let map f                       =   List.map (fun(id,x)->(id,f x)) 
let pair_map f                  =   List.map (fun (id, x) -> (id, f id x)) 
let filter_map f                =   BatList.filter_map (fun(id, x)-> BatOption.map (fun ret->(id,ret)) (f x))
let choose_contract  l          =   try   List.assoc l 
  with  Not_found       ->  eprintf "choose_contract: not_found\n%!"; raise Not_found
let print_int_for_cids f        =   List.iter (fun cid -> printf "%d |-> %d, " cid (f cid))
let insert id a orig            =   (id, a)::orig       (* shall I sort it?  Maybe later at once. *)
let lookup_id f l               =   let (id, _) = List.find (fun (_, x) -> f x) l in id 
let empty                       =   []
let cids     l                  =   List.map fst l
let values   l                  =   List.map snd l
