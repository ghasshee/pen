open Imm

type stor_location   = int 


type 'imm code_range    =
                        { code_start        : 'imm      (* byte *)
                        ; code_size         : 'imm   }

type 'imm stor_range    =
                        { stor_start        : 'imm      (* word as in EVM *)
                        ; stor_size         : 'imm      (* word *)  }

type 'imm mem_range     =
                        { mem_start         : 'imm      (* byte as in EVM *)
                        ; mem_size          : 'imm      (* byte *)  }

type 'imm volatile_loc  =
                        | Memory        of 'imm mem_range
                        | Stack         of int          (* [Stack 0] is the deepest element in the stack. *)

type 'imm cached_stor   =
                        { cached_original   : 'imm stor_range
                        ; modified          : bool      (* if the cache has to be written again *)
                        ; cache             : 'imm volatile_loc  }

type calldata_range     =
                        { calldata_start    : int
                        ; calldata_size     : int   }


(* location *) 
type location           =
                        | Code          of imm code_range
                        | Stor          of imm stor_range
                        | CachedStor    of imm cached_stor
                        | Volatile      of imm volatile_loc
                        | Calldata      of calldata_range
                        | Stack         of int

let string_of_location           = function 
    | Stor _                -> "Storage ..."
    | CachedStor _          -> "CachedStorage ..."
    | Volatile _            -> "Volatile ..."
    | Code _                -> "Code ..."
    | Calldata c            -> Printf.sprintf "Calldata offset %d, size %d" c.calldata_start c.calldata_size
    | Stack i               -> Printf.sprintf "Stack %d" i

