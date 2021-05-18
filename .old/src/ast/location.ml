type 'imm memory_range =
    { memory_start : 'imm     (* byte as in EVM *)
    ; memory_size  : 'imm     (* byte *)  }

type 'imm storage_range =
    { storage_start : 'imm    (* word as in EVM *)
    ; storage_size :  'imm    (* word *)  }

type 'imm code_range =
    { code_start : 'imm       (* byte *)
    ; code_size  : 'imm   }

type 'imm volatile_location =
    | Memory      of 'imm memory_range
    | Stack       of int                (* [Stack 0] is the deepest element in the stack. *)

type 'imm cached_storage =
    { cached_original : 'imm storage_range
    ; modified        : bool            (* if the cache has to be written again *)
    ; cache           : 'imm volatile_location  }

type calldata_range     =
    { calldata_offset     : int
    ; calldata_size       : int   }

type location           =
    | Storage       of PseudoImm.pseudo_imm storage_range
    | CachedStorage of PseudoImm.pseudo_imm cached_storage
    | Volatile      of PseudoImm.pseudo_imm volatile_location
    | Code          of PseudoImm.pseudo_imm code_range
    | Calldata      of calldata_range
    | Stack         of int

let as_string           = function 
    | Storage _             -> "Storage ..."
    | CachedStorage _       -> "CachedStorage ..."
    | Volatile _            -> "Volatile ..."
    | Code _                -> "Code ..."
    | Calldata c            -> Printf.sprintf "Calldata offset %d, size %d" c.calldata_offset c.calldata_size
    | Stack i               -> Printf.sprintf "Stack %d" i
