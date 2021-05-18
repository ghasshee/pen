val word_bits                   :   int
val signature_bits              :   int

type interface_typ              =
  | InterfaceUint of int
  | InterfaceBytes of int
  | InterfaceAddress
  | InterfaceBool

(* size of values of the interface type in bytes *)
val interface_typ_size          :   interface_typ -> int

type interface_arg              =   string * interface_typ

val interpret_interface_type    :   Syntax.typ -> interface_typ    (* e.g. uint -> InterfaceUnit 256 *) 
val to_typ                      :   interface_typ -> Syntax.typ

(* return a string used to compute the method signatures. Addresses are "address", uint is "uint256". *)
val string_of_interface_type    :   interface_typ -> string     

type function_signature         =   { sig_return : interface_typ list
                                    ; sig_name : string
                                    ; sig_args : interface_typ list  }

val get_interface_typs          :   Syntax.arg list -> (string * interface_typ) list
val arguments_with_locations    :   Syntax.typ Syntax.case -> (string * Location.location) list
val constructor_arguments       :   Syntax.typ Syntax.contract -> (string * interface_typ) list
val arrays_in_contract          :   Syntax.typ Syntax.contract -> (string * Syntax.typ * Syntax.typ) list
val total_size_of_interface_args :  interface_typ list -> int

(* returns the Keccak-256 hash of a string without prefix [0x]. *)
val string_keccak               :   string -> string

(* input a hexString & returns the Keccak-256 hash without prefix [0x]. *)
val hex_keccak                  :   string -> string

val strip_0x                    :   string -> string

(* [keccak_short "pay(address)"] returns the method signature code (commonly used in the ABI. *)
val keccak_signature            :   string -> string

(* returns the signature of a fucntion, ingredient of the function hash. Like "pay(address)" *)
val case_header_signature_string :  Syntax.usual_case_header -> string

(* takes a string like `f(uint8,address)` & returns a 4byte signature hash used in ABI. *)
val compute_signature_hash      :   string -> string

(* returns the method signature used in the common ABI, without 0x *)
val case_header_signature_hash  :   Syntax.usual_case_header -> string

val event_signature_hash        :   Syntax.event -> string
val hex_to_big_int              :   string -> Big_int.big_int
val pr_abi                   :   Syntax.typ Syntax.toplevel Assoc.contract_id_assoc -> unit
