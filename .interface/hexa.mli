type hex

val empty_hex           : hex
val concat_hex          : hex -> hex -> hex

(* [length_of_hex h] returns how much bytes. Hence; [length_of_hex h] * 2 == [string_of_hex h]. *)
val length_of_hex       : hex -> int

(** [hex_of_big_int b l] returns the hex, which is zero-padded to [2 * l] characters.
 *  If [b] is too big, raises a failure. *)
val hex_of_big_int      : Big_int.big_int -> int -> hex

(** [hex_of_string "0101"] is the hex "0x0101" *)
val hex_of_string       : string -> hex
val string_of_hex       : ?prefix:string -> hex -> string
val pr_hex           : ?prefix:string -> hex -> unit
