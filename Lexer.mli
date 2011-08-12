exception Lexer_exception of string;;

val explode : string -> char list
val implode : char list -> string
val get_hex_value : char -> int 
val lexer : Lexing.lexbuf -> Parser.token
