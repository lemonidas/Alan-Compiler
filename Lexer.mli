exception Lexer_exception of string;;

val lexer : Lexing.lexbuf -> Parser.token
