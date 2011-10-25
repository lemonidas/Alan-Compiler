{
open AlanString
open Lexing
open Error
open Parser

(* Function to increase line count in lexbuf *)
let line_inc lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- {
    pos with 
      Lexing.pos_lnum = pos.Lexing.pos_lnum +1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
  }

(* Creation of the keywords hashtable *)
let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key,data) -> Hashtbl.add tbl key data) init;
  (tbl)

let keywords = create_hashtable 10 [
  ("byte", T_Byte);
  ("else", T_Else);
  ("false", T_False);
  ("if", T_If);
  ("int", T_Int);
  ("proc", T_Proc);
  ("reference", T_Ref);
  ("return", T_Return);
  ("while", T_While);
  ("true", T_True)
]

}

let digit   = ['0'-'9']
let letter  = ['a'-'z' 'A'-'Z']
let id_char = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let white   = [' ' '\t' '\r']
let hex     = ['0'-'9' 'a'-'f' 'A'-'F']

(* Main Parsing match *)
rule lexer = parse

  (* Digits are passed as strings *)
  |digit+ as num      
              {T_Const(num)}

  (* Identifiers are searched in the keyword hashtable.
   * If found - the keyword is return, otherwise the 
   * identifier itself is as a string *)
  |letter id_char* as id    
              {
                try Hashtbl.find keywords id
                with Not_found -> T_Id(id)              
              }
  
  (* Whitespaces are skipped, line feeds increase line count *)
  |white      {lexer lexbuf}
  |'\n'       {line_inc lexbuf; lexer lexbuf}
  
  (* Comments are handled seperately *)
  |"--"       {singleline_comment lexbuf}
  |"(*"       {multiline_comment 0 lexbuf}
  
  (* Operators and Other Symbols *)
  |"+"        {T_Add}
  |"-"        {T_Sub}
  |"/"        {T_Div}
  |"*"        {T_Mult}
  |"%"        {T_Mod}
  |"!"        {T_Not}
  |"|"        {T_Or}
  |"&"        {T_And}
  |">"        {T_Ge}
  |">="       {T_Geq}
  |"<"        {T_Le}
  |"<="       {T_Leq}
  |"="        {T_Set}
  |"=="       {T_Eq}
  |"!="       {T_Neq}
  
  |";"        {T_Semicolon}
  |":"        {T_Colon}
  |","        {T_Comma}
  |"("        {T_LParen}
  |")"        {T_RParen}
  |"["        {T_LSq_Bracket}
  |"]"        {T_RSq_Bracket}
  |"{"        {T_LCur_Bracket}
  |"}"        {T_RCur_Bracket}

  (* Strings and Constant Chars handled seperately *)
  |"\""       {parse_string [] lexbuf}
  |"\'"       {parse_char lexbuf}  
  
  (* EOF *)  
  |eof        {T_Eof}
  
  (* Any other character is invalid - raise error *)
  |_ as c     {
                let pos = lexbuf.Lexing.lex_curr_p in
                error "Invalid Character '%c' at line %d, position %d." 
                  c pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
                lexer lexbuf
              }

(* Handle strings *)
and parse_string acc= parse
  (* Invalid characters are line feeds and eofs *)
  |'\n'       {
                let pos = lexbuf.Lexing.lex_curr_p in
                error "Multiline string at line %d" 
                  pos.pos_lnum;
                dispose_multiline_string lexbuf
              }
  |eof        {fatal "Unterminated string"; T_Eof}
  
  (* When end of string is found check for correctness *)
  |"\""       {
                let str = implode (List.rev acc) in 
                T_String(str, de_escape str)
              }
  
  (* Any other character is added to the accumulator *)
  |'\''       {warning "\' characters must be escaped. Ignoring.."; 
               parse_string acc lexbuf}
  |_ as c     {parse_string (c::acc) lexbuf}

(* When line feed is found in the middle of the string:
 * 1 - It is unterminated and so we reach eof or another string 
 * 2 - It is deliberately multiline and so we notify the 
       programmer and parse to the next '"'                  *)
and dispose_multiline_string = parse
  |eof        { fatal "Unterminated string"; T_Eof }
  |"\""       { T_String ("","")}
  |_          { dispose_multiline_string lexbuf }

(* Handle escaped characters in chars *)
and parse_char = parse
  |'\\' 'x' hex hex '\'' as s  
                      {T_Char((String.sub s 0 4), implode [(parse_hex s)])}
  |'\\' 'n' '\''      {T_Char("\\n","\n")}
  |'\\' 'r' '\''      {T_Char("\\r","\r")}
  |'\\' 't' '\''      {T_Char("\\t","\t")}
  |'\\' '0' '\''      {T_Char("\\0","\000")}
  |'\\' '\'' '\''     {T_Char("\\\'","\'")}
  |'\\' '"' '\''      {T_Char("\\\"","\"")}
  |eof                {
                        fatal "Unterminated character at line: %d." 
                          lexbuf.lex_curr_p.pos_lnum; 
                        T_Eof
                      }
  |_ '\'' as c        {let ch = implode [c.[0]] in T_Char(ch,ch)}
  
  (* Anything else is invalid *)
  |_          {
                let pos = lexbuf.Lexing.lex_curr_p in
                error "Invalid escape sequence at line %d, position %d." 
                  pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
                lexer lexbuf
              }

(* Multi Line Comment Handling - Extra care for Nesting *)
and multiline_comment n = parse
  |"*)"       {if n==0 then lexer lexbuf else multiline_comment (n-1) lexbuf}
  |"(*"       {multiline_comment (n+1) lexbuf}
  |"\n"       {line_inc lexbuf; multiline_comment n lexbuf}
  |eof        {fatal "Unterminated comment"; T_Eof}
  |_          {multiline_comment n lexbuf}

(* Single line comment Handling *)
and singleline_comment = parse
  |'\n'       {line_inc lexbuf; lexer lexbuf}
  |eof        {T_Eof}
  |_          {singleline_comment lexbuf}


