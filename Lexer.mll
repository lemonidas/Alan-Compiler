{
open Lexing;;
open Error;;
open Parser;;

(*let found_eof = ref true;;*)

let line_inc lexbuf =
	let pos = lexbuf.Lexing.lex_curr_p in
	lexbuf.Lexing.lex_curr_p <- {
		pos with 
			Lexing.pos_lnum = pos.Lexing.pos_lnum +1;
			Lexing.pos_bol = pos.Lexing.pos_cnum;
	};;

let create_hashtable size init =
	let tbl = Hashtbl.create size in
	List.iter (fun (key,data) -> Hashtbl.add tbl key data) init;
	(tbl);;

exception Lexer_exception of (string);;

(* Caml Inria explode - implode functions *)
let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

let implode l =
  let result = String.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  imp 0 l;;

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
];;

let is_hex ch =
	((ch >= '0' && ch <= '9') || 
	 (ch >= 'a' && ch <= 'f') || 
	 (ch >= 'A' && ch <= 'Z'))

let get_hex_value x =
	if (x>='0' && x<='9')
	then Char.code x - Char.code '0'
	else if (x>='A' && x <= 'F')
	then Char.code x - Char.code 'A'
	else Char.code x - Char.code 'a'

let parse_hex str =
	let h1 = get_hex_value str.[2] in
	let h2 = get_hex_value str.[3] in
	Char.chr (h1*16+h2)

let de_escape str = 
	let length = String.length str in
	let rec loop i acc=
		if (i < length)
		then (
			match str.[i] with
			|'\\' ->  (
				match str.[i+1] with
				|'n' -> loop (i+2) ('\n'::acc)
				|'r' -> loop (i+2) ('\r'::acc)
				|'t' -> loop (i+2) ('\t'::acc)
				|'x' ->
					if ((is_hex str.[i+2]) && (is_hex str.[i+3]))
					then 
						let h1 = get_hex_value str.[i+2] in
						let h2 = get_hex_value str.[i+3] in
						loop (i+4) ((Char.chr (h1*16+h2))::acc)
					else (
						error "Invalid escape sequence"; 
						loop (i+5) acc
					)
				|_ -> error "Invalid escape sequence";
					loop (i+2) acc	
				)									
			|_ -> loop (i+1) (str.[i]::acc)
		)
		else implode (List.rev acc)
	in loop 0 []	

}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id_char = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let white = [' ' '\t' '\r']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']


rule lexer = parse
	|digit+ as num			{T_Const(num)}
	|letter id_char* as id		{
					try Hashtbl.find keywords id
					with Not_found -> T_Id(id)							
					}
	
	|white				{lexer lexbuf}
	|'\n'				{line_inc lexbuf; lexer lexbuf}
	
	|"--"				{singleline_comment lexbuf}
	|"(*"				{multiline_comment 0 lexbuf}
	
	|"+"				{T_Add}
	|"-"				{T_Sub}
	|"/"				{T_Div}
	|"*"				{T_Mult}
	|"%"				{T_Mod}
	|"!"				{T_Not}
	|"|"				{T_Or}
	|"&"				{T_And}
	|">"				{T_Ge}
	|">="				{T_Geq}
	|"<"				{T_Le}
	|"<="				{T_Leq}
	|"="				{T_Set}
	|"=="				{T_Eq}
	|"!="				{T_Neq}
	
	|";"				{T_Semicolon}
	|":"				{T_Colon}
	|","				{T_Comma}
	|"("				{T_LParen}
	|")"				{T_RParen}
	|"["				{T_LSq_Bracket}
	|"]"				{T_RSq_Bracket}
	|"{"				{T_LCur_Bracket}
	|"}"				{T_RCur_Bracket}

	|"\""				{parse_string [] lexbuf}
	|"\'"				{parse_char lexbuf}	
	
		
	|eof 				{T_Eof(*if (found_eof.contents) then (found_eof := false; T_Eof) else raise End_of_file*)}
	|_ as c				{
							let pos = lexbuf.Lexing.lex_curr_p in
							error "Invalid Character '%c' at line %d, position %d." 
								c pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
							lexer lexbuf
						}

and parse_string acc= parse
	|'\n'				{
							let pos = lexbuf.Lexing.lex_curr_p in
							error "Multiline string at line %d" 
								pos.pos_lnum;
							dispose_multiline_string lexbuf
						}
	|eof				{fatal "Unterminated string"; T_Eof}
	|"\""				{let str = implode (List.rev acc) in T_String(str, de_escape str)}
	|_ as c 			{parse_string (c::acc) lexbuf}

and dispose_multiline_string = parse
	|eof 				{ fatal "Unterminated string"; T_Eof }
	|"\""				{ T_String ("","")}
	|_ 					{ dispose_multiline_string lexbuf }

and parse_char = parse
	|'\\' 'x' hex hex '\'' as s	
							{T_Char((String.sub s 0 4), implode [(parse_hex s)])}
	|'\\' 'n' '\''			{T_Char("\\n","\n")}
	|'\\' 'r' '\''			{T_Char("\\r","\r")}
	|'\\' 't' '\''			{T_Char("\\t","\t")}
	|eof					{fatal "Unterminated character at line: %d." lexbuf.lex_curr_p.pos_lnum; T_Eof}
	|_ '\'' as c			{let ch = implode [c.[0]] in T_Char(ch,ch)}
	|_						{
								let pos = lexbuf.Lexing.lex_curr_p in
								error "Invalid escape sequence at line %d, position %d." 
									pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
									lexer lexbuf
							}

and multiline_comment n = parse
	|"*)" 				{if n==0 then lexer lexbuf else multiline_comment (n-1) lexbuf}
	|"(*"				{multiline_comment (n+1) lexbuf}
	|"\n"				{line_inc lexbuf; multiline_comment n lexbuf}
	|eof 				{fatal "Unterminated comment"; T_Eof}
	|_					{multiline_comment n lexbuf}
and singleline_comment = parse
	|'\n'				{line_inc lexbuf; lexer lexbuf}
	|eof 				{T_Eof}
	|_					{singleline_comment lexbuf}


