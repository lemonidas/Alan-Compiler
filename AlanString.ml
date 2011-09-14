open Error

(* Caml Inria explode - implode functions *)
let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let implode l =
  let result = String.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  imp 0 l

(* Functions for parsing hex characters *)
let is_hex ch =
  ((ch >= '0' && ch <= '9') || 
   (ch >= 'a' && ch <= 'f') || 
   (ch >= 'A' && ch <= 'Z'))

let get_hex_value x =
  if (x>='0' && x<='9')
  then Char.code x - Char.code '0'
  else if (x>='A' && x <= 'F')
  then Char.code x - Char.code 'A' + 10
  else Char.code x - Char.code 'a' + 10

let parse_hex str =
  let h1 = get_hex_value str.[2] in
  let h2 = get_hex_value str.[3] in
  Char.chr (h1*16+h2)

(* Takes a string and returns it without escape characters *)
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
        |'0' -> loop (i+2) ((Char.chr 0)::acc)
        |'\'' -> loop (i+2) ('\''::acc)
        |'"' -> loop (i+2) ('"'::acc)
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

(* Handle escape sequences for Final Code Creation *)
let handle_escapes str =
  let char_list = explode str in
  let base = List.rev (explode "\tdb '") in
  let rec delete_empty_strings char_list acc =  
    match char_list with 
    | [] -> implode acc
    | ('\n'::'\''::'\''::' '::'b'::'d'::'\t'::t) -> 
      delete_empty_strings t acc
    | (h::t) -> 
      delete_empty_strings t (h::acc) in
  let rec parse_char_list acc lst = 
    match lst with
    | [] -> 
      let tail_lst = List.rev (explode "'\n\tdb 0\n") in
      delete_empty_strings (tail_lst@acc) []
    | ('\\'::'t'::t) ->
      let to_add = List.rev (explode "'\n\tdb 9\n\tdb '") in
      parse_char_list (to_add @ acc) t 
    | ('\\'::'n'::t) ->
      let to_add = List.rev (explode "'\n\tdb 10\n\tdb '") in
      parse_char_list (to_add @ acc) t 
    | ('\\'::'r'::t) ->
      let to_add = List.rev (explode "'\n\tdb 13\n\tdb '") in
      parse_char_list (to_add @ acc) t 
    | ('\\'::'0'::t) ->
      let to_add = List.rev (explode "'\n\tdb 0\n\tdb '") in
      parse_char_list (to_add @ acc) t 
    | ('\\'::'\\'::t) ->
      let to_add = List.rev (explode "'\n\tdb \\\n\tdb '") in
      parse_char_list (to_add @ acc) t 
    | ('\\'::'\''::t) ->
      let to_add = List.rev (explode "'\n\tdb 39\n\tdb '") in
      parse_char_list (to_add @ acc) t 
    | ('\\'::'"'::t) ->
      let to_add = List.rev (explode "'\n\tdb 34\n\tdb '") in
      parse_char_list (to_add @ acc) t 
    | ('\\'::'x'::n1::n2::t) ->
      let code = (get_hex_value n1 * 16 + get_hex_value n2) in
      let to_add_str = Printf.sprintf "'\n\tdb %d\n\tdb '" code in
      let to_add = List.rev (explode to_add_str) in
      parse_char_list (to_add @ acc) t
    | (h::t) ->
      parse_char_list (h::acc) t in
  parse_char_list base char_list
 
