open Types
open Error
open Identifier
open Symbol
open Format
open Parsing
open Lexing

let show_offsets = true

let rec string_of_typ typ =
	match typ with
	|TYPE_none -> "<undefined>"
	|TYPE_int -> "int"
	|TYPE_byte -> "byte"
	|TYPE_proc -> "proc"
	|TYPE_array(et,sz) when sz > 0 -> String.concat "" [(string_of_typ et);("[");(string_of_int sz);("]")]
	|TYPE_array(et,sz) -> String.concat "" [(string_of_typ et);("[]")]

let string_of_pass_mode = function
	|PASS_BY_VALUE -> "V"
	|PASS_BY_REFERENCE -> "R"
	|PASS_RET -> "RET"

let rec pretty_typ ppf typ =
  match typ with
  | TYPE_none ->
      fprintf ppf "<undefined>"
  | TYPE_int ->
      fprintf ppf "int"
  | TYPE_byte ->
      fprintf ppf "byte"
  | TYPE_array (et, sz) ->
      pretty_typ ppf et;
      if sz > 0 then
        fprintf ppf " [%d]" sz
      else
        fprintf ppf " []"
  | TYPE_proc ->
      fprintf ppf "proc"

let pretty_mode ppf mode =
  match mode with
  | PASS_BY_REFERENCE ->
      fprintf ppf "reference "
  | _ ->
      ()
;;

let printSymbolTable () =
  let rec walk ppf scp =
    if scp.sco_nesting <> 0 then begin
      fprintf ppf "scope: ";
      let entry ppf e =
        fprintf ppf "%a" pretty_id e.entry_id;
        match e.entry_info with
        | ENTRY_none ->
            fprintf ppf "<none>"
        | ENTRY_variable inf ->
            if show_offsets then
              fprintf ppf "[%d]" inf.variable_offset
        | ENTRY_function inf ->
            let param ppf e =
              match e.entry_info with
                | ENTRY_parameter inf ->
                   fprintf ppf "%a%a : %a"
                      pretty_mode inf.parameter_mode
                      pretty_id e.entry_id
                      pretty_typ inf.parameter_type
                | _ ->
                    fprintf ppf "<invalid>" in
            let rec params ppf ps =
              match ps with
              | [p] ->
                  fprintf ppf "%a" param p
              | p :: ps ->
                  fprintf ppf "%a; %a" param p params ps;
              | [] ->
                  () in
            fprintf ppf "(%a) : %a"
              params inf.function_paramlist
              pretty_typ inf.function_result
        | ENTRY_parameter inf ->
            if show_offsets then
              fprintf ppf "[%d]" inf.parameter_offset
        | ENTRY_temporary inf ->
            if show_offsets then
              fprintf ppf "[%d]" inf.temporary_offset in
      let rec entries ppf es =
        match es with
          | [e] ->
              fprintf ppf "%a" entry e
          | e :: es ->
              fprintf ppf "%a, %a" entry e entries es;
          | [] ->
              () in
      match scp.sco_parent with
      | Some scpar ->
          fprintf ppf "%a\n%a"
            entries scp.sco_entries
            walk scpar
      | None ->
          fprintf ppf "<impossible>\n"
    end in
  let scope ppf scp =
    if scp.sco_nesting == 0 then
      fprintf ppf "no scope\n"
    else
      walk ppf scp in
  printf "%a----------------------------------------\n"
    scope !currentScope
;;

let print_type_error op_name t1 t2 exp_t sp ep =
	error
		"Type Mismatch: Operator (%s) and operand don't agree\n\
		\tOperator Domain:\t%s * %s\n\
		\tOperand:\t\t%s * %s\n\
		\tIn expression starting at line %d position %d, ending\
    	 at line %d position %d."
	(op_name)
	(string_of_typ exp_t) (string_of_typ exp_t)
	(string_of_typ t1) (string_of_typ t2)
	(sp.pos_lnum) (sp.pos_cnum - sp.pos_bol)
	(ep.pos_lnum) (ep.pos_cnum - ep.pos_bol)
;;

let print_unary_type_error op_name t pos =
	error 
		"Type Mismatch: Unary Operator (%s) and operand don't agree\n\
		\tOperator Domain:\t int
		\tOperand Domain: \t %s
		\tIn expression at line %d, position %d."
	(op_name)
	(string_of_typ t)
	(pos.pos_lnum) (pos.pos_cnum - pos.pos_bol)
;;
