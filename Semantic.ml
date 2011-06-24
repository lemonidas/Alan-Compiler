open Types
open Identifier
open Symbol
open Printing
open Error
open Lexing

let check_types op_name type_1 type_2 sp ep=
	match (type_1, type_2) with
	|(TYPE_int, TYPE_int)
	|(TYPE_byte, TYPE_byte)
		-> true
	|(TYPE_int, TYPE_none)
	|(TYPE_none, TYPE_int)
	|(TYPE_byte, TYPE_none)
	|(TYPE_none, TYPE_byte) 
		-> false
	|(TYPE_byte,_ )
	|(_, TYPE_byte) ->
		print_type_error op_name type_1 type_2 TYPE_byte sp ep;
		false
	|_ ->
		print_type_error op_name type_1 type_2 TYPE_int sp ep;
		false

let check_entry_type lval_t id pos arr = 
	match lval_t with
	|TYPE_int
	|TYPE_byte
	|TYPE_array(_,_)
		-> true
	|TYPE_none 
		-> false
	|_ -> error "Identifier (%s%s) has type %s when int or \
			byte was expected, at line %d, position %d"
		id arr (string_of_typ lval_t)
		(pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
		false

let check_lvalue id pos is_array=
	let ent = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
	let lvalue_type = 
		match ent.entry_info with
		|ENTRY_variable (info) -> info.variable_type
		|ENTRY_parameter (info) -> info.parameter_type
		|_ ->
			error "The identifier (%s) does not correspond to \
				a variable or a parameter, at line %d, position %d."
				id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
			TYPE_none
	in if (is_array) 
		then 
			match lvalue_type with 
			|TYPE_array (typ, _) -> 
				(ent, typ,  check_entry_type typ id pos "[_]")
			|_ -> 
				error "The identifier (%s) does not correspond to \
				 	an array, at line %d, position %d."
					id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
				(ent, TYPE_none, false)
		else 
			(ent, lvalue_type, check_entry_type lvalue_type id pos "")

let check_func_call fun_info id param_list pos =
	let rec check_parameters i acc = function
		|([],[]) -> 
			if (fun_info.function_result = TYPE_proc) 
			then acc 
			else (
				error "Too many arguments in function call %s() \
				in line %d, position %d"
				id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
			false
			)
		|([], _) ->
			error "Too many arguments in function call %s() \
				in line %d, position %d"
				id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
			false
		|([_], []) ->
			if (fun_info.function_result = TYPE_byte || 
				fun_info.function_result = TYPE_int)
			then acc
			else (
				error "Too few arguments in function call %s() \
					in line %d, position %d"
					id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
				false
			)
		|(_,[]) ->
				error "Too few arguments in function call %s() \
					in line %d, position %d"
					id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
				false
		|(h1::t1, h2::t2) -> (				
			match h1.entry_info with
			|ENTRY_parameter (par_info) ->
				if (equalType par_info.parameter_type h2)
				then check_parameters (i+1) acc (t1,t2)
				else (
					error "Type Mismatch: Argument number %d in \
						function call %s() has wrong type:\n\
						\tExpected:\t %s\n\
						\tFound:   \t %s\n\
						\tAt line %d.\n"
					i id
					(string_of_typ (par_info.parameter_type))
					(string_of_typ h2)
					(pos.pos_lnum);
					check_parameters (i+1) false (t1,t2)
				)
			|_ ->
				internal "Function parameter not a parameter???";
				raise Terminate
		)
	in
	check_parameters 1 true (fun_info.function_paramlist, param_list)	

let check_array_reference id ttype pos =
	match ttype with
	|TYPE_array (_,_) ->
		warning "Invalid parameter: Array (%s) can only be passed by \
			reference, at line %d, position %d...Fixing automatically"
			id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
		false;
	|_ -> true

let check_first_proc ent =
	if (equalType !currentScope.ret_type TYPE_proc) 
	then (
		closeScope ent;
	)
	else (
		fatal "Invalid program: Main function must have type proc";
		raise Terminate
	)

