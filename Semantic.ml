open Types
open Identifier
open Symbol
open Printing
open Error
open Lexing
open QuadTypes

(* Semantic checking of values in binary expressions *)
let check_types op_name type_1 type_2 sp ep=
	match (type_1, type_2) with

  (* Same kind of types in expression are correct *)
	|(TYPE_int, TYPE_int)
	|(TYPE_byte, TYPE_byte)
		-> true

  (* If anything is Type_none a message has allready been created *)
	|(TYPE_int, TYPE_none)
	|(TYPE_none, TYPE_int)
	|(TYPE_byte, TYPE_none)
	|(TYPE_none, TYPE_byte) 
		-> false

  (* If TYPE_byte is found yield that as the "correct" one *)
	|(TYPE_byte,_ )
	|(_, TYPE_byte) ->
		print_type_error op_name type_1 type_2 TYPE_byte sp ep;
		false

  (* Default is to expect Int in expressions *)
	|_ ->
		print_type_error op_name type_1 type_2 TYPE_int sp ep;
		false

(* Semantic checking of entries *)
let check_entry_type lval_t id pos arr = 
	match lval_t with
  (* Entries can be ints, bytes or arrays *)
	|TYPE_int
	|TYPE_byte
	|TYPE_array _
		-> true
	|TYPE_none 
		-> false
  (* Yield an error in case of anything else *)
	|_ -> error "Identifier (%s%s) has type %s when int or \
			byte was expected, at line %d, position %d"
		id arr (string_of_typ lval_t)
		(pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
		false

(* Semantic check for L-Values *)
let check_lvalue id pos is_array=

  (* Extract entry from id *)
	let ent = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in

  (* L-Value must be either a variable or a parameter *)
	let lvalue_type = 
		match ent.entry_info with
		|ENTRY_variable (info) -> info.variable_type
		|ENTRY_parameter (info) -> info.parameter_type
		|_ ->
			error "The identifier (%s) does not correspond to \
				a variable or a parameter, at line %d, position %d."
				id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
			TYPE_none in

  (* If the lvalue corresponds to an array then extract the inner type *)
	if (is_array) 
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

(* Semantic check of a function Call 
 * Important: Return parameter is passed as a parameter by reference,
 * therefore expect one more parameter in such a case               *)
let check_func_call fun_info id param_list pos =

  (* In the tuple, the first argument consists of the parameters of 
   * the function (as registered in the symbol table) and the second
   * argument contains the parameters the function call is made with *)
	let rec check_parameters i acc = function
    
    (* Both lists empty simultaneously:
     * Correct only for TYPE_proc       *)
		|([],[]) -> 
			if (fun_info.function_result = TYPE_proc) 
			then acc 
			else (
				error "Too many arguments in function call %s() \
				in line %d, position %d"
				id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
			false
			)
  
    (* Under no circumstances can the parameters called be more *)
		|([], _) ->
			error "Too many arguments in function call %s() \
				in line %d, position %d"
				id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
			false

    (* In case of TYPE_byte or TYPE_int return type then we have
     * an extraneous parameter to account for                   *)
		|([_], []) ->
			if (fun_info.function_result = TYPE_byte || 
				  fun_info.function_result = TYPE_int )
			then acc
			else (
				error "Too few arguments in function call %s() \
					in line %d, position %d"
					id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
				false
			)

    (* More than 2 arguments less is also invalid *)
		|(_,[]) ->
				error "Too few arguments in function call %s() \
					in line %d, position %d"
					id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
				false

    (* Main recursive case *)
		|(h1::t1, h2::t2) -> (				
			match h1.entry_info with

      (* Extract the type of the "correct" parameter *)
			|ENTRY_parameter (par_info) ->
      
        (* Check for type correctness *)
				if (equalType par_info.parameter_type h2)

        (* If semantically correct continue recursing *)
				then check_parameters (i+1) acc (t1,t2)

        (* Otherwise print an ML-like error message *)
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
    
      (* I'd be really surprised if this happens... *)
			|_ ->
				internal "Function parameter not a parameter???";
				raise Terminate
		)
	in
	check_parameters 1 true (fun_info.function_paramlist, param_list)	

(* Check that all arrays are passed by reference *) 
let check_array_reference id ttype pos =
	match ttype with
	|TYPE_array (_,_) ->
		warning "Invalid parameter: Array (%s) can only be passed by \
			reference, at line %d, position %d...Fixing automatically"
			id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
		false;
	|_ -> true

(* Ensure that the first function (program) is proc *)
let check_first_proc ent =
	if (equalType !currentScope.sco_ret_type TYPE_proc) 
	then (
		closeScope ent;
	)
	else (
		fatal "Invalid program: Main function must have type proc";
		raise Terminate
	)

(* Ensure that a function is proc and return its code *)
let check_func_proc func_ret pos =
	match func_ret.place with
	|Quad_none -> func_ret.code
	|Quad_entry(ent) ->
		error "Function %s has non-proc return value \
				at line %d, position %d."
		(id_name ent.entry_id)
		(pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
		[]
	|_ -> internal "Function returns neither entry or proc"; raise Terminate

(* Otherwise ensure the function has a return type and return the code with 
 * the return location *)
let check_func_expr func_ret pos =
	match func_ret.place with
	|Quad_entry(_) -> func_ret
	|Quad_none -> 
		error "Function has proc return value and is used as an \
			expression at line %d, position %d"
		(pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
		return_null ()
	|_ -> internal "Function returns neither entry or proc"; raise Terminate
