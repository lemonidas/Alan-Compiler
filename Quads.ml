open Types
open Error
open Symbol
open Printing
open Identifier
open Semantic
open Lexing

(* Necessary Type Declarations *)
type quad_elem_t =
	|Quad_none
	|Quad_entry of Symbol.entry
	|Quad_valof of Symbol.entry
	|Quad_int of string
	|Quad_char of string
	|Quad_string of string	

let string_of_quad_elem_t = function
	|Quad_none -> ""
	|Quad_entry(ent) -> id_name ent.entry_id
	|Quad_valof(ent) -> Printf.sprintf "[%s]" (id_name ent.entry_id)
	|Quad_int(str) -> str
	|Quad_char(str) -> str
	|Quad_string(str) -> Printf.sprintf "\"%s\"" str

type quad_t =
	|Quad_dummy (* For optimization purposes *)
	|Quad_unit of Symbol.entry
	|Quad_endu of Symbol.entry
	|Quad_calc of string * quad_elem_t * quad_elem_t * Symbol.entry
	|Quad_set of quad_elem_t * Symbol.entry
	|Quad_array of quad_elem_t * quad_elem_t * Symbol.entry
	|Quad_cond of string * quad_elem_t * quad_elem_t * (int ref)
	|Quad_jump of (int ref)
	|Quad_call of Symbol.entry
	|Quad_par of quad_elem_t * Symbol.pass_mode
	|Quad_ret
	
type expr_ret_type = {
	code : quad_t list;
	place : quad_elem_t;
}

type cond_ret_type = {
	c_code : quad_t list;	
	q_true: int ref list;
	q_false : int ref list;
}

let is_entry quad =
	match quad with
	| Quad_entry(_) -> true
	| _ -> false

(* Handling [x] case *)
let dereference x = 
	match x.code with
	|(Quad_array(_, _, ent)::_) ->
		{x with place = Quad_valof(ent)}
	|_ -> x

(* Main function to convert a quad to a string *)
let string_of_quad_t = function
	|Quad_unit(ent) -> 
		Printf.sprintf "unit, %s, -, -"
		(id_name ent.entry_id)
	|Quad_endu(ent) -> 
		Printf.sprintf "endu, %s, -, -" 
		(id_name ent.entry_id)
	|Quad_calc (op, q1, q2, ent) ->
		Printf.sprintf "%s, %s, %s, %s"
			(op)
			(string_of_quad_elem_t q1)
			(string_of_quad_elem_t q2)
			(id_name ent.entry_id)
	|Quad_set(q,ent) ->
		Printf.sprintf ":=, %s, -, %s" 
			(string_of_quad_elem_t q)
			(id_name ent.entry_id)
	|Quad_array(q1, q2, e) ->
		Printf.sprintf "array, %s, %s, %s"
			(string_of_quad_elem_t q1)
			(string_of_quad_elem_t q2)
			(id_name e.entry_id)
	|Quad_cond(op, q1, q2, i) ->
		Printf.sprintf "%s, %s, %s, %d"
			(op)
			(string_of_quad_elem_t q1)
			(string_of_quad_elem_t q2)
			!i
	|Quad_jump(i) ->
		Printf.sprintf "jump, -, -, %d" !i
	|Quad_call(ent) ->
		Printf.sprintf "call, -, -, %s"
			(id_name ent.entry_id)
	|Quad_par(q,pm) ->
		Printf.sprintf "par, %s, %s, -"
			(string_of_quad_elem_t q)
			(string_of_pass_mode pm)
	|Quad_ret -> "ret, -, -, -"	
	|Quad_dummy -> ""

(* Few small functions for compact code later *)
let get_type = function
	|Quad_none -> TYPE_none
	|Quad_int (_) -> TYPE_int
	|Quad_char(_) -> TYPE_byte
	|Quad_string (str) -> TYPE_array(TYPE_byte, String.length str)
	|Quad_valof (ent) 
	|Quad_entry (ent) -> 
		match ent.entry_info with
		|ENTRY_none -> TYPE_none
		|ENTRY_variable (info) -> info.variable_type
		|ENTRY_parameter (info) -> info.parameter_type
		|ENTRY_function (info) -> info.function_result
		|ENTRY_temporary (info) -> info.temporary_type

let extract_entry = function
	|Quad_entry (ent) -> ent
	|Quad_valof (ent) -> ent
	|_ -> internal "Not an entry"; raise Terminate

let get_id = function
	|Quad_none -> internal "proc func call"; raise Terminate
	|Quad_int (i) -> i
	|Quad_char (c) -> c
	|Quad_string (s) -> s
	|Quad_valof (ent)
	|Quad_entry (ent) -> id_name ent.entry_id

(* Returning a "null" quad - error handling mostly *)
let return_null () = {code = []; place = Quad_none}

(* Handle an arithmetical expression *)
let handle_expression op e1 e2 (sp,ep) =
	let t1 = get_type e1.place in
	let t2 = get_type e2.place in
	if (check_types op t1 t2 sp ep)
	then let temp = newTemporary t1 in {
		code  =	Quad_calc(op,e1.place, e2.place, temp)::(e1.code)@(e2.code);
		place =	Quad_entry(temp);
	}
	else return_null ()

(* Handle signs in expressiong *)
let handle_unary_expression op exp pos =
	let t = get_type exp.place in
	if (t==TYPE_int) 
	then match op with
		|"+" -> 
			exp
		|"-" -> 
			let temp = newTemporary TYPE_int in
			let new_quad = Quad_calc("-",Quad_int("0"), exp.place, temp) in
				{ code = (new_quad :: exp.code); place =  Quad_entry(temp) }
		|_ -> internal "wrong unary expression"; raise Terminate
	else (
		print_unary_type_error op t pos;
		return_null ()
	)

(* Handle L-Values *)
let handle_simple_lvalue id pos =
	let (ent, _, correct) = check_lvalue id pos false in
	if (correct) 
		then {code = []; place = Quad_entry(ent)}
	else return_null ()

(* Handle an array lvalue *)
let handle_array_lvalue id pos context q_t =
	let t = get_type q_t.place in
	if (t==TYPE_int) 
		then let (ent, l_typ, correct) = check_lvalue id pos true in
		if (correct) 
			then let temp = newTemporary l_typ in
			let new_quad =
				 Quad_array(Quad_entry(ent), q_t.place, temp) in
			{code = new_quad::q_t.code ; place = Quad_entry(temp)}
		else return_null ()
	else let sp = fst context and ep = snd context in  
		error "Array index must be an integer in expression starting \
			at line %d, position %d and ending at line %d, position %d."
		(sp.pos_lnum) (sp.pos_cnum - sp.pos_bol)
		(ep.pos_lnum) (ep.pos_cnum - ep.pos_bol);
		return_null ()

(* Ugliest function yet - Handle function calls *)
let handle_func_call id pos expr_list =
	let ent = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
	let rec create_code_list acc = function
		|([],_,_) -> 
			acc
		|(h::t, hp::tp, hq::tq) -> (
			match hp.entry_info with
			|ENTRY_parameter (par_info) ->
				let new_quad = Quad_par (hq,par_info.parameter_mode) in
				create_code_list (new_quad::h@acc) (t, tp, tq)
			|_ -> internal "Function parameter not a parameter???";
				raise Terminate	
			)
		|_ -> internal "Create code should be called with correct args";
			raise Terminate		
	in let rec unzip_expr_list acc1 acc2 acc3 = function
		|[] ->
			 (acc1, List.rev acc2, List.rev acc3)
		|(h::t) ->
			unzip_expr_list (h.code::acc1) (h.place::acc2)
						 ((get_type h.place)::acc3) t
	in let (code_list, param_list, type_list) = 
		unzip_expr_list [] [] []  expr_list in
	match ent.entry_info with
	|ENTRY_function (info) ->
		if (check_func_call info id type_list pos)
		then (
			let new_code = create_code_list [] 
				(code_list, info.function_paramlist, param_list)
			in 
			match (info.function_result) with
			|TYPE_proc ->
				{code = Quad_call(ent)::new_code; place = Quad_none}
			|TYPE_int
			|TYPE_byte -> 
				let temp = newTemporary info.function_result in 
				let par_q = Quad_par ( Quad_entry(temp) , PASS_RET) in {
					code = Quad_call(ent)::par_q::new_code;
					place = Quad_entry(temp)
				}
			|_ -> return_null ()					
			)
		else 
			return_null ()
	|_ ->		
		error "Invalid Function call. Identifier %s is not a function \
			at line %d, position %d."
		id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
		return_null ()
	
(* Handle Comparisons *)
let handle_comparison op e1 e2 (sp,ep) =
	let t1 = get_type e1.place in
	let t2 = get_type e2.place in
	if (check_types op t1 t2 sp ep) 
	then
		let true_ref = ref 2 in
		let false_ref = ref 1 in
		let code_true = (Quad_cond(op, e1.place, e2.place, true_ref))
		in let code_false = (Quad_jump (false_ref)) in {
			c_code = code_false::code_true::e2.code@e1.code;
			q_true = [true_ref];
			q_false = [false_ref];
		}
	else (
		internal "Can't recover from condition error";
		raise Terminate
	)
;;		(*Error recovery after condition error?*)
	
(* Handle boolean values *)
let handle_cond_const is_true = 
	let x = ref 1 in {
		c_code = [Quad_jump(x)];
		q_true = if (is_true) then [x] else [];
		q_false = if (is_true) then [] else [x];
	}

(* Handle an "and" cond *)
let handle_and c1 c2 =
	let len = List.length c2.c_code in
	List.iter (fun x -> x := !x + len) c1.q_false;
	{ 
		c_code = c2.c_code @ c1.c_code;
		q_true = c2.q_true;
		q_false = c1.q_false @ c2.q_false;
	}

(* Handle an "or" cond *)
let handle_or c1 c2 = 
	let len = List.length c2.c_code in
	List.iter (fun x -> x := !x + len) c1.q_true;
	{
		c_code = c2.c_code @ c1.c_code;
		q_true = c1.q_true @ c2.q_true;
		q_false = c2.q_false;
	}

(* Handle assignmenet *)
let handle_assignment lval expr (sp,ep) =
	let t1 = get_type lval.place in
	let t2 = get_type expr.place in
	if (check_types "=" t1 t2 sp ep) 
	then 
		let new_quad = 
			match lval.place with
			|Quad_entry (ent) -> (Quad_set(expr.place,ent))
			|_ -> internal "Assigning to something not an entry";
				raise Terminate
		in new_quad::lval.code@expr.code
	else []

(* Handle if statement *)
let handle_if_stmt cond stmt =
	let len = List.length stmt in
	List.iter (fun x -> x := !x + len) cond.q_false;
	stmt @ cond.c_code

(* Handle if-else statement *)
let handle_if_else_stmt cond s1 s2 =
	let l1 = List.length s1 in
	let l2 = List.length s2 in
	let new_quad = Quad_jump (ref (l2+1)) in
	List.iter (fun x -> x := !x + l1 + 1) cond.q_false;
	s2 @ (new_quad::(s1 @ cond.c_code))

(* Handle while statement *)
let handle_while_stmt cond stmt = 
	let l = List.length stmt in
  let lc = List.length cond.c_code in
	List.iter (fun x -> x := !x + l + 1) cond.q_false;
	let new_quad = Quad_jump (ref (-l-lc)) in
	new_quad :: (stmt @ cond.c_code)

(* Handle a return expressiong *)
let handle_return_expr expr pos=
	let t = get_type expr.place in
	if (equalType t !currentScope.ret_type) 
	then let ret_entry = lookupEntry (id_make "$$") LOOKUP_CURRENT_SCOPE true
		in Quad_ret ::(Quad_set(expr.place, ret_entry)):: expr.code
	else (
		error "Attempting to return %s when %s was expected, \
			in line %d, position %d" 
			(string_of_typ t) (string_of_typ !currentScope.ret_type)
			(pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
		[]
	)

(* Proc return *)
let handle_return_proc pos =
	if (equalType TYPE_proc !currentScope.ret_type)
	then
		[Quad_ret]
	else (
		error "Attemping to return proc when %s was expected, \
			in line %d, position %d"
			(string_of_typ !currentScope.ret_type)
			(pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
		[]
	)

(* Function definitions *)
let handle_func_def id local_def stmt =
	let ent = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
	let s_quad = Quad_unit(ent) in
	let e_quad = Quad_endu(ent) in
	e_quad :: (stmt @ (s_quad :: local_def))

(*Semantic needed here *)
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

let check_func_expr func_ret pos =
	match func_ret.place with
	|Quad_entry(_) -> func_ret
	|Quad_none -> 
		error "Function has proc return value and is used as an \
			expression at line %d, position %d"
		(pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
		return_null ()
	|_ -> internal "Function returns neither entry or proc"; raise Terminate

	
	
