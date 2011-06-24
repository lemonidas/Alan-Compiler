open Quads
open Symbol
open Types
open Error
open Identifier
open FinalTypes

(* A simple set to hold all library functions used *)
module Ostring = struct
	type t = string
	let compare = compare
end

module Sstring = Set.Make(Ostring)

let lib_functions = ref Sstring.empty

(* Start code *)
let starting_code main_label= 
	Printf.sprintf "\
	xseg\tsegment\tpublic code\n\
	\t\tcs : xseg, ds : xseg, ss : xseg\n\
	\tassume\n\
	\torg\t100h\n\
	main\tproc\tnear\n\
	\tcall\tnear ptr %s\n\
	\tmov\tax, 4C00h\n\
	\tint\t21h\n\
	main endp\n"
		main_label

(* End code *)
let end_code = "\
xseg ends\n\
\tend  main\n"

(* Hash table for function labels *)
let func_hash = Hashtbl.create 42

(* Stack for function calls and nesting *)
let func_stack = Stack.create ()

(* Queue for constant string handling *)
let string_queue = Queue.create ()
let queue_len = ref 0 
let add_string str = 
	incr(queue_len);
	Queue.push str string_queue;
	Printf.sprintf "@str%d" (!queue_len)

(* Ref for unique label creation *)
let label_id = ref 0 

(* Registers the label for function p *)
let set_name p = 
	incr (label_id);
	let id = id_name p.entry_id in
	let s = Printf.sprintf "_%s_%d" id !label_id in
	Hashtbl.add func_hash p s

(* Calls the above function if quad is unit *)
let register_quad = function
	| Quad_unit (p) -> set_name p
	| _ -> ()

(* Gets the name registered for function p *)
let get_name p = 
	flush_all ();
	try
		Hashtbl.find func_hash p
	with 
		Not_found -> (*Library function*)
			let s = Printf.sprintf "_%s" (id_name p.entry_id) in
			lib_functions := Sstring.add s !lib_functions;
			s

(* End label of function p is the label of p with a @ *)
let endof p =
	let s = Hashtbl.find func_hash p in
	Printf.sprintf "@%s" s

(* Find the size of a single parameter *)
let find_single_parameter_size ent =
	match ent.entry_info with
	| ENTRY_parameter (info) ->
		sizeOfType(info.parameter_type)
	| _ -> internal "Function parameter not a parameter"; raise Terminate

(* Find the size of the paremeters of a function f *)
let find_parameter_size f =
	match f.entry_info with
	| ENTRY_function (info) -> 
		let dec_amt = if (info.function_result = TYPE_proc) then 0 else 2 in
		List.fold_left (fun x y -> x + (find_single_parameter_size y)) 
				0 info.function_paramlist - dec_amt
	| _ -> internal "Function not a function entry"; raise Terminate

(* Abbreviation *)
let label n = Printf.sprintf "@%d" n

(* Update Links *)
let update_AL callee called =
	if (callee.entry_scope.sco_nesting < called.entry_scope.sco_nesting)
	then 
		Printf.sprintf "\tpush\tbp\n"
	else if (callee.entry_scope.sco_nesting = called.entry_scope.sco_nesting)
	then 
		Printf.sprintf "\tpush\tword ptr [bp+4]\n"
	else 
		let n = callee.entry_scope.sco_nesting - called.entry_scope.sco_nesting in
		let rec walk i acc =
			if (i < n) 
			then
				walk (i+1) ("\tmov\tsi, word ptr [si+4]\n"::acc)
			else String.concat "" (List.rev (("\tpush\tword ptr [si+4]\n")::acc))
		in walk (n-1) ["\tmov\tsi, word ptr [bp+4]\n"]

(* Bool function to find if entry is local *)
let local ent = 
	let c = Stack.top func_stack in	
	ent.entry_scope.sco_nesting = c.entry_scope.sco_nesting + 1

(* Function to convert type to "word"/"byte" *)
let size_description = function
	|TYPE_byte -> "byte"
	|TYPE_int -> "word"
	|TYPE_array(_) -> "word" (*FIXME*)
	|_ -> internal "Attempting to load wrong thingy";
		raise Terminate

(* Extracts information from entry *)
let get_info = function
	|ENTRY_variable (info) ->
		let sd = size_description info.variable_type in
		let offset = info.variable_offset in
		(sd, offset, false)
	|ENTRY_temporary (info) ->
		let sd = size_description info.temporary_type in
		let offset = info.temporary_offset in
		(sd, offset, false)
	|ENTRY_parameter (info) ->
		let sd = size_description info.parameter_type in
		let offset = info.parameter_offset in
		let mode = (info.parameter_mode = PASS_BY_REFERENCE)
		in (sd,offset,mode)
	|_ -> internal "Getting information from something not a parameter";
		raise Terminate

(* Get AR function *)
let get_ar ent = 
	let base = "\tmov\tsi, word ptr [bp+4]\n" in
	let c =  (Stack.top func_stack) in 
	let na = ent.entry_scope.sco_nesting in
	let nc = c.entry_scope.sco_nesting + 1 in
	let rec loop acc = function
		| 0 -> String.concat "" (List.rev acc)
		| n -> 
			Printf.printf "Calling loop with %d\n" n;
			flush_all();
			loop ("\tmov\tsi, word ptr [si+4]\n"::acc) (n-1)
	in loop [base] (nc - na - 1)

(* Load helper function *)
let rec load reg q = 
	flush_all();
	match q with
	|Quad_none -> ""
	|Quad_int(str) -> 
		Printf.sprintf "\tmov\t%s, %s\n" reg str
	|Quad_char(str) ->
		Printf.sprintf "\tmov\t%s, %d\n" reg (Char.code str.[0])
	|Quad_string(str)->	
		internal "Cannot Load a String in a register"; raise Terminate
	|Quad_entry(ent) -> (
		let (size, offset, mode) = get_info ent.entry_info in
		match ((local ent), mode) with
		|(true,true) ->
			Printf.sprintf  "\tmov\tsi, word ptr [si%+d]\n\
							 \tmov\t%s, %s ptr [si]\n"
				offset reg size 
		|(true,false) ->
			Printf.sprintf "\tmov\t%s, %s ptr [bp%+d]\n"
				reg size offset
		|(false,true) ->
			let ar = get_ar ent in
			Printf.sprintf "%s\tmov\tsi, word ptr [si%+d]\n\
							  \tmov\t%s, %s ptr [si]\n"
				ar offset reg size
		|(false, false) ->
			let ar = get_ar ent in
			Printf.sprintf "%s\tmov\t%s, %s ptr [si%+d]\n"
				ar reg size offset
		)
	|Quad_valof (ent) ->
		Printf.sprintf "%s\tmov\t%s, %s ptr [di]\n"
			(load "di" (Quad_entry(ent))) reg "word" (*FIXME*)

(* Load address helper function *)
let load_addr reg q =
	match q with
	|Quad_entry (ent) -> (
		let (size,offset,mode) = get_info ent.entry_info in
		match ((local ent), mode) with
		|(true,true) ->
			Printf.sprintf "\tmov\t%s, word ptr [bp%+d]\n"
				reg offset
		|(true,false) ->
			Printf.sprintf "\tlea\t%s, %s ptr [bp%+d]\n"
				reg size offset
		|(false, true) ->
			Printf.sprintf "%s\tlea\t%s, %s ptr [si%+d]\n"
				(get_ar ent) reg size offset
		|(false, false) ->
			Printf.sprintf "%s\tmov\t%s, word ptr [si%+d]\n"
				(get_ar ent) reg offset
		)
	|Quad_valof(ent) -> 
		load reg (Quad_entry(ent))
	|Quad_string(str) -> 
		let addr = add_string str in
		Printf.sprintf "\tlea\t%s, byte ptr %s\n"
			reg addr
	|_ -> internal "Loading address of non entry/valof/string"; 
		raise Terminate

(* Store helper function *)
let store reg ent =
	let (size, offset, mode) = get_info ent.entry_info in
		match ((local ent), mode) with
		|(true,true) ->
			Printf.sprintf "\tmov\tsi, word ptr [bp%+d]\n\
						    \tmov\t%s ptr [si], %s\n"
				offset size reg
		|(true,false) ->
			Printf.sprintf "\tmov\t%s ptr [bp%+d], %s\n"
				size offset reg
		|(false,true) ->
			let ar = get_ar ent in
			Printf.sprintf "%s\tmov\tsi, word ptr [si%+d]\n\
							  \tmov\t%s ptr [si], %s\n"
				ar offset size reg
		|(false, false) ->
			let ar = get_ar ent in
			Printf.sprintf "%s\tmov\t%s ptr [si%+d], %s\n"
				ar size offset reg

(* Main function to convert a quad to string of final code *)
let convert_to_string = function
	|Quad_set(q,e) ->
		Printf.sprintf "%s%s" (load "ax" q) (store "ax" e)
	|Quad_array(q1,q,e2) ->
		let e1 = match q1 with
		|Quad_entry x -> x
		|_ -> internal "Array not an lvalue..."; raise Terminate
		in let size = 
			match e1.entry_info with
			|ENTRY_variable(info)-> sizeOfArrayElem info.variable_type
			|ENTRY_parameter(info) ->sizeOfArrayElem info.parameter_type
			|_ -> internal "Called array with not an array"; raise Terminate
		in
		String.concat "" 
			[(load "ax" q);
			 (Printf.sprintf "\tmov\tcx, %d\n" size);
			 ("\timul\tcx\n");
			 (load_addr "cx" (Quad_entry(e1)));
			 ("\tadd\tax, cx\n");
			 (store "ax" e2)]
	|Quad_calc(op,q1,q2,e) ->
		begin 
		match op with
	 	|"+" ->
			String.concat "" 
				[(load "ax" q1);
				 (load "dx" q2);
				 ("\tadd\tax, dx\n");
				 (store "ax" e)]
	 	|"-" ->
			String.concat ""
				[(load "ax" q1);
				 (load "dx" q2);
				 ("\tsub\tax, dx\n");
				 (store "ax" e)]
	 	|"*" ->
			String.concat ""
				[(load "ax" q1);
				 (load "cx" q2);
				 ("\timul\tcx\n");
				 (store "ax" e)]
	 	|"/" ->
			String.concat ""
				[(load "ax" q1);
				 ("\tpwd\n");
				 (load "cx" q2);
				 ("\tidiv\tcx\n");
				 (store "ax" e)]
	 	|"%" ->
			String.concat "" 
				[(load "ax" q1);
				 ("\tpwd\n");
				 (load "cx" q2);
				 ("\tidiv\tcx\n");
				 (store "dx" e)]
		|_ -> internal "Not an operator"; raise Terminate
		end
	|Quad_cond(op, q1, q2, n) ->
		begin 
		let jmp = 
			match op with
			|"==" -> "je"
			|"!=" -> "jne"
			|"<=" -> "jle"
			|"<" -> "jl"
			|">=" -> "jge"
			|">" -> "jg"
			|_ -> internal "Not a comparator"; raise Terminate
		in String.concat ""
			[(load "ax" q1);
			 (load "dx" q2);
			 ("\tcmp\tax,dx\n");
			 (Printf.sprintf "\t%s\t%s\n" jmp (label(!n)))]
		end
	|Quad_jump(z)->
		Printf.sprintf "\tjmp\t%s\n" (label(!z))
	|Quad_unit(f)->
		Stack.push f func_stack;
		let size = match f.entry_info with
			| ENTRY_function (info) -> (-info.function_negoffs)
			| _ -> internal "Function not a function"; raise Terminate
		in String.concat "" 
 			[(Printf.sprintf "%s\tproc\tnear\n" (get_name f));
			 ("\tpush\tbp\n");
			 ("\tmov\tbp,sp\n");
			 (Printf.sprintf "\tsub\tsp, %d\n" size)]
	|Quad_endu(f)->
		ignore (Stack.pop func_stack);
		String.concat ""
			[(Printf.sprintf "%s:\n\tmov\tsp, bp\n" (endof f));
			 ("\tpop\tbp\n");
			 ("\tret\n");
			 (Printf.sprintf "%s\tendp\n" (get_name f))]
	|Quad_call(f) ->
		let f_type = match f.entry_info with
		| ENTRY_function (info) -> info.function_result
		| _ -> internal "Calling something not a function"; raise Terminate
		in let dec_amt = if (f_type = TYPE_proc) then 2 else 0 in
		let size = find_parameter_size f in 
		String.concat "" 
			[(Printf.sprintf "\tsub\tsp, %d\n" dec_amt);
			 (update_AL (Stack.top func_stack) f);
			 (Printf.sprintf "\tcall\tnear ptr %s\n" (get_name f));
			 (Printf.sprintf "\tadd\tsp, %d\n" (4+size))]
	|Quad_ret ->
		Printf.sprintf "\tjmp\t%s\n" (endof (Stack.top func_stack))
	|Quad_dummy -> ""
	|Quad_par(q,pm)->
		match ((get_type q), pm) with
		|(TYPE_int, PASS_BY_VALUE) ->
			Printf.sprintf "%s\tpush\tax\n" (load "ax" q)
		|(TYPE_byte, PASS_BY_VALUE) ->
			String.concat "" 
				[(load "al" q);
				 ("\tsub\tsp, 1\n");
				 ("\tmov\tsi, sp\n");
				 ("\tmov\tbyte ptr [si], al\n")]
		|(_, PASS_BY_REFERENCE)
		|(_, PASS_RET) ->
			Printf.sprintf "%s\tpush\tsi\n" 
				(load_addr "si" q)	 
	

(* Main Final Code function - outputs im_code to out_chan *)
let output_final_code out_chan block_code =
	
	(* First passage to register all the functions with their respecting labels *)
	Array.iter (Array.iter register_quad) block_code;

	(* Get the label of the "main" function *)
	let len = Array.length block_code in
	let last_len = Array.length block_code.(len-1) in 
	let main_label = 
		match block_code.(len-1).(last_len-1) with
		| Quad_endu (f) -> (get_name f)
		| _ -> internal "Last quad is not a quad_endu"; raise Terminate 

	(* Output starting segment *)
	in Printf.fprintf out_chan "%s" (starting_code main_label);

	(* Iteration through all quads for output *)
	let output_single_quad quad =
		Printf.fprintf out_chan "%s" (convert_to_string quad)
	in let output_single_block block_no block =
		Printf.fprintf out_chan "@%d:\n" block_no;
		Array.iter output_single_quad block
	in Array.iteri output_single_block block_code;

	(* Iterate through all strings to output them *)
	let i = ref 0 in
	let output_single_string str =
		incr(i);
		Printf.fprintf out_chan "@str%d\tdb\t'%s'\n" !i str
	in Queue.iter output_single_string string_queue;
	Queue.clear string_queue;

	(* Iterate through all library functions used to declare them *)
	let output_single_external str = 
		Printf.fprintf out_chan "\textrn\t%s\t: proc\n" str in
	Sstring.iter output_single_external !lib_functions;

	(* Output end code *)
	Printf.fprintf out_chan "%s" end_code
