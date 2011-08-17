open QuadTypes
open Quads
open Lexer
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
  let start = Printf.sprintf "\
	xseg\tsegment\tpublic 'code'\n\
	\tassume\tcs : xseg, ds : xseg, ss : xseg\n\
	\torg\t100h\n\
	main\tproc\tnear\n\
	\tcall\tnear ptr %s\n\
	\tmov\tax, 4C00h\n\
	\tint\t21h\n\
	main endp\n"
		main_label
  in (Start start)

(* End code *)
let end_code = End "xseg ends\n\tend  main\n"

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
    if (info.parameter_mode = PASS_BY_REFERENCE) then 2 
    else sizeOfType(info.parameter_type)
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
let label f n = 
  let name = get_name f in
  Printf.sprintf "@%d_%s" n name 

(* Update Links *)
let update_AL callee called =
	if (callee.entry_scope.sco_nesting < called.entry_scope.sco_nesting || 
      called.entry_scope.sco_nesting = max_int )
	then 
    [Push (Register Bp)]
	else if (callee.entry_scope.sco_nesting = called.entry_scope.sco_nesting)
	then 
    [Push (Register Ax); Mov (Register Ax, Mem_loc ("word", Bp, 4))]
	else 
		let n = callee.entry_scope.sco_nesting - called.entry_scope.sco_nesting in
		let rec walk i acc =
			if (i < n) 
			then
        walk (i+1) ((Mov (Register Si, Mem_loc ("word", Si, 4)))::acc)
			else ((Push (Mem_loc ("word",Si, 4)))::acc)
		in walk (n-1) [Mov (Register Si,Mem_loc ("word", Bp, 4))]

(* Bool function to find if entry is local *)
let local ent = 
	let c = Stack.top func_stack in	
	ent.entry_scope.sco_nesting = c.entry_scope.sco_nesting + 1

(* Function to convert type to "word"/"byte" *)
let rec size_description = function
	|TYPE_byte -> "byte"
	|TYPE_int -> "word"
	|TYPE_array(tp, sz) -> size_description tp
  |TYPE_pointer (inner) -> "word"
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
	let base = [Mov (Register Si,Mem_loc ("word", Bp, 4))] in
	let c =  (Stack.top func_stack) in 
	let na = ent.entry_scope.sco_nesting in
	let nc = c.entry_scope.sco_nesting + 1 in
	let rec loop acc = function
		| 0 -> acc
		| n -> 
      loop ((Mov (Register Si,Mem_loc ("word", Si, 4)))::acc) (n-1)
	in loop base (nc - na - 1)

(* Load helper function *)
let rec load reg q = 
	match q with
	|Quad_none -> []
	|Quad_int(str) -> [Mov (Register reg, Num str)]
	|Quad_char(str) -> 
    let character_code = string_of_int (Char.code str.[0]) in
    [Mov (Register reg, Num character_code)]
	|Quad_string(str)->	
		internal "Cannot Load a String in a register"; raise Terminate
	|Quad_entry(ent) -> (
		let (size, offset, mode) = get_info ent.entry_info in
		match ((local ent), mode) with
		|(true,true) ->
      List.rev (* Everything must be in reverse order! *)
      [ Mov (Register Si, Mem_loc ("word", Bp, offset));
        Mov (Register reg, Mem_loc (size, Si, 0)) ]
		|(true,false) ->
      [ Mov (Register reg, Mem_loc (size, Bp, offset)) ]
		|(false,true) ->
			let ar = get_ar ent in
      let tail = List.rev
      [ Mov (Register Si, Mem_loc ("word", Si, offset));
        Mov (Register reg, Mem_loc (size, Si, 0)) ]
      in tail @ ar
		|(false, false) ->
			let ar = get_ar ent in
      ( (Mov (Register reg, Mem_loc (size, Si, offset)))::ar )
		)
	|Quad_valof (ent) ->
    let size = size_description (extractType (get_type q))  in
    (Mov (Register reg, Mem_loc (size, Di, 0)))::
    (load Di (Quad_entry(ent)))

(* Load address helper function *)
let load_addr reg q =
	match q with
	|Quad_entry (ent) -> (
		let (size,offset,mode) = get_info ent.entry_info in
		match ((local ent), mode) with
		|(true,true) ->
      [Mov (Register reg, Mem_loc ("word", Bp, offset))]
		|(true,false) ->
      [Lea (Register (reg), Mem_loc (size, Bp, offset))]
		|(false, true) ->
      (Mov (Register (reg), Mem_loc (size, Si, offset)))::
			(get_ar ent)
		|(false, false) ->
      (Lea (Register reg, Mem_loc ("word", Si, offset)))::
  		(get_ar ent)
		)
	|Quad_valof(ent) -> 
		load reg (Quad_entry(ent))
	|Quad_string(str) -> 
		let addr = add_string str in
    [Lea (Register reg, String_addr addr)]
	|_ -> internal "Loading address of non entry/valof/string"; 
		raise Terminate

(* Store helper function *)
let store reg q =
  match q with
  | Quad_entry ent -> (
  	let (size, offset, mode) = get_info ent.entry_info in
	  	match ((local ent), mode) with
		  |(true,true) ->
        List.rev
        [ Mov (Register Si, Mem_loc ("word", Bp, offset));
          Mov (Mem_loc (size, Si, 0), Register reg) ]
  		|(true,false) ->
        [ Mov (Mem_loc (size, Bp, offset), Register (reg)) ]
		  |(false,true) ->
  			let ar = get_ar ent in
        let tail = List.rev 
        [ Mov (Register Si, Mem_loc ("word", Si, offset));
          Mov (Mem_loc (size, Si, 0), Register (reg)) ]
        in tail @ ar (* Reverse order! *)
		  |(false, false) ->
    		let ar = get_ar ent in
        ( Mov (Mem_loc (size, Si, offset), Register (reg)) )::ar
    )
  | Quad_valof ent ->
    let size = size_description (extractType (get_type q))  in
    (Mov (Mem_loc (size, Di, 0), Register reg))::
    (load Di (Quad_entry(ent)))
    
    

let rec flatten_rev acc = function
  | [] -> acc
  | (h::t) -> flatten_rev (h @ acc) t
      

(* Main function to convert a quad to string of final_type code *)
let final_t_of_quad = function
	|Quad_set(q,e) ->
    let size = get_size q in 
    flatten_rev [] 
      [load (get_register size Ax) q; 
       store (get_register size Ax) e ]
	|Quad_array(q1,q,e2) ->
		let e1 = match q1 with
		|Quad_entry x -> x
		|_ -> internal "Array not an lvalue..."; raise Terminate
		in let size = 
			match e1.entry_info with
			|ENTRY_variable(info)-> sizeOfArrayElem info.variable_type
			|ENTRY_parameter(info) ->sizeOfArrayElem info.parameter_type
			|_ -> internal "Called array with not an array"; raise Terminate
		in let reg_size = get_size q1 in
    flatten_rev [] 
    [ load (get_register reg_size Ax) q ;
      [ Mov (Register (get_register reg_size Cx), Num (string_of_int size)) ];
      [ IMul (get_register reg_size Cx) ];
      load_addr (get_register reg_size Cx) (Quad_entry e1);
      [Add (Action_reg (get_register reg_size Ax), 
            Action_reg (get_register reg_size Cx)) ];
      store (get_register reg_size Ax) (Quad_entry (e2))
    ]
	|Quad_calc(op,q1,q2,e) ->
		begin 
    let size = if (get_type q1 = TYPE_byte) then "byte" else "word" in
		match op with
	 	|"+" ->
      flatten_rev [] 
      [ load (get_register size Ax) q1;
        load (get_register size Cx) q2;
        [Add (Action_reg (get_register size Ax), 
              Action_reg (get_register size Cx))];
        store (get_register size Ax) e ]
	 	|"-" ->
      flatten_rev []
      [ load (get_register size Ax) q1;
        load (get_register size Cx) q2;
        [Sub (Action_reg (get_register size Ax), 
              Action_reg (get_register size Cx))];
        store (get_register size Ax) e ]
	 	|"*" ->
      flatten_rev []
      [ load (get_register size Ax) q1;
        load (get_register size Cx) q2;
        [IMul (get_register size Cx)];
        store (get_register size Ax) e ]
	 	|"/" ->
      flatten_rev []
      [ load (get_register size Ax) q1;
        [Cwd];
        load (get_register size Cx) q2;
        [IDiv (get_register size Cx)];
        store (get_register size Ax) e ]
	 	|"%" ->
      flatten_rev [] 
      [ load (get_register size Ax) q1;
        [Cwd];
        load (get_register size Cx) q2;
        [IDiv (get_register size Cx)];
        store Dx e ]
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
    in let size = get_size q1 
    in flatten_rev []
      [ load (get_register size Ax) q1;
        load (get_register size Cx) q2;
        [Cmp ((get_register size Ax),(get_register size Cx))];
        [Cond_jump (jmp, (label (Stack.top func_stack) (!n)))] ]
		end
	|Quad_jump(z)->
    [ Jump (label (Stack.top func_stack) (!z)) ]
	|Quad_unit(f)->
		Stack.push f func_stack;
		let size = match f.entry_info with
			| ENTRY_function (info) -> (-info.function_negoffs)
			| _ -> internal "Function not a function"; raise Terminate
    in flatten_rev []
    [ [Proc (get_name f)];
      [Push (Register Bp)];
      [Mov (Register Bp, Register Sp)];
      [Sub (Action_reg Sp, Constant size)] ]
	|Quad_endu(f)->
		ignore (Stack.pop func_stack);
    let endp = (Printf.sprintf "%s\tendp\n" (get_name f)) in
    flatten_rev [] 
    [ [Label (endof f)];
      [Mov (Register Sp, Register Bp)];
      [Pop Bp];
      [Ret];
      [Misc endp] ]
	|Quad_call(f) ->
		let f_type = match f.entry_info with
		| ENTRY_function (info) -> info.function_result
		| _ -> internal "Calling something not a function"; raise Terminate
		in let dec_amt = if (f_type = TYPE_proc) then 2 else 0 in
		let size = find_parameter_size f in 
    flatten_rev []
    [ [Sub (Action_reg Sp, Constant dec_amt)];
			update_AL (Stack.top func_stack) f;
      [Call (get_name f)];
      [Add (Action_reg Sp, Constant (4+size))] ]
	|Quad_ret ->
    [Jump (endof (Stack.top func_stack))]
	|Quad_dummy -> []
	|Quad_par(q,pm)->
		match ((get_type q), pm) with
		|(TYPE_int, PASS_BY_VALUE) ->
      flatten_rev []
      [ load Ax q;
        [Push (Register Ax)] ]
		|(TYPE_byte, PASS_BY_VALUE) ->
      flatten_rev [] 
      [ load Al q;
        [Sub (Action_reg Sp, Constant 1)];
        [Mov (Register Si, Register Sp)];
        [Mov (Mem_loc ("byte", Si, 0), Register Al)] ]
		|(_, PASS_BY_REFERENCE)
		|(_, PASS_RET) ->
      flatten_rev []
      [ load_addr Si q;
        [Push (Register Si)] ]

(* Main Final Code function - outputs im_code to out_chan and returns the low-level representation 
 * IN  : quad_t array array array, out_channel
 * OUT : final_t list
 *)
let output_final_code out_chan fun_code optimize =
	
  (* First passage to register all the functions with their respecting labels 
   * All Quad_unit are the first quads of each block... *)
  let register_function fun_block = 
    match fun_block.(0).(0) with
    | Quad_unit f -> set_name f
    | _ -> internal "First quad not a unit"; raise Terminate
  in 
  Array.iter register_function fun_code;
	
  (* Get the label of the "main" function 
   * The last block's first quad contains it *)
	let len = Array.length fun_code in
	let main_label = 
		match fun_code.(len-1).(0).(0) with
		| Quad_unit f -> get_name f
		| _ -> internal "Last funs first quad is not a quad_unit"; raise Terminate 
  in 
  let starting_segment = starting_code main_label in
  
  Printf.fprintf out_chan "%s" (string_of_final_t starting_segment);
	(* Iteration through all quads to create low-level final code *)
  let convert_single_block block_code =
    Array.fold_left 
      (fun acc quad -> (final_t_of_quad quad @ acc)) [] block_code
  in let convert_single_fun fun_block =
    let len = Array.length fun_block in
    let f_entry = 
      match fun_block.(0).(0) with
      | Quad_unit f -> f
      | _ -> internal "First quad not a unit"; raise Terminate
    in let rec walk_fun i acc =
      if (i >= len) 
      then acc
      else 
        let block_label = label f_entry i in
        let final_t_label = if i > 0 then [Label block_label] else [] in
        walk_fun (i+1) ((convert_single_block fun_block.(i)) @ (final_t_label @ acc))
    in walk_fun 0 []
  in let low_level_code = 
    List.rev (Array.fold_left 
      (fun acc block -> (convert_single_fun block) @ acc) [] fun_code) in

  (* Optimize low_level code *)
  let optimized = 
    if (optimize) 
    then FinalOptimizations.optimize low_level_code 
    else low_level_code in
    
  (* Output low_level code *)
  List.iter 
    (fun final -> 
      Printf.fprintf out_chan "%s" (string_of_final_t final)
    ) optimized;

	(* Iterate through all strings to output them *)
	let i = ref 0 in
	let output_single_string str =
		incr(i);
    let asm_string = AlanString.handle_escapes str in
		Printf.fprintf out_chan "@str%d%s" !i asm_string
	in Queue.iter output_single_string string_queue;
	Queue.clear string_queue;

	(* Iterate through all library functions used to declare them *)
	let output_single_external str = 
		Printf.fprintf out_chan "\textrn\t%s\t: proc\n" str in
	Sstring.iter output_single_external !lib_functions;

	(* Output end code *)
  Printf.fprintf out_chan "%s" (string_of_final_t end_code)
