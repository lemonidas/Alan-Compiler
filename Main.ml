open Parser
open Error
open Lexing
open Quads
open Final

type mode_t = Normal | Intermediate | Final

let mode = ref Normal
let optimizations = ref false
let source = ref None

let usage () = 
	Printf.printf "Usage: ./alanc ([-f|-i] | <source_code>) [-O]\n";
	exit 1

let hash = Hashtbl.create 3

let keywords = 
	[("-f", fun () ->
		if (!mode = Normal) 
		then mode := Final
		else usage ());
	 ("-i", fun () ->
		if (!mode = Normal)
		then mode := Intermediate
		else usage ());
	 ("-O",  fun () ->
		if (Array.length (Sys.argv) = 2) 
		then usage ()
		else optimizations := true)]

let populate = 
	List.iter (fun (key, data) -> Hashtbl.add hash key data) keywords

let read_args () =
	let argc = Array.length (Sys.argv) in
	if ( argc = 0 || argc > 3) 
	then usage ()
	else
		for arg = 1 to argc-1 do
		try
			Hashtbl.find hash Sys.argv.(arg) ()
		with
			Not_found -> 
				if (!source = None) 
				then ( 
					let name = Sys.argv.(arg) in
					let len = String.length name in
					if (len < 6 || 
						(not (String.sub name (len-5) 5 = ".alan")))
					then
						Printf.printf "Source code must end in .alan\n"
					else 
						source := Some(name);			
				)
				else usage()
	done

let optimize block_code =
	if (!optimizations)
	then (
		(* First optimization is allways immediate backward *)
		Optimizations.immediate_backward_propagation block_code;

		(* Remaining optimizations - constant folding *)
		Optimizations.fold_constants block_code;
		Optimizations.dummy_elimination block_code;

		(* Start copy propagation - locally at first *)
		Array.iter (Optimizations.local_copy_propagation) block_code;

		(* Unreachable Code Elimination *)
		let flow_graph = Flow.convert_to_flow_graph block_code in
		Optimizations.unreachable_code_elimination flow_graph;

		Flow.blocks_of_flow_graph flow_graph
		)
	else block_code
	
let main = 
	read_args () ;
	let in_channel = 
		match !source with
		|None -> stdin; 
		|Some(str) -> open_in str
  	in let lexbuf = Lexing.from_channel in_channel in
	let code_list = Parser.program Lexer.lexer lexbuf in
	let len = List.length code_list in
	let code = Array.make len Quad_ret in
	let rec walk lst i=
		match lst with
		|[] -> ()
		|(quad::tail) -> 
			begin match quad with
			|Quad_jump(offset) ->
				code.(i) <- Quad_jump(ref(i + !offset))
			|Quad_cond(op, q1, q2, offset) ->
				code.(i) <- Quad_cond(op, q1, q2, ref(i + !offset))				
			|_ -> code.(i) <- quad
			end;
			walk tail (i+1)
	in walk (List.rev code_list) 0;
	let block_code = optimize (Flow.convert_to_blocks code) in
	match !mode with
	|Intermediate ->
		Flow.output_block_code stdout block_code
	|Final ->
		Printf.printf "Final mode not implemented yet\n"
	|Normal ->
		match (!source) with
		|None -> 
			internal "Normal mode reads from source code";
			raise Terminate
		|Some(str) ->
			let base = String.sub str 0 ((String.length str) - 5) in
			let imm_channel = open_out (base^".imm") in (
			Flow.output_block_code imm_channel block_code;
			output_final_code (open_out (base^".asm")) block_code;
			)
