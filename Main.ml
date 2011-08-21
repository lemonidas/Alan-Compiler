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

    (* Constant Folding *)
		Optimizations.constant_folding block_code;

    (* Unreachable simple deletions *)
    CodeElimination.perform_deletions block_code;
  
    (* Simplify jumps *)
    (*Optimizations.jump_simplification block_code;*)

    (* Dummy elimination *)
		Optimizations.dummy_elimination block_code;
    
    (* Convert to flowgraph for further optimizations *)
    let flowgraphs = ControlFlow.flowgraph_array_of_quads block_code in
    
    (* Unreachable Code Elimination *)
    CodeElimination.delete_unreachable_blocks flowgraphs;
  
    (* Copy Propagation *)
    Array.iter CopyPropagation.copy_propagation flowgraphs;

    (* Tail Recursion *)
    TailRecursion.tail_recursion_elimination flowgraphs;

    (* Convert back *)
    let block_code = ControlFlow.convert_back_to_quads flowgraphs in
    
    block_code
		)
	else block_code
	
let main = 
	read_args () ;
	let in_channel = 
		match !source with
		|None -> stdin; 
		|Some(str) -> open_in str
  	in let lexbuf = Lexing.from_channel in_channel in
	let code_list = List.rev (Parser.program Lexer.lexer lexbuf) in
  let block_code = Blocks.blocks_of_quad_t_list code_list in
	let block_code = optimize block_code in
  (* For debug purposes *)
  let flowgraphs = ControlFlow.flowgraph_array_of_quads block_code in
  let block_code = ControlFlow.convert_back_to_quads flowgraphs in
  (* End debug *)
	match !mode with
	|Intermediate ->
		Blocks.output_block_code stdout block_code
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
			Blocks.output_block_code imm_channel block_code;
			output_final_code (open_out (base^".asm")) block_code (!optimizations);
			)
