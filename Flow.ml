open Quads

(* Types and functions for creation and maintenance of flow_graph *)
type flow_graph_node_t = {
	code_block : quad_t array;
	mutable parents : int list;
	mutable children : int list
}	

type flow_graph_t = flow_graph_node_t array	

let convert_to_flow_graph block_code = 
	let length = Array.length block_code in
	let flow_graph = 
		Array.init (length+2) 
		(fun i -> {
			code_block = 
				if (i = 0 || i = length + 1) 
				then Array.make 0 Quad_dummy
				else block_code.(i-1);
			parents = [];
			children = []
		})
	in let insert_edge i j =
		flow_graph.(i).children <- j::flow_graph.(i).children;
		flow_graph.(j).parents <- i::flow_graph.(j).parents
	in let parse_block cur block = 
		let len = Array.length block in
		let rec walk i =
			if (i>=0) 
			then match block.(i) with
			|Quad_jump(x) 
			|Quad_cond (_,_,_,x) ->
				insert_edge cur (!x+1);
				walk (i-1)
			|_ -> walk (i-1)
		in walk (len-1)
	in let rec find_function_blocks units endus i = 
		if (i >= length ) 
		then (units,endus)
		else 
			let l = Array.length block_code.(i) in
			match (block_code.(i).(0), block_code.(i).(l-1)) with
			| (Quad_unit(_), Quad_endu(_)) -> 
				find_function_blocks ((i+1)::units) ((i+1)::endus) (i+1)
			| (_, Quad_endu(_)) ->
				find_function_blocks units ((i+1)::endus) (i+1)
			| (Quad_unit(_), _ ) ->
				find_function_blocks ((i+1)::units) endus (i+1)
			| _ -> find_function_blocks units endus (i+1)
	in let (units, endus) = find_function_blocks [] [] 0 in 
	List.iter (fun x -> insert_edge 0 x) units;
	List.iter (fun x -> insert_edge x (length+1)) endus;	
	Array.iteri (fun i x -> parse_block i x.code_block) flow_graph;
	flow_graph	

let blocks_of_flow_graph flow_graph =
	let f acc x = (x.code_block :: acc) in
	let list_code = Array.fold_left f [] flow_graph in
	Array.of_list (List.rev list_code)

(* Converts initial quad code to blocky code *)

let convert_to_blocks im_code =
	
	(* Variable Declarations *)
	let length = Array.length im_code in
	let breakpoint = Array.make length false in
	let blocks = Array.make length 0 in
	let block_no = ref 0 in

	(* Helper Functions *)
	let extract_jump_value = function
		| Quad_jump (x)
		| Quad_cond  (_,_,_,x)
			-> Some (!x)
		| _ -> None
 	in
	
	let is_jump = function
		| Quad_jump (_)
		| Quad_cond (_)
			-> true
		| _ -> false
 	in

	let convert_jump quad = 
		match quad with
		| Quad_jump (x)
		| Quad_cond (_, _, _, x) 
			-> x := blocks.(!x); quad
		| _ -> quad
 	in
	
	(* Important Functions Before Unit Calls *)
	
	(* Initialize break points *)
	let handle_quad_break x =
		match (extract_jump_value x) with
		| None -> ()
		| Some (n) -> breakpoint.(n) <- true
	in
	
	(* Main function to loop through array *)
	let rec walk i jump_mode =
		if (i >= length) 
		then ()
		else match (breakpoint.(i), jump_mode, is_jump im_code.(i)) with
		| (true,_,b) ->
			incr(block_no);
			blocks.(i) <- !block_no;
			walk (i+1) b
		| (false, true, true) ->
			blocks.(i) <- !block_no;
			walk (i+1) true
		| (false, true, false) ->
			incr(block_no);
			blocks.(i) <- !block_no;
			walk (i+1) false
		| (false, false, b) ->
			blocks.(i) <- !block_no;
			walk (i+1) b
	in
	
	(* Function to Return the next block and various info *)
	let rec next_block i index acc =
		if (i >= length) then 
			(List.rev acc, (i+1))
		else if (blocks.(i) > index) then
			(List.rev acc, (i))
		else 
			next_block (i+1) index ((convert_jump im_code.(i))::acc)
	in

	let rec loop i acc =
		if (i >= length)
		then 
			Array.of_list (List.rev acc)
		else
			let next_block_id = blocks.(i) in
			let (quads, new_i) = next_block i next_block_id [] in
			loop new_i ((Array.of_list quads)::acc)
	in

	(* Main Point *)
	Array.iter handle_quad_break im_code;
	breakpoint.(0) <- false; (* Zero is allready a break point *)
	walk 0 false;
	loop 0 [] 

(* Printing Function *)
let output_block_code out_chan block_code=
	let output_single quad = 
		Printf.fprintf out_chan "%s\n" (string_of_quad_t quad)
	in let output_block i block =
		Printf.fprintf out_chan "Block %d\n" i;
		Array.iter output_single block
	in Array.iteri output_block block_code


