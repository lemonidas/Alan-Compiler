open Quads
open Symbol
open Error
open Types
open Identifier

(* Dummy elimination - After some optimizations the code 
 * contains a few Quad_dummy. This filters them.
 * IN  : quad_t array array array
 * OUT : quad_t array array array
 *)
let dummy_elimination fun_code = 
  let dummy_helper block_code = 
    let length = Array.length block_code in
    for i = 0 to length - 1 do
      let code_list = Array.to_list block_code.(i) in
      let new_code = List.filter ((<>) Quad_dummy) code_list in
      let new_block = Array.of_list new_code in
      block_code.(i) <- new_block
    done
  in Array.iter dummy_helper fun_code

(*
(* FIXME : flow graph *)
let unreachable_code_elimination flow_graph =
	let length = array.length flow_graph in
	let reachable = array.make length false in
	let rec dfs v =
		if (reachable.(v))
		then ()
		else (
			reachable.(v) <- true;
			list.iter dfs (flow_graph.(v).children);
		)
	in dfs 0;
	array.iteri (fun i x -> 
		if (not reachable.(i))
		then flow_graph.(i) <- {
			x with code_block = array.make 1 quad_dummy
		}) flow_graph
*)

(* Reverse Copy Propagation 
 * IN  : quad_t array array array
 * OUT : quad_t array array array
 *)
let immediate_backward_propagation fun_code = 
  let propagate_single_fun block_code =
    let propagate_single_block block =
      let length = Array.length block in
      for i = 1 to length - 1 do
        match (block.(i-1),block.(i)) with
        |(Quad_calc(op,q1,q2,ec), Quad_set (Quad_entry(es), e))->
          if (es==ec)
          then (
            block.(i-1) <- Quad_calc(op,q1,q2, e);
            block.(i) <- Quad_dummy
          )
        |_ -> ()
      done
    in Array.iter propagate_single_block block_code
  in Array.iter propagate_single_fun fun_code

(* Constant folding *)
let constant_folding fun_code = 
  let fold_block block_code =
    let hash_initial_size = 42 in
    let constants_hash = Hashtbl.create hash_initial_size in
    let get_constant = function
      |Quad_none 
      |Quad_valof _
      |Quad_string _
      |Quad_char _
        -> None
      |Quad_int str -> Some (int_of_string str)
      |Quad_entry ent ->
        try
          Some (Hashtbl.find constants_hash ent)
        with
          Not_found -> None
    in let propagate_single_quad quad = 
      match quad with
      |Quad_calc(op, q1, q2, e) ->
        begin
        let calc = match op with
          |"+" -> (+)
          |"-" -> (-)
          |"*" -> ( * )
          |"/" -> ( / )
          |"%" -> (mod)
          |_ -> internal "Not an operator"; raise Terminate
        in let c1 = get_constant q1 in
        let c2 = get_constant q2 in
        match (c1,c2) with
        |(Some(v1), Some(v2))->
          Hashtbl.add constants_hash e (calc v1 v2);
          Some (Quad_dummy)
        |(Some v, None) ->
          Some (Quad_calc(op, (Quad_int(string_of_int v)), q2, e))
        |(None, Some v) -> 
          Some (Quad_calc(op, q1, (Quad_int(string_of_int v)), e))
        |(None, None) -> None
        end	
      |Quad_cond (op, q1, q2, e) ->
        begin
        let c1 = get_constant q1 in
        let c2 = get_constant q2 in
        match(c1,c2) with
        |(Some(v1), Some(v2))->
          Some (Quad_cond(op, Quad_int(string_of_int v1), Quad_int(string_of_int v2), e))
        |(Some v, None) ->
          Some (Quad_cond(op, (Quad_int(string_of_int v)), q2, e))
        |(None, Some v) -> 
          Some (Quad_cond(op, q1, (Quad_int(string_of_int v)), e))
        |(None, None) -> None
        end	
      |Quad_set(q,e) -> 
        begin
        match (get_constant q) with
        |Some v ->
          Some (Quad_set(Quad_int(string_of_int v),e))
        |None -> 
          None
        end
      |Quad_array(q1,q2, e)->
        begin 
        match get_constant q2 with
        | Some v -> 
          Some (Quad_array (q1, Quad_int(string_of_int v), e))
        | None -> None
        end
      |Quad_par(q,pm)-> 
        begin
        match get_constant q with
        |Some v -> Some (Quad_par (Quad_int(string_of_int v), pm))
        |None -> None			
        end
      |_ -> None
    in let length = Array.length block_code in
    for i = 0 to length - 1 do
      let l = Array.length block_code.(i) in
      for j = 0 to l - 1 do
        match (propagate_single_quad block_code.(i).(j)) with
        |Some q -> block_code.(i).(j) <- q
        |None -> ()
      done
    done
  in Array.iter fold_block fun_code

(* Copy Propagation Begins here *)

let local_copy_propagation block =
	let n = Array.length block in
	let local_cp_hash = Hashtbl.create n in
	let copy_value q = 
		try 
			(Hashtbl.find local_cp_hash q, true)
		with
			Not_found -> (q,false)
	in for i = 0 to n-1 do
		match block.(i) with
		| Quad_calc (op, q1, q2, e) ->
        let (new_q1, changed_1) = copy_value q1 in
        let (new_q2, changed_2) = copy_value q2 in
        if (changed_1 || changed_2) then (
          block.(i) <- Quad_calc(op, new_q1, new_q2, e)
        );
        Hashtbl.remove local_cp_hash (Quad_entry(e))
		| Quad_set (q,e) ->
        if(is_entry q)
        then Hashtbl.replace local_cp_hash (Quad_entry(e)) q
        else Hashtbl.remove local_cp_hash (Quad_entry(e))
		| Quad_array (q1, q2, e) ->
        let (new_q2, changed) = copy_value q2 in
        if (changed) then (
          block.(i) <- Quad_array(q1, new_q2, e)
        );
        Hashtbl.remove local_cp_hash (Quad_entry(e))
		| Quad_cond(op, q1, q2, n)->
        let (new_q1, changed_1) = copy_value q1 in
        let (new_q2, changed_2) = copy_value q2 in
        if (changed_1 || changed_2) then (
          block.(i) <- Quad_cond(op, new_q1, new_q2, n)
        )
    | Quad_par(q,pm)-> (
        let (new_q, changed) = copy_value q in
        match pm with
        | PASS_BY_VALUE ->
            if(changed) then block.(i) <- Quad_par(new_q, pm)
        | _ -> (* Value changes by reference.. *)
            Hashtbl.remove local_cp_hash q
      )
		|_ -> () (* Nothing to be done here *)
	done
			

			
	

