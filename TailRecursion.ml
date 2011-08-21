open QuadTypes
open Quads
open OptimizationSupport
open Symbol
open Error
open Identifier
open Types

(* Consider a tail-call in intermediate code found, this function does what is 
 * needed *)
(* To deal with the problem of modified pars in between sets i do the following:
 * Let all the par quads do their work (increase stack size and put the args 
 * there). Then before the call i copy them back to the original place and
 * redecrease the stack size *)
let convert_tail_recursive_call flowgraph node_no inst_no =
  
  (* Get the function entry *)
  let func = match flowgraph.(node_no).code_block.(inst_no) with
  |Quad_call ent -> ent
  | _ -> internal "Not a function call!!"; raise Terminate in

  (* Get the function info *)
  let fun_info = match func.entry_info with
    | ENTRY_function fun_info -> fun_info
    | _ -> internal "Func must be a function"; raise Terminate in

  (* Check if it has a parameter by reference in order not to perform opt *)
  let result_type = fun_info.function_result in
  let rec check_if_exists_by_reference = function
    | []  when result_type = TYPE_proc -> true
    | [_] when result_type = TYPE_int || result_type = TYPE_byte -> true
    | (h::t) -> (
      match h.entry_info with
      | ENTRY_parameter par_info ->
        if par_info.parameter_mode = PASS_BY_REFERENCE
        then false 
        else check_if_exists_by_reference t
      | _ -> internal "Not a parameter"; raise Terminate 
      )
    | _ -> internal "Error in return type.. Should be caught earlier"; 
      raise Terminate in
  
  (* Right before the call "insert" the new code *)
  
  (* Make code list *)
  let code_as_list = Array.to_list (flowgraph.(node_no).code_block) in  
  
  (* Find the location of the quad_call *)
  let rec take_n_first acc lst n =
    match n, lst with
    | 0, (Quad_call _:: tail)   -> (acc, tail)
    | 0, _ -> internal "nth should be func call"; raise Terminate
    | _, (h::t) -> take_n_first (h::acc) t (n-1) 
    | _ -> 
      internal "Block can't finish before taking part of it!";
      raise Terminate in
   
  
  (* Find the size of the parameters pushed *)
  let params = fun_info.function_paramlist in
  let negoffs = fun_info.function_negoffs in
  
  Printf.printf "%d\n" negoffs;
 
  let rec walk_params param_list offset total code_acc =
    match param_list with
    | [] -> (total, code_acc)
    | (param::tail) ->
      match param.entry_info with 
      |ENTRY_parameter par_info ->
        let size = if (par_info.parameter_type = TYPE_byte) then 1 else 2 in
        let elem = Quad_tailpar (offset-size, par_info.parameter_type) in
        let new_quad = Quad_set(elem, Quad_entry(param)) in
        walk_params tail (offset-size) (total+size) (new_quad::code_acc)
      | _ -> internal "Parameter not a parameter?"; raise Terminate in

  
  if (check_if_exists_by_reference params)
  then (
    let (total, new_code) = walk_params params negoffs 0 [] in
    let (head, tail) = take_n_first [] code_as_list inst_no in
    let extra_code = [Quad_stack total;Quad_jump (ref 1)] in
    
    let new_code_list = List.rev_append head (new_code@extra_code@tail) in
    flowgraph.(node_no) <- { flowgraph.(node_no) with
      code_block = Array.of_list new_code_list
    };
  )
  



(* Finds out given a flowgraph where tail-recursion optimizations can occur *)
let single_tail_recursion_elimination flowgraph =
  let fun_entry = match flowgraph.(0).code_block.(0) with
    | Quad_unit ent -> ent
    | _ -> internal "First quad must be unit"; raise Terminate in
  let n = Array.length flowgraph in
  let predecessors = flowgraph.(n-1).parents in
  
  let rec parse_predecessors parents =
    match parents with
    | [] -> ()
    | (h::t) -> 
      let code = flowgraph.(h).code_block in
      let len = Array.length code in
      
      let rec check i =
        let handle_ret_case () =
          try  begin
            match code.(i-1) with
            (* If the function is proc then the one 1 instruction before return 
             * will be the tail call *)
            | Quad_call ent ->
              if ((ent.entry_scope.sco_nesting = fun_entry.entry_scope.sco_nesting) &&
                  (id_name ent.entry_id = id_name fun_entry.entry_id))
              then convert_tail_recursive_call flowgraph h (i-1)
            (* If the function is not proc, then 1 instruction before will be 
             * set, 2 the tail call and 3 the par(x, RET) *)
            | Quad_set (q1, q2) -> (
              if get_id q2 = "$$" 
              then
                match code.(i-2), code.(i-3) with
                | Quad_call ent, Quad_par (q, PASS_RET) ->
                  if ((ent.entry_scope.sco_nesting = 
                       fun_entry.entry_scope.sco_nesting) &&
                      (id_name ent.entry_id = id_name fun_entry.entry_id) &&
                      ((get_id q1) = (get_id q)))
                  then convert_tail_recursive_call flowgraph h (i-2)
                | _ -> ()
              )
            | Quad_ret -> check (i-1)
            | Quad_jump _ -> check (i-1)
            | _ -> ()
          end
          (* Array out of bounds exception *)
          with _ -> () in
                   
        match code.(i) with
        (* If last thing was a function call check if it is tail recursive *)
        | Quad_call ent ->
          (* Consider two functions equal if they have the same nesting and id *)
          if ((ent.entry_scope.sco_nesting = fun_entry.entry_scope.sco_nesting) &&
              (id_name ent.entry_id = id_name fun_entry.entry_id))
          then convert_tail_recursive_call flowgraph h (i)
        (* Else if it was a return check the one before *)
        | Quad_ret -> handle_ret_case ()
        | Quad_jump _ -> check (i-1)
        (* Else there is no tail recursion *)
        | _ -> () in
    
      check (len-1);
     
      (* Continue with the rest of the predecessors *)
      parse_predecessors t in (* End Parse Predecessors *)

  parse_predecessors predecessors

(* Use the above function to find all tail recursions - to be exported *)
let tail_recursion_elimination flowgraphs =
  Array.iter single_tail_recursion_elimination flowgraphs
