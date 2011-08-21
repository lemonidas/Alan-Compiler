open QuadTypes
open Quads
open OptimizationSupport
open Symbol
open Error
open Identifier
open Types

(* Consider a tail-call in intermediate code found, this function does what is 
 * needed *)
let convert_tail_recursive_call flowgraph node_no inst_no =
  
  let func = match flowgraph.(node_no).code_block.(inst_no) with
  |Quad_call ent -> ent
  | _ -> internal "Not a function call!!"; raise Terminate in

  let (params, result_type) = match func.entry_info with
    | ENTRY_function fun_info ->
        (fun_info.function_paramlist, fun_info.function_result)
    | _ -> internal "Func must be a function"; raise Terminate in

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
  
  let n = List.length params in
  
  (* Find par quad locations - assumes no "by reference" *)
  let rec find_par_locations acc i pars_left =
    match pars_left with
    | 0 -> acc
    | _ -> 
      match flowgraph.(node_no).code_block.(i) with
      | Quad_par (q,_) -> find_par_locations ((i,q)::acc) (i-1) (pars_left - 1)
      | _ -> find_par_locations acc (i-1) pars_left in

  let param_locs = find_par_locations [] inst_no n in

  let rec walk_params param_list param_locs =
    match param_list, param_locs with
    | [], [] -> ()
    | (h::t), ((i,q)::ti) ->
      flowgraph.(node_no).code_block.(i) <- Quad_set (q,(Quad_entry h));
      walk_params t ti
    | _ -> internal "Uneven arguments"; raise Terminate in
  
  if (check_if_exists_by_reference params)
  then (
    walk_params params param_locs;
    flowgraph.(node_no).code_block.(inst_no) <- Quad_jump (ref 1)
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
