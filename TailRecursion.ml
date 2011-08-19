open QuadTypes
open Quads
open OptimizationSupport
open Symbol
open Error
open Identifier

(* Consider a tail-call in intermediate code found, this function does what is 
 * needed *)
let convert_tail_recursive_call flowgraph node_no inst_no =
  
  let func = match flowgraph.(node_no).code_block.(inst_no) with
  |Quad_call ent -> ent
  | _ -> internal "Not a function call!!"; raise Terminate in

  let params = match func.entry_info with
    | ENTRY_function fun_info ->
        fun_info.function_paramlist
    | _ -> internal "Func must be a function"; raise Terminate in
  
  let n = List.length params in
  
  (* Figure out the beginning of the "par quads" - and check for errors *)
  let param_start = inst_no - n in
  if (param_start < 0) 
  then (
    internal "Call block consists of less than needed par quads";
    raise Terminate;
  );

  let rec walk_params param_list i =
    match param_list, flowgraph.(node_no).code_block.(i) with
    | [], _ -> ()
    | (h::t), Quad_par(q,_) ->
      flowgraph.(node_no).code_block.(i) <- Quad_set (q,(Quad_entry h));
      walk_params t (i+1) in
  
  walk_params params param_start;
  flowgraph.(node_no).code_block.(inst_no) <- Quad_jump (ref 1)

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
      
      match code.(len-1) with
      (* If last thing was a function call check if it is tail recursive *)
      | Quad_call ent ->
        (* Consider two functions equal if they have the same nesting and id *)
        if ((ent.entry_scope.sco_nesting = fun_entry.entry_scope.sco_nesting) &&
            (id_name ent.entry_id = id_name fun_entry.entry_id))
        then convert_tail_recursive_call flowgraph h (len-1)
      (* Else if it was a return check the one before *)
      | Quad_ret -> (
        if (len-2 >= 0)
        then 
          match code.(len-2) with
          (* If the function is proc then the one 1 instruction before return 
           * will be the tail call *)
          | Quad_call ent ->
            if ((ent.entry_scope.sco_nesting = fun_entry.entry_scope.sco_nesting) &&
                (id_name ent.entry_id = id_name fun_entry.entry_id))
            then convert_tail_recursive_call flowgraph h (len-2)
          (* If the function is not proc, then 1 instruction before will be 
           * set, 2 the tail call and 3 the par(x, RET) *)
          | Quad_set (q1, q2) -> (
            if get_id q2 = "$$" 
            then try 
              match code.(len-3), code.(len-4) with
              | Quad_call ent, Quad_par (q, PASS_RET) ->
                if ((ent.entry_scope.sco_nesting = 
                     fun_entry.entry_scope.sco_nesting) &&
                    (id_name ent.entry_id = id_name fun_entry.entry_id) &&
                    ((get_id q1) = (get_id q)))
                then convert_tail_recursive_call flowgraph h (len-3)
              | _ -> ()
            with _ -> ()
          )
          | _ -> ()
        )
      (* Else there is no tail recursion *)
      | _ -> ();
     
      (* Continue with the rest of the predecessors *)
      parse_predecessors t in (* End Parse Predecessors *)

  parse_predecessors predecessors

(* Use the above function to find all tail recursions - to be exported *)
let tail_recursion_elimination flowgraphs =
  Array.iter single_tail_recursion_elimination flowgraphs
