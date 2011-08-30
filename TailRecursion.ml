open QuadTypes
open Quads
open OptimizationSupport
open Symbol
open Error
open Identifier
open Types
open FinalTypes

(*-------------------------------------- Front End Part---------------------------------------------- *)

(* Identify and mark all tail-recursive calls *)
let single_tail_recursion_elimination flowgraph =
  
  let n = Array.length flowgraph in

  (* Get the list of blocks that lead to the exit *)
  let predecessors = flowgraph.(n-1).parents in

  (* Small Function to handle each function call *)
  let handle_call ent actual_param_list i node_no = 

    (* Grab current function ID *)
    let cur_fun = 
      match flowgraph.(0).code_block.(0) with
      | Quad_unit ent -> ent
      | _ -> internal "First quad must be unit"; raise Terminate in

    (* Grab the function info *)
    let fun_info = 
      match ent.entry_info with
      | ENTRY_function fun_info -> fun_info
      | _ -> internal "Function not a function??"; raise Terminate in
    let param_list = fun_info.function_paramlist in

    (* If it is tail_rec then we check that all parameters called with by 
     * reference are not local *)
    let rec walk_params params actual_params = 
      match params, actual_params with
      | [],[] -> flowgraph.(node_no).code_block.(i) <- Quad_tailCall ent
      | hp::tp, ha::ta ->
        begin
          match hp.entry_info with
          | ENTRY_parameter par_info ->
            begin
              if par_info.parameter_mode = PASS_BY_REFERENCE 
              then (
                match ha with
                | Quad_entry aent ->
                  begin
                    match aent.entry_info with
                    | ENTRY_parameter _ -> walk_params tp ta 
                    | _ -> if (ta = [] && fun_info.function_result != TYPE_proc) then walk_params tp ta
                  end
                | _ -> ()
              )
              else walk_params tp ta
            end
          | _ -> internal "Parameter not a parameter"; raise Terminate
        end
      | _ -> internal "Uneven arguments"; raise Terminate in
    
    (* Start trying to handle the tail recursion only if entries match *)
    if (Symbol.equalEntries ent cur_fun)
    then walk_params param_list actual_param_list in (* End handle call *) 

  (* Assumes some unreachable code optimization is complete so that the last quad in each block
   * is either a call (tail - call) or a call followed by (optionally a set $$ +) ret *)
  let rec parse_predecessors parents = 
    match parents with
    | [] -> ()
    | (h::t) ->
      Printf.printf "Entering Predecessor %d\n" h;
      let code = flowgraph.(h).code_block in
      let len = Array.length code in
      try
        match code.(len-1) with
        | Quad_call (ent, param_list) -> handle_call ent param_list (len-1) h
        | Quad_ret -> (
          match code.(len-2) with
          | Quad_set (q,q2) ->
            begin
              if (get_id q2 = "$$")
              then 
                match code.(len-3) with
                | Quad_call (ent, param_list) -> handle_call ent param_list (len-3) h
                | _ -> ()
              else ();
            end
          |Quad_call (ent,param_list) -> handle_call ent param_list (len-2) h
          | _ -> ()
        )
        | _ -> ()
      with (* Array index out of bounds exception *)
        _ -> () in

  parse_predecessors predecessors

(* Use the above function to find all tail recursions - to be exported *)
let tail_recursion_elimination flowgraphs =
  Array.iter single_tail_recursion_elimination flowgraphs


(*--------------------------------------- Back End Part--------------------------------------------- *)

(* Function to be Called from Final.ml to handle the tail Recursive Call. All parameters have been 
 * pushed succesfully so the stack is at the "bottom" of the function plus the parameters size *)
let handle_final_code_tail_recursion ent label =

  (* Extract function info *)  
  let fun_info = 
    match ent.entry_info with
    | ENTRY_function fun_info -> fun_info
    | _ -> internal "Not a function"; raise Terminate in

  (* Find the size of a single parameter *)
  let find_size param =
    match param.entry_info with
    | ENTRY_parameter par_info -> 
      if (par_info.parameter_type = TYPE_byte && 
          par_info.parameter_mode = PASS_BY_VALUE) 
      then 1 
      else 2 
    | _ -> internal "Not a Parameter"; raise Terminate 
  in
    
  (* Find the size of the parameters *)
  let param_length =
     List.fold_left (fun acc param -> acc + (find_size param)) 0 fun_info.function_paramlist in
    
  (* Find the size of the stack above bp - not accounting for bp itself *)
  let bp_offset = (if fun_info.function_result = TYPE_proc then 2 else 0) + 6 in
  (* Find the location of the stack pointer - negoffs is negative!!! *)
  let sp_negoff = fun_info.function_negoffs - param_length in

  (* Debug *)
  (* Printf.printf "Param Length : %d\n" param_length;
  Printf.printf "Bp offset : %d\n" bp_offset;
  Printf.printf "Sp Negoff : %d\n" sp_negoff; *)

  let rec create_code acc param_list size_acc =
    match param_list with
    | [] ->
      (Jump(label))::(Add (Action_reg Sp, Constant param_length))::acc
    | [_] when fun_info.function_result != TYPE_proc -> 
      (Jump(label))::(Add (Action_reg Sp, Constant param_length))::acc
    | (h::t) ->
      let size = find_size h in
      let desc = if size = 1 then "byte" else "word" in
      let reg = if size = 1 then Al else Ax in
      let move_code = 
        [ Mov (Register reg, Mem_loc(desc, Bp, sp_negoff + size_acc - size));
          Mov (Mem_loc (desc, Bp, size_acc - size + bp_offset), Register reg)] in
      let new_acc = List.rev_append move_code acc in
      create_code new_acc t (size_acc-size)
  in
    
  let result = create_code [] fun_info.function_paramlist param_length in
  result
  
       
  
      
    

  
