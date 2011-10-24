open QuadTypes
open Error
open Symbol
open Types
open Quads
open UDChains
open OptimizationSupport

let simplified_jump = ref false

let single_constant_propagation flowgraph (uses_hash,defs_hash) temp_hash =

  (* Function to handle a use and return the value it (may) have *)
  let handle_use i j q =
    match q with 
    | Quad_entry _ -> (
      if is_not_temporary q then (
        let binding = 
          try Hashtbl.find uses_hash (q,i,j)
          with Not_found -> 
            internal "No binding for use %s" (string_of_quad_elem_t q); 
            raise Terminate in
        match binding.links with
        | []   -> None
        | [h]  -> h.value
        | h::t -> 
            if (List.for_all (fun x -> x.value = h.value) t)
            then h.value
            else None
      )  
      else (
        let binding = 
          try Hashtbl.find temp_hash q 
          with Not_found -> 
            internal "No binding for use %s" (string_of_quad_elem_t q); 
            raise Terminate in
          binding.temp_value
      ) 
    ) 
    | Quad_int x -> Some (int_of_string x)
    | _ -> None
    in (* End handle use *)

  let set_def i j q value = 
    if is_entry q then (
      if is_not_temporary q then (
        let binding = 
          try Hashtbl.find defs_hash (q,i,j)
          with Not_found -> 
            internal "No binding for use %s" (string_of_quad_elem_t q); 
            raise Terminate in
        binding.value <- value
      )
      else (
        let binding = 
          try Hashtbl.find temp_hash q 
          with Not_found -> 
            internal "No binding for use %s" (string_of_quad_elem_t q); 
            raise Terminate in
        binding.temp_value <- value
      )
    ) in (* End set def *) 

  (*
  let has_single_use q =
    if is_not_temporary q then (* If it is not a temporary find the links *)
      let binding = 
        try Hashtbl.find defs_hash q 
        with Not_found -> 
          internal "No binding for use %s" (string_of_quad_elem_t q); 
          raise Terminate in
      List.length (binding.links) = 1
    else true (* If it is temporary it has one use *) in
  *)
 
  let handle_single_node i node = 
    let handle_single_quad j = function
      | Quad_calc (op, q1, q2, q) -> (
          let calc = 
            match op with
            |"+" -> ( + )
            |"-" -> ( - )
            |"*" -> ( * )
            |"/" -> ( / )
            |"%" -> (mod)
            | _  -> internal "Not an operator"; raise Terminate in
          match (handle_use i j q1, handle_use i j q2) with
          | (None, None) -> ()
          | (Some v, None) -> 
              node.code_block.(j) <- Quad_calc (op, Quad_int (string_of_int v), q2, q)
          | (None, Some v) ->
              node.code_block.(j) <- Quad_calc (op, q1, Quad_int (string_of_int v), q)
          | (Some v1, Some v2) ->
              let q_value = calc v1 v2 in
              set_def i j q (Some (q_value));
              if (is_temporary q) then
                node.code_block.(j) <- Quad_dummy
              else 
                node.code_block.(j) <- Quad_set (Quad_int (string_of_int q_value), q)
      )
      | Quad_cond (op, q1, q2, e) -> (
          match (handle_use i j q1, handle_use i j q2) with
          | (Some v1, Some v2) -> (
            let cond = 
              match op with
              | "==" -> ( = )
              | "<"  -> ( < )
              | ">"  -> ( > )
              | "!=" -> ( != )
              | "<=" -> ( <= )
              | ">=" -> ( >= )
              | _ -> internal "Not a cond operand"; raise Terminate in
              if (cond v1 v2) then 
                node.code_block.(j) <- Quad_jump e
              else
                node.code_block.(j) <- Quad_dummy;
              simplified_jump := true;
          )
          | (Some v, None) ->
              node.code_block.(j) <- Quad_cond(op, Quad_int (string_of_int v), q2, e)
          | (None, Some v) ->
              node.code_block.(j) <- Quad_cond(op, q1, Quad_int (string_of_int v), e)
          | _ -> ()
      )
      | Quad_set (q1, q2) -> (
          match handle_use i j q1 with
          | Some v as q_value -> 
              set_def i j q2 q_value;
              if is_temporary q2 then
                node.code_block.(j) <- Quad_dummy
              else
                node.code_block.(j) <- Quad_set (Quad_int (string_of_int v), q2)
          | None -> ()
      )
      | Quad_array (a, q, e) -> (
          match handle_use i j q with
          | Some v ->
              node.code_block.(j) <- Quad_array (a, Quad_int (string_of_int v), e)
          | None -> ()
      )
      | Quad_par (q, pm) when pm = PASS_BY_VALUE -> (
          match handle_use i j q with
          | Some v -> 
              node.code_block.(j) <- Quad_par (Quad_int (string_of_int v), pm)
          | None -> ()
      )
      | _ -> () in (* End handle single quad *)

    Array.iteri handle_single_quad node.code_block in (* End handle_single_block *)

  Array.iteri handle_single_node flowgraph 

  (* End single_constant_propagation *)

let constant_propagation flowgraphs chains temps =
  simplified_jump := false;
  let n = Array.length flowgraphs in
  for i = 0 to n-1 do 
    single_constant_propagation flowgraphs.(i) chains.(i) temps.(i)
  done;
  !simplified_jump

  











