open QuadTypes
open Error
open Symbol

(* Type Definitions *)

(* Flowgraph *)
type flowgraph_node_t = {
  code_block : quad_t array;
  mutable parents : int list;
  mutable children : int list
}

type flowgraph_t = flowgraph_node_t array

(* Data flow graph *)
type data_flow_node_t = {
  entry        : quad_elem_t;           (* The entry it holds  *)
  location     : int * int;             (* Block * instruction *)
  is_def       : bool;                  (* Is Definition?      *)
  mutable defs : data_flow_node_t list; (* List of Definitions *)
  mutable uses : data_flow_node_t list; (* List of Uses        *)
}

(* A simple Set of Integers for use throughout *)
module Oint = struct 
  type t = int
  let compare = compare 
end

module Sint = Set.Make(Oint)

let compute_postorder_traversal flowgraph =
  let n = Array.length flowgraph in
  let visited = Array.make n false in

  (* Main DFS traversal *)
  let rec dfs i acc = 
    (* First Mark a Node as Visited *)
    visited.(i) <- true;

    (* Then parse the children of the node while adding nodes to the accumulator
     * Since we add values to the accumulator, we automatically create a reverse 
     * postorder traversal *)
    let rec walk_children children walk_acc = 
      match children with
      | [] -> walk_acc 
      | (h :: t)->
          if (visited.(h)) 
          then 
            (* If the node has been visited then walk the rest of the children *)
            walk_children t walk_acc
          else 
            (* Explore the node's children *)
            let new_acc = dfs h walk_acc in
            (* Add the node to the returned accumulator and continue parsing *)
            walk_children t new_acc in (* End Walk Children *)

    (* Use the above function to walk through all the children *)
    let new_acc = walk_children flowgraph.(i).children acc in
    
    (* Return the accumulator with the node in the beginning *)
    (i::new_acc) in (* End dfs *)

  dfs 0 [] (* End Compute Postorder Traversal *)


let compute_global_definitions quads =
  
  let compute_for_function fun_code =

    (* Extract the function entry to set the definitions *)
    let current_function = 
      match fun_code.(0).(0) with
      | Quad_unit f -> f
      | _ -> internal "First quad not a unit"; raise Terminate in

    (* Extract the function info *)
    let fun_info =
      match current_function.entry_info with
      | ENTRY_function fun_info -> fun_info
      | _ -> internal "Function not a function"; raise Terminate in

    let handle_entry ent is_def =
      (* Check if it is global - then its "nesting" level will be at least 1 less than the current
       * meaning it will be less or equal to the nesting of the function *)
      if (ent.entry_scope.sco_nesting <= current_function.entry_scope.sco_nesting)
      then try
        match Hashtbl.find fun_info.function_global ent with
        | GLOBAL_DEFINED -> ()
        | GLOBAL_USED -> 
          if (is_def) then Hashtbl.replace fun_info.function_global ent GLOBAL_DEFINED
      with
        Not_found ->
          let binding = if is_def then GLOBAL_DEFINED else GLOBAL_USED in
          Hashtbl.add fun_info.function_global ent binding in
  
    (* Function to handle a quad depending on "use" or "definition" *)
    let handle_quad_elem q is_def=
      match q with
      | Quad_entry ent -> handle_entry ent is_def
      | _ -> () in

    let handle_hash_entry e mode = 
      handle_entry e (if mode = GLOBAL_DEFINED then true else false) in

    (* Function to handle a single quad using the above handle func *)
    let handle_quad = function
      | Quad_calc (_, q1 ,q2, q3) ->
        handle_quad_elem q1 false;
        handle_quad_elem q2 false;
        handle_quad_elem q3 true
      | Quad_set (q1, q2) ->
        handle_quad_elem q1 false;
        handle_quad_elem q2 true
      | Quad_array (q1,q2,_ ) ->
        handle_quad_elem q1 true; (* If an array address is computed, consider the entire array used *)
        handle_quad_elem q2 false
      | Quad_cond (_, q1, q2, _) -> 
        handle_quad_elem q1 false;
        handle_quad_elem q2 false
      | Quad_par (q, PASS_BY_VALUE) ->
        handle_quad_elem q false
      | Quad_par (q, _) ->
        handle_quad_elem q true
      | Quad_call (f, _) -> (
        let called_info = 
          match f.entry_info with
          | ENTRY_function info -> info.function_global
          | _ -> internal "Not a function"; raise Terminate in
        Hashtbl.iter handle_hash_entry called_info;
        )
      | _ -> () in 

    (* Perform the actual computation *)
    Array.iter (Array.iter handle_quad) fun_code in
  
  (* Now use the compute_for_function to compute for all functions *) 
  (* Reverse order to account for all calls *)
  let n = Array.length quads in
  let rec walk i =
    if i >= 0 
    then (
      compute_for_function quads.(i);
      walk (i-1);
    ) in
  walk (n-1)



