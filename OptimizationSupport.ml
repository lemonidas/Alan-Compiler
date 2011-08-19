open QuadTypes

type flowgraph_node_t = {
  code_block : quad_t array;
  mutable parents : int list;
  mutable children : int list
}

type flowgraph_t = flowgraph_node_t array

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
  
