open Quads
open ControlFlow
open Error
open OptimizationSupport

(* Nodes are numbers 0 -> n-1, flowgraph contains all 
 * necessary information, root is allways 0 
 *
 *
 *)
let compute_dominating_frontier flowgraph =
 
  let n = Array.length flowgraph in

  (* Compute immediate dominators *)
  let node_idom = compute_immediate_dominators flowgraph in
  let idom = Array.make n Sint.empty in
  let rec populate_idom i =
    if (i < n)
    then 
      let id = node_idom.(i) in
      idom.(id) <- Sint.add i idom.(id);
    populate_idom (i+1) in

  populate_idom 1;  

  let df = Array.make n Sint.empty in

  let handle_list_element i = 
    (* Function to traverse children *)
    let rec traverse_children children =
      match children with
      | [] -> ()
      | (h :: t) ->
          if (not (Sint.mem h idom.(i)))
          then df.(i) <- Sint.add h df.(i);
          traverse_children t 
    in

    (* Function to handle components in domset *)
    let handle_single_dominated z =
      let handle_frontier_element y =
        if (not (Sint.mem y idom.(i)))
        then df.(i) <- Sint.add y df.(i) 
      in Sint.iter handle_frontier_element df.(z) 
    in

    traverse_children flowgraph.(i).children;
    Sint.iter handle_single_dominated idom.(i) 
  in
    
  let visited = Array.make n false in
  let post_order = Array.make n 0 in
  let rec dfs i index =
    visited.(i) <- true;
    let rec walk_children children index = 
      match children with
      | [] -> index
      | (h :: t)->
          if (visited.(h)) 
          then 
            walk_children t index
          else 
            let nind = dfs h index in
            walk_children t (nind+1)
    in let new_index = walk_children flowgraph.(i).children index in
    post_order.(new_index) <- i;
    new_index
  in 

  (* Main function *)
  ignore (dfs 0 0);
  Array.iter handle_list_element post_order;

  (* Debug *)

  let print_set i set = 
    Printf.printf "Set %d:\n" i;
    Sint.iter (Printf.printf "%d ") set;
    Printf.printf "\n"; flush_all ();
  in
  Array.iteri print_set idom;
  Array.iteri print_set df;
  
  df
    
    
          
    
