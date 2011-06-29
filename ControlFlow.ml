open Quads
open Blocks

type flowgraph_node_t = {
  code_block : quad_t array;
  mutable parents : int list;
  mutable children : int list
}

type flowgraph_t = flowgraph_node_t array

(* Function to convert function_block_t to flowgraph_t
 * IN  : function_block_t
 * OUT : flowgraph_t
 *)
let flowgraph_t_of_function_block_t fun_block =
  let total_blocks = Array.length fun_block in
  let init_fun i = 
    {
      code_block = fun_block.(i);
      parents = [];
      children = []
    } in
  let flowgraph = Array.init total_blocks init_fun in
  let insert_edge i j =
    flowgraph.(i).children <- j::flowgraph.(i).children;
    flowgraph.(j).parents <- i::flowgraph.(j).parents in
  let parse_block i code =
    let code_length = Array.length code in
    let rec walk_rev n = 
      if (n>=0)
      then match code.(n) with
      | Quad_jump x
      | Quad_cond (_,_,_,x) ->
          insert_edge i (!x);
          walk_rev (n-1)
      | Quad_ret ->
          insert_edge i (total_blocks-1);
          walk_rev (n-1)
      | _ -> () in
    match code.(code_length - 1) with
    | Quad_jump _
    | Quad_cond _
    | Quad_ret -> walk_rev (code_length - 1)
    | _ -> insert_edge i (i+1) in
  Array.iteri parse_block fun_block;
  flowgraph

(* Function to compute dominators, 
 * - Set of nodes represented by integers 0..N-1 
 *   (Function Block Size) 
 * - Root node is allways 0 (the block containing unit)
 * - returns Array (Node -> Node) immediate dominators
 * IN  : function_block_t (quad_t array array)
 * OUT : int array 
 *)
  
