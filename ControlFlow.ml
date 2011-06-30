open Quads
open Blocks
open Error

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

(* Set module of int - used later *)
module Oint = struct 
  type t = int
  let compare = compare 
end

module Sint = Set.Make(Oint)

(* Function to compute (immediate)  dominators, 
 * - Set of nodes represented by integers 0..N-1 
 *   (Function Block Size) 
 * - Root node is allways 0 (the block containing unit)
 * - returns Array (Node -> Node) immediate dominators
 * IN  : flowgraph_t
 * OUT : int array 
 *
 * Further reference : Muchnik p.216
 *)
let compute_immediate_dominators flowgraph =
  let n = Array.length flowgraph in
  let bucket = Array.make n Sint.empty in
  let sdno = Array.make n 0 in
  let idom = Array.make n 0 in
  let parent = Array.make n 0 in
  let ndfs = Array.make n 0 in
  let ancestor = Array.make n 0 in
  let child = Array.make n 0 in
  let label = Array.make n 0 in
  let size = Array.make n 0 in

  (* DFS function with initializations *)
  let rec dfs i cnt =
    sdno.(i) <- cnt;
    label.(i) <- i;
    ndfs.(cnt) <- i;
    ancestor.(i) <- -1;
    child.(i) <- -1;
    size.(i) <- 1;
    let rec parse_children children cnt =
      match children with
      | [] -> cnt
      | (h::t) ->
          if (sdno.(h) = 0)
          then (
            parent.(h) <- i;
            let new_cnt = dfs h (cnt+1) in
            parse_children t new_cnt
          ) 
          else parse_children t cnt in
    parse_children flowgraph.(i).children (cnt+1) in

  (* Path compression Function *)
  let rec compress v = 
    let a = ancestor.(v) in
    if a < 0 then (
      internal "compressing with n0"; raise Terminate 
    )
    else if (ancestor.(a) != -1) 
    then (
      compress a;
      if (sdno.(label.(a)) < sdno.(label.(v)))
      then label.(v) <- label.(a);
      ancestor.(v) <- ancestor.(a);
    ) in (* End compress *)    

  (* Function to evaluate a node *)
  let eval v = 
    let a = ancestor.(v) in
    if a = -1 
    then label.(v)
    else (
      compress v;
      if (sdno.(label.(a)) >= sdno.(label.(v)))
      then label.(v)
      else label.(a)
    ) in (* End eval *)

  (* Links 2 nodes *)
  let link v w =
    let rec rebalance s =
      if (sdno.(label.(w)) >= sdno.(label.(child.(s))))
      then s
      else (
        if (size.(s) + size.(child.(child.(s))) >= 2 * size.(child.(s)))
        then (
          ancestor.(child.(s)) <- s;
          child.(s) <- child.(child.(s));
          rebalance s
        )
        else (
          size.(child.(s)) <- size.(s);
          ancestor.(s) <- child.(s);
          rebalance (ancestor.(s))
        )
      ) in
    let s = rebalance w in
    label.(s) <- label.(w);
    size.(v) <- size.(v) + size.(w);
    let new_s = 
      if (size.(v) < 2 * size.(w)) 
      then (
        let tmp = child.(v) in
        child.(v) <- s;
        tmp
      )
      else s in
    let rec go_to_n0 s =
      if s >= 0 
      then (
        ancestor.(s) <- v;
        go_to_n0 (child.(s))
      )
      else () in
    go_to_n0 new_s in (* End link *)

  (* Main Function now *)
  Printf.printf "N is %d\n" n
  let res = dfs 0 0 in
  Printf.printf "%d\n" res;
        
      
      
          
 
  
  

            
    
    
  



