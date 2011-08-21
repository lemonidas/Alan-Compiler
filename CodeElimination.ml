open OptimizationSupport
open QuadTypes
open Error

(* Unreachable code Elimination *)

(* First iterate through all blocks and delete everything after 
 * an unconditional jump or a ret - useful particularly before
 * converting to a flowgraph - issue a warning *)
let delete_after_jumps block =
  let n = Array.length block in
  let rec loop i =
    if i < n then 
    begin
      match block.(i) with
      | Quad_ret
      | Quad_jump _ ->
        loop_delete (i+1)
      | _ -> loop (i+1)
    end
  and loop_delete i =
    if i < n then
    begin
      block.(i) <- Quad_dummy;
      loop_delete (i+1)
    end in
  loop 0

(* Perform the above deletions in basic blocks *)
let perform_deletions quads =
  Array.iter (Array.iter delete_after_jumps) quads

(* DFS in a flowgraph to determine unreachable from entry *)
let find_unreachable flowgraph =
  let n = Array.length flowgraph in
  let visited = Array.make n false in
  let rec dfs i =
    visited.(i) <- true;
    List.iter (fun x -> if (not visited.(x)) then dfs x) flowgraph.(i).children in
  dfs 0;
  for i = 0 to n-1 do
    if (not visited.(i))
    then (
      flowgraph.(i) <- {flowgraph.(i) with code_block = Array.make 0 Quad_dummy};
    )
  done

(* Use the above to do it in all Flowgraphs *)
let delete_unreachable_blocks flowgraphs =
  Array.iter find_unreachable flowgraphs
