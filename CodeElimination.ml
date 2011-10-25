open OptimizationSupport
open QuadTypes
open Error
open Symbol
open Identifier
open Debug
open Types

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

(* Eliminate extraneous temporary variables *)
let single_delete_temporary_variables fun_code = 
  let temps = ref EntrySet.empty in

  let handle_single_block block = 
    
    let handle_use q = 
      match q with
      | Quad_valof ent -> temps := EntrySet.add ent !temps
      | Quad_entry ent -> (
          match ent.entry_info with
          | ENTRY_temporary _ -> temps := EntrySet.add ent !temps
          | _ -> ()
      )
      | _ -> () in          

    let handle_single_quad = function
      | Quad_calc (_, q1, q2, q3) ->
          handle_use q1;
          handle_use q2;
          handle_use q3
      | Quad_cond (_, q1, q2, _) -> 
          handle_use q1; 
          handle_use q2
      | Quad_set (q1, q2) ->
          handle_use q1;
          handle_use q2
      | Quad_array (_, q, e) ->
          handle_use q;
          temps  := EntrySet.add e !temps
      | Quad_par (q, _) ->
          handle_use q
      | _ -> () in

    Array.iter handle_single_quad block in

  Array.iter handle_single_block fun_code;

  if !debug_temporary_deletion then begin
    Printf.printf "Temporaries used:\n";
    EntrySet.iter (fun e -> Printf.printf "<%s> " (id_name e.entry_id)) !temps;
    Printf.printf "\n";
  end;

  (* Extract the function entry *)
  let f = 
    match fun_code.(0).(0) with
    | Quad_unit f -> f
    | _ -> internal "First quad not a unit"; raise Terminate in 

  (* Extract the scope from the function *)
  let scope = 
    match f.entry_info with
    | ENTRY_function fun_info -> (
      match fun_info.function_scope with
      | Some scope -> scope
      | None -> internal "No scope registered"; raise Terminate
    )
   | _ -> internal "Function not a function"; raise Terminate in
  
  (* Find the last offset of the "local" information *)
  let (acc, lst) = find_first_temporary_offset scope in
  let temporary_offset = ref acc in
  let entry_list = ref lst in

  (* Debug *)
  if !debug_temporary_deletion then begin
    Printf.printf "Negofs are: %d\n" scope.sco_negofs;
    Printf.printf "Temporary offset is at %d\n" !temporary_offset;
  end;

  let handle_single_temp entry =
    let info = 
      match entry.entry_info with
      | ENTRY_temporary temp_info -> temp_info
      | _ -> internal "Not temporary"; raise Terminate in
    temporary_offset := !temporary_offset - (sizeOfType info.temporary_type);
    info.temporary_offset <- !temporary_offset;
    entry_list := entry :: !entry_list  in

  EntrySet.iter handle_single_temp !temps;
  scope.sco_negofs <- !temporary_offset;
  scope.sco_entries <- !entry_list;

  if !debug_temporary_deletion then begin
    Printf.printf "Finally negofs become: %d\n" !temporary_offset;
  end

let delete_temporary_variables block_code =
  Array.iter single_delete_temporary_variables block_code


