open OptimizationSupport 
open QuadTypes
open Quads
open Error
open Symbol
open Debug

(* Returns a bool array array where every essential instruction is marked true *)
let mark_essential_blocks flowgraph =
  let mark_single_block block =
    let is_essential = function
      | Quad_par _
      | Quad_call _
      | Quad_tailCall _
      | Quad_ret _
      | Quad_array _
      | Quad_unit _
      | Quad_endu _
      | Quad_jump _ -> true
      | Quad_set (_,q) when get_id q = "$$" -> true
      | Quad_calc (_,_,_,q) when get_id q = "$$"
          -> true
      | _ -> false in
    let mark = Array.make (Array.length block) false in
    let handle_quad i quad = if (is_essential quad) then mark.(i) <- true in
    Array.iteri handle_quad block;
    mark (* Returns mark *) in
  let mark = Array.make (Array.length flowgraph) (Array.make 0 false) in
  let handle_block i flowgraph_node = mark.(i) <- mark_single_block flowgraph_node.code_block in
  Array.iteri handle_block flowgraph;
  mark (* Returns mark *)

let print_marks mark =
  Array.iteri (fun i node -> Array.iteri (fun j b -> Printf.printf "%d.%d: %b\n" i j b) node) mark

let single_dead_code_elimination flowgraph (uses_hash, defs_hash) = 
  let mark = mark_essential_blocks flowgraph in
  let worklist = ref PairSet.empty in

  (* Add all initially essential nodes to the worklist *)
  Array.iteri
    (fun i node -> Array.iteri 
      (fun j b -> if b then worklist := PairSet.add (i,j) !worklist) node) mark;
  
  let rec loop () =
    if PairSet.is_empty !worklist then ()
    else 
      (* Choose a pair from the worklist *)
      let (i,j) = PairSet.choose !worklist in
      worklist := PairSet.remove (i,j) !worklist;

      (* Extract the used and defined variables from the respective quad *)
      let (used, defined) =
        let quad = flowgraph.(i).code_block.(j) in
        match quad with
        | Quad_par (q,pm) -> if (pm = PASS_BY_VALUE) then ([q],[]) else ([], [q])
        | Quad_calc (_, q1, q2, q3) -> ([q1;q2], [q3])
        | Quad_set (q1, q2) -> ([q1], [q2])
        | Quad_cond (_, q1, q2, _) -> ([q1;q2], [])
        | Quad_array (_, q2, _) -> ([q2], [])
        | Quad_call (f,_) ->  (
          match f.entry_info with
          | ENTRY_function info ->
            Hashtbl.fold 
              (fun a m (l1,l2) -> 
                if m = GLOBAL_DEFINED then 
                  (l1, (Quad_entry a)::l2) 
                else ((Quad_entry a)::l1, l2))
            info.function_global ([],[])
          | _ -> internal "Not a function"; raise Terminate
        )
        | _ -> ([], []) in

      (* To handle a use, find its definitions and add them to the worklist *)
      let handle_quad_elem q =
        if (is_entry q) then
        let binding = 
          try Hashtbl.find uses_hash (q,i,j) 
          with Not_found -> (internal "No ud chain for %s"
          (string_of_quad_elem_t q); raise Terminate) in
        let handle_binding binding = 
          let b = binding.block_id and o = binding.offset in
          if not mark.(b).(o) then (
            mark.(b).(o) <- true;
            worklist := PairSet.add (b,o) !worklist
          ) in
        List.iter handle_binding binding.defs in
      List.iter handle_quad_elem used;

      (* To handle a def, find its uses and handle the conditions *)
      let handle_def_elem q =
        let binding = 
          try Hashtbl.find defs_hash (q,i,j)
          with Not_found -> (internal "No du chain"; raise Terminate) in
        let handle_binding binding =
          let b = binding.block_id and o = binding.offset in
          match flowgraph.(b).code_block.(o) with
          | Quad_cond _ -> 
              if (not mark.(b).(o)) then
                mark.(b).(o) <- true;
                worklist := PairSet.add (b,o) !worklist
          | _ -> () in
        List.iter handle_binding binding.uses in
      List.iter handle_def_elem defined;

      (* Finally loop again *)
      loop () in

  (* Call starting loop *)
  loop ();

  Array.iteri (fun i node -> Array.iteri (fun j b -> if (not b) then
    Printf.printf "Unused: %d %d\n" i j) node) mark;

