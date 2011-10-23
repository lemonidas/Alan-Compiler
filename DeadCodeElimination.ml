open OptimizationSupport 
open QuadTypes
open Quads
open Error
open Symbol
open Debug

(* Returns a bool array array where every essential instruction is marked true *)
let mark_essential_blocks flowgraph =

  (* Extract the function from the flowgraph *)
  let f =
    match flowgraph.(0).code_block.(0) with
    | Quad_unit f -> f
    | _ -> internal "Not a function"; raise Terminate in

  (* Function to mark initially essential blocks *)
  let mark_single_block block =
    let is_essential = function
      (* All Calls are important *)
      | Quad_par _
      | Quad_call _
      | Quad_tailCall _
      | Quad_ret _
      (* All Array address calculations are *)
      | Quad_array _
      | Quad_unit _
      | Quad_endu _
      | Quad_cond _ (* As in bubble sort, conditions must be essential *)
      | Quad_jump _ -> true
      (* An assignment is initially essential only if it modifies a 
       * value by reference or a global variable *)
      | Quad_set (_,q) 
      | Quad_calc (_,_,_,q) ->
        if (is_not_local_var) f q then true
        else is_parameter_by_reference q      
      | _ -> false in
    let mark = Array.make (Array.length block) false in
    let handle_quad i quad = if (is_essential quad) then mark.(i) <- true in
    Array.iteri handle_quad block;
    mark (* Returns mark *) in
  let mark = Array.make (Array.length flowgraph) (Array.make 0 false) in
  let handle_block i flowgraph_node = mark.(i) <- mark_single_block flowgraph_node.code_block in
  Array.iteri handle_block flowgraph;
  mark (* Returns mark *)

(* Printing function for debugging *)
let print_marks mark =
  Array.iteri (fun i node -> Array.iteri (fun j b -> Printf.printf "%d.%d: %b\n" i j b) node) mark

(* Dead Code Elimination for a single flowgraph *)
let single_dead_code_elimination flowgraph (uses_hash, defs_hash) temp_hash= 

  (* Initialize marks and worklist *)
  let mark = mark_essential_blocks flowgraph in
  let worklist = ref PairSet.empty in

  (* Debug *)
  if !debug_dead_code_elimination then begin
    Printf.printf "Initial Essential Marks\n";
    print_marks mark;
  end;

  (* Add all initially essential nodes to the worklist *)
  Array.iteri
    (fun i node -> Array.iteri 
      (fun j b -> if b then worklist := PairSet.add (i,j) !worklist) node) mark;
  
  (* Main loop (while worklist != empty) *)
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
        | Quad_par (q,pm) -> 
            if (pm = PASS_BY_VALUE) 
            then ([q],[]) 
            else ([], [q])
        | Quad_calc (_, q1, q2, q3) -> ([q1;q2], [q3])
        | Quad_set (q1, q2) -> ([q1], [q2])
        | Quad_cond (_, q1, q2, _) -> ([q1;q2], [])
        | Quad_array (_, q, e) -> ([q], [Quad_valof e])
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
        | Quad_unit f -> (
            match f.entry_info with
            | ENTRY_function fun_info ->
                let globals_used = Hashtbl.fold (fun a _ acc -> (Quad_entry a)::acc)
                  fun_info.function_global [] in
                let params = List.fold_left (fun acc a -> (Quad_entry a)::acc)
                  [] fun_info.function_paramlist in
                ([], params @ globals_used)
            | _ -> internal "Not a function"; raise Terminate
        )
        | _ -> ([], []) in

      (* Small Function To Add (conditionally) a pair (b,o) *)
      let cond_add b o =
        if not mark.(b).(o) then begin
          mark.(b).(o) <- true;
          worklist := PairSet.add (b,o) !worklist
        end in

      (* To handle a use, find its definitions and add them to the worklist *)
      let handle_use_elem q =

        (* Debug message *)
        if !debug_dead_code_elimination then
          Printf.printf "Entering Handle use elem with %s when parsing %d %d\n"
          (string_of_quad_elem_t q) i j;

        (* For uses, 2 cases - temporary and not *)

        (* If it is not a temporary variable : *)
        if (is_not_temporary q) then begin

          (* Extract the binding from the uses hash table *)
          let binding = 
            try Hashtbl.find uses_hash (q,i,j) 
            with Not_found -> (internal "No ud chain for %s"
            (string_of_quad_elem_t q); raise Terminate) in

          (* Function to iterate over all definitions of the binding:
           * Find its block + offset and conditionally add it *)
          let handle_definition def = cond_add (def.block_id) (def.offset) in

          (* Use the above to iterate over all definitions *)
          List.iter handle_definition binding.links

        end

        (* Else if it is a temporary *)
        else if (is_temporary q) then begin

          (* Extract the binding from the temp hash table *)
          let binding = 
            try Hashtbl.find temp_hash q 
            with Not_found -> (internal "No definition for %s"
              (string_of_quad_elem_t q); raise Terminate) in

          (* The binding now contains only one use, so add that *)
          cond_add (binding.def_block) (binding.def_offset)
        end in

      (* Handle_use for all used variables *)
      List.iter handle_use_elem used;


      (* To handle a def, find its uses and handle the conditions *)
      let handle_def_elem q =

        (* Debug message *)
        if !debug_dead_code_elimination then
          Printf.printf "Entering Handle def elem with %s when parsing %d %d \n"
          (string_of_quad_elem_t q) i j;

        (* If q is not Temporary *)
        if is_not_temporary q then begin

          (* Extract the binding from the definitions hash table *)
          let binding = 
            try Hashtbl.find defs_hash (q,i,j)
            with Not_found -> (internal "No du chain for %s"
            (string_of_quad_elem_t q); raise Terminate) in

          (* Function to handle a single binding *)
          let handle_binding single_binding = (            
            let b = single_binding.block_id and o = single_binding.offset in

            (* Match over the type of the "used" quads *)
            match flowgraph.(b).code_block.(o) with
            | Quad_cond _ -> (* If it is a condition, add it *)
                cond_add b o

            | Quad_calc (_,_,_,q) -> ( 
              (* If it is a calculation, check if it
               * leads to a second condition via a temporary *)
              let handle_second_order_cond b1 o1 = (
                match flowgraph.(b1).code_block.(o1) with 
                | Quad_cond _ -> 
                    cond_add b o;
                    cond_add b1 o1
                | _ -> () 
              ) in

              try (

                (* For temporaries, find the use via the temp hash *)
                if is_temporary q then  
                  let bind = Hashtbl.find temp_hash q in
                  handle_second_order_cond bind.use_block bind.use_offset

                (* For non-temporaries, search in the def_hash again, to find the uses *)
                else if is_not_temporary q then
                  let bind = Hashtbl.find defs_hash (q,b,o) in

                  (* Iterate over all links *)
                  let handle_link link = 
                    handle_second_order_cond link.block_id link.offset in
                  List.iter handle_link bind.links
              )

              with Not_found -> internal "No binding for %s"
                (string_of_quad_elem_t q); raise Terminate;
            )
            | _ -> () 

          ) in
          (* Iterate over all binding links *)
          List.iter handle_binding binding.links

        end (* End if not temporary *)

        (* Now if it is temporary *)
        else if is_temporary q then begin
          (* Find its use *)
          let binding = 
            try Hashtbl.find temp_hash q 
            with Not_found -> (internal "No use for %s"
              (string_of_quad_elem_t q); raise Terminate) in
          let b = binding.use_block and o = binding.use_offset in
          match flowgraph.(b).code_block.(o) with
          | Quad_cond _ -> 
              (* Add it if it is a condition *)
              cond_add b o
          | _ -> ()
        end in

      (* Now iterate over all definitions *)
      List.iter handle_def_elem defined;

      (* Finally loop again *)
      loop () in

  (* Call starting loop *)
  loop ();

  (* Next handle all unmarked nodes *)
  let handle_single_block i block =
    let handle_single_quad j b =
      if not b then begin
        warning "Non-essential instruction found: %s" 
          (string_of_quad_t flowgraph.(i).code_block.(j));
        if !debug_dead_code_elimination then 
          Printf.printf "Non-essential found at : %d %d\n" i j
        else
          flowgraph.(i).code_block.(j) <- Quad_dummy
      end in  
    Array.iteri handle_single_quad block in
  Array.iteri handle_single_block mark

let dead_code_elimination flowgraphs chains temp_chains=
  let len = Array.length flowgraphs in
  for i = 0 to len - 1 do
    single_dead_code_elimination flowgraphs.(i) chains.(i) temp_chains.(i)
  done
