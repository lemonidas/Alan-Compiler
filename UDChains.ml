open OptimizationSupport
open QuadTypes
open Quads
open Error
open Symbol
open Debug
open Types

(* Gen element, contains the variable/parameter + the definition instruction number *)
module DefElem = struct
  type t = quad_elem_t * int * int
  let compare (q1,b1,i1) (q2,b2,i2) =
    let res1 = compare (get_id q1) (get_id q2) in
    if res1 = 0 then
      let res = compare b1 b2 in
      if res = 0 then compare i1 i2 else res
    else res1
end

module DefSet = Set.Make(DefElem)

module PrsvElem = struct
  type t = quad_elem_t
  let compare q1 q2 = compare (get_id q1) (get_id q2)
end

module PrsvSet = Set.Make(PrsvElem)

(* Printing function for debug purposes *)
let print_def_set set =
  let print_elem (q,b,i) = Printf.printf "<%s,%d,%d> " (string_of_quad_elem_t q) b i in 
  DefSet.iter print_elem set;
  Printf.printf "\n"  

let print_prsv_set set =
  let print_elem q = Printf.printf "<%s> " (string_of_quad_elem_t q) in
  PrsvSet.iter print_elem set;
  Printf.printf "\n"

let find_all_uses flowgraph =
  let all_uses = ref PrsvSet.empty in
  let cond_add q_list = 
    List.iter (fun q -> if is_not_temporary q then all_uses := PrsvSet.add q !all_uses) q_list in
  let handle_quad = function
    | Quad_calc (_, q1, q2, q3) -> cond_add [q1;q2;q3]
    | Quad_set (q1,q2) -> cond_add [q1;q2]
    | Quad_array (q1,q2,_) -> cond_add [q1;q2]
    | Quad_cond (_,q1,q2,_) -> cond_add [q1;q2]
    | Quad_par (q, _) -> cond_add [q]
    | Quad_call (f, _) -> (
      match f.entry_info with
      | ENTRY_function info -> 
        cond_add (Hashtbl.fold (fun a _ acc -> Quad_entry a :: acc) info.function_global [])
      | _ -> internal "Not a function"; raise Terminate
      )        
    | _ -> ()
  in
  let n = Array.length flowgraph in
  for i = 0 to n-1 do
    Array.iter handle_quad flowgraph.(i).code_block;
  done;
  (* Return all the used variables in a flowgraph *)
  !all_uses

(* Function to go through a block and find all "definitions" within *)
let find_local_information block_id block all_vars=
  (* Create the set to hold the definitions *)
  let defs = ref (DefSet.empty) in
  let prsv = ref all_vars in
  
  (* Helper function to handle all quads *)
  let handle_quad inst_no= function
    | Quad_set (_,q) 
    | Quad_calc (_,_,_,q) ->
      if is_not_temporary q 
      then begin  
        defs := DefSet.add (q,block_id,inst_no) !defs;
        prsv := PrsvSet.remove q !prsv
      end
    | Quad_unit f ->
      let globals_used = PrsvSet.filter (is_not_local_var f) all_vars in
      PrsvSet.iter (fun x -> defs := DefSet.add (x, 0, 0) !defs) globals_used;
      let param_list = 
        match f.entry_info with
        | ENTRY_function fun_info -> fun_info.function_paramlist
        | _ -> internal "Function not a function"; raise Terminate;
      in 
      List.iter (fun x ->  defs:= DefSet.add (Quad_entry x, 0,0) !defs; ) param_list;
    | Quad_par (q,pm) when pm != PASS_BY_VALUE ->
        if is_not_temporary q 
        then begin
          defs := DefSet.add (q, block_id, inst_no) !defs;
          prsv := PrsvSet.remove q !prsv
        end
    | Quad_tailCall f
    | Quad_call (f, _) -> (
        match f.entry_info with
        | ENTRY_function fun_info ->
            let handle_binding entry status = 
              if status = GLOBAL_DEFINED
              then begin
                let q = Quad_entry entry in
                defs := DefSet.add (q, block_id, inst_no) !defs;
                prsv := PrsvSet.remove q !prsv
              end in
            Hashtbl.iter handle_binding fun_info.function_global
        | _ -> internal "Not a function"; raise Terminate
      )
    | _ -> ()
  in
  
  (* Use the handler to find all definitions in a block *)
  Array.iteri handle_quad block;
  (* Return both Sets *)
  (!defs, !prsv)

(* I think this is O(n^2) *)
let filter_gen_result defs =
  let filter_fun (q,b,i) = 
    not (DefSet.exists (fun (q1,b1,i1) -> (get_id q = get_id q1) && i < i1) defs) in
  DefSet.filter filter_fun defs

let single_reaching_definitions flowgraph =
 
  (* Use the precedent functions to create the necessary sets *)
  let n = Array.length flowgraph in
  let gen = Array.make n DefSet.empty in
  let unfiltered_gen = Array.make n DefSet.empty in
  let prsv = Array.make n PrsvSet.empty in
  let all_uses = find_all_uses flowgraph in
  
  (* Debug *)
  if !debug_reaching_definitions then begin
    Printf.printf "ALL USES:\n";
    print_prsv_set all_uses;
  end;

  for i = 0 to n-1 do
    let (g,p) = find_local_information i flowgraph.(i).code_block all_uses in
    gen.(i) <- filter_gen_result g;
    unfiltered_gen.(i) <- g;
    prsv.(i) <- p;  
  done;

  (* Debug *)
  if !debug_reaching_definitions then begin
    Printf.printf "Unfiltered Gen Sets:\n";
    Array.iter print_def_set unfiltered_gen;
    Printf.printf "Gen sets:\n";
    Array.iter print_def_set gen;
    Printf.printf "Prsv sets:\n";
    Array.iter print_prsv_set prsv;
    Printf.printf "\n"
  end;

  (* Compute a postorder traversal for the graph *)
  let postorder = compute_postorder_traversal flowgraph in

  (* Initialize RCHin values *)
  let rchin = Array.make n DefSet.empty in

  (* Initialize the worklist - with "all" nodes of the flowgraph but 0 *)
  let worklist = ref Sint.empty in
  for i = 1 to n-1 do
    worklist := Sint.add i !worklist
  done;

  (* Add all nodes to a priority queue *)
  let queue = Queue.create () in
  List.iter (fun x -> Queue.push x queue) postorder;
  
  (* Pop the zero from the queue *)
  if (Queue.pop queue != 0) then raise Terminate;
  
  (* Debug *)
  if !debug_reaching_definitions then
    Printf.printf "Before Defining loop\n";
  
  let rec loop () =
    if Queue.is_empty queue then ()
    else let next = Queue.pop queue in
    if Sint.mem next !worklist 
    then begin
      worklist:= Sint.remove next !worklist;
      let rec walk_preds acc_effect = function
        | []     -> acc_effect
        | (h::t) ->
            if !debug_reaching_definitions then Printf.printf "Walking predecessor %d\n" h;

            (* Create RCHin(h) ^ Prsv(h) *)
            let filter_fun (q,b,i) = PrsvSet.exists (fun q1 -> get_id q1 = get_id q) prsv.(h) in
            let preserved = DefSet.filter filter_fun rchin.(h) in

            if !debug_reaching_definitions then begin
              Printf.printf "Preserved: "; print_def_set preserved;
            end;
    
            (* New Effect = Gen(h) U (RCHin(h) ^ prsv(h)) *)
            let new_effect = DefSet.union preserved gen.(h) in

            if !debug_reaching_definitions then begin
              Printf.printf "New effect: "; print_def_set new_effect;
            end;

            (* New Accumulated effect is the union of the allready accumulated and the new *)
            walk_preds (DefSet.union new_effect acc_effect) t
      in (* End Walk Preds *)
      let total_effect = walk_preds DefSet.empty flowgraph.(next).parents in
      
      (* If there has been a change between total_effect and the previous rchin value *)
      if (not (DefSet.equal total_effect rchin.(next)))
      then begin
        rchin.(next) <- total_effect;

        (* Add the succesors to the worklist *)
        let rec add_succesors = function
        | [] -> ()
        | (h::t) -> 
          begin
            if (not (Sint.mem h !worklist))
            then begin
              worklist := Sint.add h !worklist;
              Queue.push h queue
            end;
            add_succesors t
          end
        in (* End add successos *)
      
        add_succesors flowgraph.(next).children
      end; (* end if *)

      (* Now loop again *)
      loop ()  
    end 
    (* Next was not a member of the worklist - find next candidate *)
    else loop () 
  in (* End loop *)
  
  loop ();

  (* Debug *)
  if !debug_reaching_definitions then Array.iter print_def_set rchin;

  (* Reaching Definitions have been computed here *)

  (* First find a possible error, in case some control flows don't return a value *)
  let test_for_return_value () =

    if !debug_reaching_definitions then
      Printf.printf "Testing for return value\n";
   
    match flowgraph.(0).code_block.(0) with
    | Quad_unit f -> (
        match f.entry_info with
        | ENTRY_function fun_info ->
            if fun_info.function_result != TYPE_proc then begin

              if !debug_reaching_definitions then
                Printf.printf "Not a proc function\n";

              let exists_fun (q,b,i) = get_id q = "$$" && b = 0 && i = 0 in
              if DefSet.exists exists_fun rchin.(n-1)
              then 
                warning "Some control paths don't return a value in function %s"
                  (Identifier.id_name f.entry_id)
            end
        | _ -> internal "Not a function"; raise Terminate
    )
    | _ -> internal "First quad not a unit"; raise Terminate in
  test_for_return_value ();

  (* Use all the above to compute UD/DU chains as well *)

  (* Initialize the hashtable of the nodes 
   * It will hash triplets (entry * block * offset) and return the node of the
   * data flow graph *)
  
  let uses_hash = Hashtbl.create 42 in
  let defs_hash = Hashtbl.create 42 in

  (* Add all definitions to the hashtable *)
  let handle_single_unfiltered (q,b,i) =
    let binding = {
      entry = q;
      block_id = b;
      offset = i;
      links = []
    } in
    Hashtbl.add defs_hash (q,b,i) binding 
  in 
  Array.iter (DefSet.iter handle_single_unfiltered) unfiltered_gen;

  (* Function to add a single use *)
  let add_use current_defs (q,b,i) =
    let defs_set = DefSet.filter (fun (q1,_,_) -> equal_quad_elems (q,q1)) current_defs in
    let def_binding_list = 
      DefSet.fold (fun x acc -> (Hashtbl.find defs_hash x)::acc) defs_set [] in
    let use_binding = {
      entry = q;
      block_id = b;
      offset = i;
      links = def_binding_list;
    } in
    Hashtbl.add uses_hash (q,b,i) use_binding;
    let update_def def_binding = def_binding.links <- use_binding :: def_binding.links in
    List.iter update_def def_binding_list
  in (* End add_use function *)
  
  (* Creation of the data flow graph using the above information + functions *)
  let walk_flowgraph_block current_defs block_id block=
    let handle_use q i = 
      if is_not_temporary q then 
        add_use !current_defs (q,block_id, i) in
    let handle_def q i = 
      if is_not_temporary q then 
        let filter_fun (q1,_,_) = not (equal_quad_elems (q,q1)) in
        let filtered = DefSet.filter filter_fun !current_defs in
        current_defs := DefSet.add (q,block_id, i) filtered in
    let handle_single_instruction inst_no= function
      | Quad_calc (_, q1, q2, q3) -> 
          handle_use q1 inst_no; 
          handle_use q2 inst_no;
          handle_def q3 inst_no
      | Quad_set (q1,q2) -> 
          handle_use q1 inst_no;
          handle_def q2 inst_no
      | Quad_array (_,q2,_) -> 
          handle_use q2 inst_no
      | Quad_cond (_,q1,q2,_) -> 
          handle_use q1 inst_no;
          handle_use q2 inst_no
      | Quad_par (q, pm) -> 
          if pm = PASS_BY_VALUE 
          then
            handle_use q inst_no
          else ( 
            (* handle_use q inst_no; *)
            handle_def q inst_no
          )
      | Quad_tailCall f
      | Quad_call (f, _) -> (
          let handle_binding entry status =
            let q = Quad_entry entry in
            if status = GLOBAL_USED 
            then 
              handle_use q inst_no
            else (
            (* handle_use q inst_no; *)
              handle_def q inst_no
            ) in
          match f.entry_info with
          | ENTRY_function fun_info ->
              Hashtbl.iter handle_binding fun_info.function_global
          | _ -> internal "Not a function"; raise Terminate
        )
      | _ -> () in
    Array.iteri handle_single_instruction block.code_block in (* End walk_flowgraph_block *)

  for i = 0 to n-1 do
    walk_flowgraph_block (ref rchin.(i)) i flowgraph.(i)
  done;

  let print_qbi (q,b,i) = Printf.printf "%s %d %d\n" (string_of_quad_elem_t q) b i in
  let print_bi_from_binding bind = Printf.printf "\t%d %d\n" bind.block_id bind.offset in
  let print_binding def_elem binding = 
    Printf.printf "Element: "; print_qbi def_elem;
    Printf.printf "Bindings:\n";
    List.iter (print_bi_from_binding) binding.links in

  if !debug_reaching_definitions then (
    Printf.printf "Uses:\n";
    Hashtbl.iter print_binding uses_hash;
    Printf.printf "Defs:\n";
    Hashtbl.iter print_binding defs_hash
  );

  (uses_hash, defs_hash)

let reaching_definitions flowgraphs =
  let empty_hash = Hashtbl.create 0 in
  let len = Array.length flowgraphs in
  let chains = Array.make len (empty_hash, empty_hash) in
  for i = 0 to len - 1 do
    chains.(i) <- single_reaching_definitions flowgraphs.(i)
  done;
  chains
 
let single_uninitialized_values_check (uses_hash,_) =  

  (* Function to iter over uses hashtable *)
  let handle_use _ use_binding = 

    let b = use_binding.block_id in
    let o = use_binding.offset in
    let exists_fun def_binding = 
      def_binding.block_id < b || 
      (def_binding.block_id = b && def_binding.offset < o) in

    if (not (List.exists exists_fun use_binding.links)) then begin
      warning "Variable %s could be uninitialized" 
      (string_of_quad_elem_t use_binding.entry);
      if !debug_reaching_definitions then begin
        Printf.printf "Variable %s uninitialized at %d %d\n"
        (string_of_quad_elem_t use_binding.entry) b o 
      end
    end in

  Hashtbl.iter handle_use uses_hash

let check_uninitialized_values chains =
  Array.iter single_uninitialized_values_check chains



  




  
  


