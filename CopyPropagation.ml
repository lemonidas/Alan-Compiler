open QuadTypes
open OptimizationSupport
open Quads
open Symbol
open Error

module CopyElem = struct
  type t = quad_elem_t * quad_elem_t * int
  let compare (q11,q12,b1) (q21,q22,b2) =
    let f x = Quads.get_id x in
    let id11 = f q11 in
    let id12 = f q12 in
    let id21 = f q21 in
    let id22 = f q22 in
    let res1 = compare id11 id21 in
    if res1 = 0 
    then let res2 = compare id12 id22 in
      if res2 = 0 
      then compare b1 b2
      else res2
    else res1
end

module CopySet = Set.Make(CopyElem)

let local_copy_propagation flowgraph_node cp_in block_id=
  let block = flowgraph_node.code_block in
  let n = Array.length block in

  let (local_cp_hash) =
    match cp_in with
    | None -> Hashtbl.create n
    | Some x -> x in

  let kill_set = ref QuadSet.empty in

  let copy_value q = 
    try 
      (Hashtbl.find local_cp_hash q, true)
    with
      Not_found -> (q,false) in

  let remove_value q = 
    (* Remove q->_ *)
    Hashtbl.remove local_cp_hash q;
    (* Then remove all _->q *)
    let remove_if_matched a b =
      if (get_id b) = (get_id q)
      then Hashtbl.remove local_cp_hash a
      else () in
    Hashtbl.iter remove_if_matched local_cp_hash in

  (* Hash only parameters or variables - NOT temporaries *)
  for i = 0 to n-1 do
    match block.(i) with
    | Quad_calc (op, q1, q2, q) ->
        let (new_q1, changed_1) = copy_value q1 in
        let (new_q2, changed_2) = copy_value q2 in
        if (changed_1 || changed_2) then (
          block.(i) <- Quad_calc(op, new_q1, new_q2, q)
        );
        remove_value q;
        if (is_not_temporary q) then kill_set := QuadSet.add q !kill_set
    | Quad_set (q, qr) ->
        if(is_not_temporary q && is_not_temporary qr)
        then Hashtbl.replace local_cp_hash qr q
        else remove_value qr;
        if (is_not_temporary qr) then kill_set := QuadSet.add qr !kill_set
    | Quad_array (q1, q2, e) ->
        let (new_q2, changed) = copy_value q2 in
        if (changed) then (
          block.(i) <- Quad_array(q1, new_q2, e)
        );
    | Quad_cond(op, q1, q2, n)->
        let (new_q1, changed_1) = copy_value q1 in
        let (new_q2, changed_2) = copy_value q2 in
        if (changed_1 || changed_2) then (
          block.(i) <- Quad_cond(op, new_q1, new_q2, n)
        )
    | Quad_par(q,pm)-> (
        let (new_q, changed) = copy_value q in
        match pm with
        | PASS_BY_VALUE ->
            if(changed) then block.(i) <- Quad_par(new_q, pm)
        | _ -> (* Value changes by reference.. *)
            remove_value q;
             if (is_not_temporary q) then kill_set := QuadSet.add q !kill_set
      )
    |_ -> () (* Nothing to be done here *)
  done;
  
  let copy_set = ref CopySet.empty in
  Hashtbl.iter (fun a b -> copy_set := CopySet.add (a,b,block_id) !copy_set ) local_cp_hash;
  
  (* Return the Copy and Kill Sets*)
  (!copy_set, !kill_set)

(* Main Function, performs copy propagation to a single flowgraph *)
let copy_propagation flowgraph =
  (* Compute a reverse postorder traversal for the graph *)
  let postorder = compute_postorder_traversal flowgraph in

  let n = Array.length flowgraph in  
  let copy_sets = Array.make n CopySet.empty in
  let kill_sets = Array.make n QuadSet.empty in
  
  (* Populate arrays of sets, doing the initial local cp *)
  Array.iteri 
    (fun i block ->
      let (copy_set, kill_set) = local_copy_propagation block None i in
      copy_sets.(i) <- copy_set;
      kill_sets.(i) <- kill_set;
    ) flowgraph;

  let copy_union = Array.fold_left CopySet.union CopySet.empty copy_sets in

  let cpin = Array.make n copy_union in
  cpin.(0) <- CopySet.empty;
  
  (* Initialize the worklist *)
  let worklist = ref Sint.empty in
  for i = 1 to n-1 do
    worklist := Sint.add i !worklist 
  done;

  (* Add everything to a queue *)
  let queue = Queue.create () in
  List.iter (fun x -> Queue.push x queue) postorder;
  
  (* Simple check to verify everything is going as planned *)
  let entry = Queue.pop queue in
  if (entry != 0) then (
    internal "Reverse postorder not starting with 0 but with %d" entry;
    raise Terminate;
  );  
  
  let rec loop () =
    if Queue.is_empty queue then ()
    else let next = Queue.pop queue in
    if Sint.mem next !worklist 
    then begin  
      worklist := Sint.remove next !worklist;
      let total = copy_union in
      let rec walk_preds acc_effect = function
        | []     -> acc_effect
        | (h::t) -> 
          
            (* Create : CPin(h)-kill(h) *)
            let filter_fun (a,b,_) = 
               not (QuadSet.mem a kill_sets.(h) 
               ||  QuadSet.mem b kill_sets.(h)) in              
            let not_killed = CopySet.filter filter_fun cpin.(h) in
            (* New Effect = Copy(h) U (CPin(h)-Kill(h) *)
            let new_effect = CopySet.union not_killed copy_sets.(h) in
  
            walk_preds (CopySet.inter new_effect acc_effect) t in
  
      let total_effect = walk_preds total flowgraph.(next).parents in
      if ( not (CopySet.equal total_effect cpin.(next))) 
      then begin
        cpin.(next) <- total_effect;
        
        (* Add to the worklist all successors of next not allready there *)
        let rec add_successors = function
        | [] -> ()
        | (h::t) -> begin
            if (not (Sint.mem h !worklist))
            then begin
              worklist := Sint.add h !worklist;
              Queue.push h queue
            end;
            add_successors t 
          end in
        
        add_successors flowgraph.(next).children
      end;
      (* Now loop again *)
      loop ()
    end
    (* Next was not a member of the worklist so loop again *)
    else loop () in (* End Loop *)

  (* Call loop *)
  loop ();

  (* What remains should now be all correct CPin... *)
  (*
  Printf.printf "CPins...\n";
  Array.iter (fun set -> CopySet.iter (fun (a,b,c) -> Printf.printf "<%s,%s,%d> " 
  ( string_of_quad_elem_t a) (string_of_quad_elem_t b) c) set; 
  Printf.printf "\n") cpin;
  *)
    
  (* Now use the CPins to perform yet again local_copy_propagation, 
   * but now with pre-initiated local_cp_hash *)
  let create_initial_hash cpin =
    let len = CopySet.cardinal cpin in
    let temp_hash = Hashtbl.create len in
    CopySet.iter (fun (a,b,_) -> Hashtbl.add temp_hash a b) cpin;
    temp_hash in

  (* Use the above function to repeat local cp *)
  Array.iteri 
   (fun i block ->
     let temp_hash = create_initial_hash cpin.(i) in
     ignore (local_copy_propagation block (Some temp_hash) i);
   ) flowgraph
  
  

