open OptimizationSupport
open QuadTypes
open Quads
open Error
open Symbol

(* Gen element, contains the variable/parameter + the definition instruction number *)
module DefElem = struct
  type t = quad_elem_t * int * int
  let compare (q1,b1,i1) (q2,b2,i2) =
    let res = compare b1 b2 in
    if res = 0 then compare i1 i2 else res
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
      let param_list = 
        match f.entry_info with
        | ENTRY_function fun_info -> fun_info.function_paramlist
        | _ -> internal "Function not a function"; raise Terminate;
      in List.iter (fun x -> defs:= DefSet.add (Quad_entry x, 0,0) !defs) param_list
    (* FIXME - needs to include Par/Call *)
    | _ -> ()
  in
  
  (* Use the handler to find all definitions in a block *)
  Array.iteri handle_quad block;
  (* Return both Sets *)
  (!defs, !prsv)

(* I think this is O(n^2) *)
let filter_gen_result defs =
  let filter_fun (q,b,i) = not (DefSet.exists (fun (q1,b1,i1) -> (get_id q = get_id q1) && i < i1) defs) in
  DefSet.filter filter_fun defs

let reaching_definitions flowgraph =
 
  (* Use the precedent functions to create the necessary sets *)
  let n = Array.length flowgraph in
  let gen = Array.make n DefSet.empty in
  let prsv = Array.make n PrsvSet.empty in
  let all_uses = find_all_uses flowgraph in
  for i = 0 to n-1 do
    let (g,p) = find_local_information i flowgraph.(i).code_block all_uses in
    gen.(i) <- filter_gen_result g;
    prsv.(i) <- p;  
  done;

  Printf.printf "Gen sets:\n";
  Array.iter print_def_set gen;
  Printf.printf "Presv sets:\n";
  Array.iter print_prsv_set prsv;
  Printf.printf "\n";

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
            Printf.printf "Walking predecessor %d\n" h;
            (* Create RCHin(h) ^ Prsv(h) *)
            let filter_fun (q,b,i) = PrsvSet.exists (fun q1 -> get_id q1 = get_id q) prsv.(h) in
            let preserved = DefSet.filter filter_fun rchin.(h) in
            Printf.printf "Preserved: "; print_def_set preserved;
    
            (* New Effect = Gen(h) U (RCHin(h) ^ prsv(h)) *)
            let new_effect = DefSet.union preserved gen.(h) in
            Printf.printf "New effect: "; print_def_set new_effect;
    
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
  Array.iter print_def_set rchin 


