open QuadTypes
open Quads
open Error
open Symbol
open Identifier

(* Type Definitions *)

(* Flowgraph *)
type flowgraph_node_t = {
  code_block : quad_t array;
  mutable parents : int list;
  mutable children : int list
}

type flowgraph_t = flowgraph_node_t array

(* UD Chain Node *)
type chain_node_t = {
  entry         : quad_elem_t;       (* The entry it holds   *)
  block_id      : int;               (* Block no             *)
  offset        : int;               (* Instruction offset   *)
  mutable value : int option;        (* For Constant Prop    *)
  mutable links : chain_node_t list; (* List of Linked Nodes *)
}

(* Pair module and Set *)
module Pair = struct
  type t = int * int
  let compare (i11, i12) (i21,i22) =
    let res = compare i11 i21 in
    if res = 0 then compare i12 i22 else res
end

module PairSet = Set.Make(Pair)

(* A simple Set of Integers for use throughout *)
module Oint = struct 
  type t = int
  let compare = compare 
end

module Sint = Set.Make(Oint)

(* Set of quad_elem_t *)
module QuadElem = struct
  type t = quad_elem_t
  let compare q1 q2 = compare (get_id q1) (get_id q2)
end

module QuadSet = Set.Make(QuadElem)

(* Set of Symbol entries for Temporaries *)
module EntryElem = struct
  type t = Symbol.entry  
  let compare e1 e2 = 
    let id1 = id_name e1.entry_id in
    let id2 = id_name e2.entry_id in
    let t1 = String.sub id1 1 (String.length id1 - 1) in
    let t2 = String.sub id2 1 (String.length id2 - 1) in
    compare (int_of_string t1) (int_of_string t2)
end

module EntrySet = Set.Make(EntryElem)

let compute_postorder_traversal flowgraph =
  let n = Array.length flowgraph in
  let visited = Array.make n false in

  (* Main DFS traversal *)
  let rec dfs i acc = 
    (* First Mark a Node as Visited *)
    visited.(i) <- true;

    (* Then parse the children of the node while adding nodes to the accumulator
     * Since we add values to the accumulator, we automatically create a reverse 
     * postorder traversal *)
    let rec walk_children children walk_acc = 
      match children with
      | [] -> walk_acc 
      | (h :: t)->
          if (visited.(h)) 
          then 
            (* If the node has been visited then walk the rest of the children *)
            walk_children t walk_acc
          else 
            (* Explore the node's children *)
            let new_acc = dfs h walk_acc in
            (* Add the node to the returned accumulator and continue parsing *)
            walk_children t new_acc in (* End Walk Children *)

    (* Use the above function to walk through all the children *)
    let new_acc = walk_children flowgraph.(i).children acc in
    
    (* Return the accumulator with the node in the beginning *)
    (i::new_acc) in (* End dfs *)

  dfs 0 [] (* End Compute Postorder Traversal *)


let compute_global_definitions quads =
  
  let compute_for_function fun_code =

    (* Extract the function entry to set the definitions *)
    let current_function = 
      match fun_code.(0).(0) with
      | Quad_unit f -> f
      | _ -> internal "First quad not a unit"; raise Terminate in

    (* Extract the function info *)
    let fun_info =
      match current_function.entry_info with
      | ENTRY_function fun_info -> fun_info
      | _ -> internal "Function not a function"; raise Terminate in

    let handle_entry ent is_def =
      (* Check if it is global - then its "nesting" level will be at least 1 less than the current
       * meaning it will be less or equal to the nesting of the function *)
      if (ent.entry_scope.sco_nesting <= current_function.entry_scope.sco_nesting)
      then try
        match Hashtbl.find fun_info.function_global ent with
        | GLOBAL_DEFINED -> ()
        | GLOBAL_USED -> 
          if (is_def) then Hashtbl.replace fun_info.function_global ent GLOBAL_DEFINED
      with
        Not_found ->
          let binding = if is_def then GLOBAL_DEFINED else GLOBAL_USED in
          Hashtbl.add fun_info.function_global ent binding in
  
    (* Function to handle a quad depending on "use" or "definition" *)
    let handle_quad_elem q is_def=
      match q with
      | Quad_entry ent -> handle_entry ent is_def
      | _ -> () in

    let handle_hash_entry e mode = 
      handle_entry e (if mode = GLOBAL_DEFINED then true else false) in

    (* Function to handle a single quad using the above handle func *)
    let handle_quad = function
      | Quad_calc (_, q1 ,q2, q3) ->
        handle_quad_elem q1 false;
        handle_quad_elem q2 false;
        handle_quad_elem q3 true
      | Quad_set (q1, q2) ->
        handle_quad_elem q1 false;
        handle_quad_elem q2 true
      | Quad_array (q1,q2,_ ) ->
        handle_quad_elem q1 true; (* If an array address is computed, consider the entire array used *)
        handle_quad_elem q2 false
      | Quad_cond (_, q1, q2, _) -> 
        handle_quad_elem q1 false;
        handle_quad_elem q2 false
      | Quad_par (q, PASS_BY_VALUE) ->
        handle_quad_elem q false
      | Quad_par (q, _) ->
        handle_quad_elem q true
      | Quad_call (f, _) -> (
        let called_info = 
          match f.entry_info with
          | ENTRY_function info -> info.function_global
          | _ -> internal "Not a function"; raise Terminate in
        Hashtbl.iter handle_hash_entry called_info;
        )
      | _ -> () in 

    (* Perform the actual computation *)
    Array.iter (Array.iter handle_quad) fun_code in
  
  (* Now use the compute_for_function to compute for all functions *) 
  (* Reverse order to account for all calls *)
  let n = Array.length quads in
  let rec walk i =
    if i >= 0 
    then (
      compute_for_function quads.(i);
      walk (i-1);
    ) in
  walk (n-1)

(* Temporary Use - Define Chains *)
type temporary_info_t = {
  def_block  : int;
  def_offset : int;
  use_block  : int;
  use_offset : int;
  mutable temp_value : int option;
}

let single_compute_temporary_info flowgraph =
  let temp_hash = Hashtbl.create 17 in
  let result_hash = Hashtbl.create 17 in
  let handle_use q i j = 
    if is_temporary q then try 
      let (id, jd) = Hashtbl.find temp_hash q in
      let temp_info = {
        def_block  = id;
        def_offset = jd;
        use_block  = i;
        use_offset = j;
        temp_value = None
      } in
      Hashtbl.add result_hash q temp_info
    with
      Not_found -> 
        internal "Temporary without preceeding definition, %s %d %d"
         (string_of_quad_elem_t q) i j;
        raise Terminate in     
  let handle_def q i j b = 
    if is_temporary q then
      if is_valof q && not b then
        handle_use q i j
      else Hashtbl.add temp_hash q (i,j) in
  let handle_block i node =
    let handle_quad j = function
    | Quad_calc (_, q1, q2, q3) ->
        handle_use q1 i j;
        handle_use q2 i j;
        handle_def q3 i j false
    | Quad_set (q1,q2) ->
        handle_use q1 i j;
        handle_def q2 i j false
    | Quad_array (_, q, e) ->
        handle_use q i j;
        handle_def (Quad_valof e) i j true
    | Quad_cond (_, q1, q2, _) ->
        handle_use q1 i j;
        handle_use q2 i j
    | Quad_par (q, pm) ->
        if pm = PASS_BY_VALUE 
        then handle_use q i j
        else handle_def q i j false
    | _ -> () in
    Array.iteri handle_quad node.code_block in
  Array.iteri handle_block flowgraph;

  (* Return the result hash *)
  result_hash

let compute_temporary_info flowgraphs = 
  Array.map single_compute_temporary_info flowgraphs
