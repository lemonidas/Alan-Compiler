open QuadTypes
open Quads

type basic_block_t = quad_t array
type function_block_t = basic_block_t array

(* Splits a single function block into flow blocks 
 * IN  : Quad_t array
 * OUT : Array of quad_t array
 *) 
let convert_function_to_blocks im_code =
  
  (* Variable Declarations *)
  let length = Array.length im_code in
  let breakpoint = Array.make length false in
  let blocks = Array.make length 0 in
  let block_no = ref 0 in

  (* Helper Functions *)
  let extract_jump_value = function
    | Quad_jump (x)
    | Quad_cond  (_,_,_,x)
      -> Some (!x)
    | _ -> None
  in
  
  let is_jump = function
    | Quad_jump (_)
    | Quad_cond (_)
      -> true
    | _ -> false
  in

  let convert_jump quad = 
    match quad with
    | Quad_jump (x)
    | Quad_cond (_, _, _, x) 
      -> x := blocks.(!x); quad
    | _ -> quad
  in
  
  (* Important Functions Before Unit Calls *)
  
  (* Initialize break points *)
  let handle_quad_break x =
    match (extract_jump_value x) with
    | None -> ()
    | Some (n) -> breakpoint.(n) <- true
  in
  
  (* Main function to loop through array *)
  let rec walk i jump_mode =
    if (i >= length) 
    then ()
    else match (breakpoint.(i), jump_mode, is_jump im_code.(i)) with
    | (true,_,b) ->
      (* In case of breakpoint - increase block number *)
      incr(block_no);
      blocks.(i) <- !block_no;
      walk (i+1) b
    | (false, true, true) ->
      (* Else, in case previous and current is jump continue *)
      blocks.(i) <- !block_no;
      walk (i+1) true
    | (false, true, false) ->
      (* If previous was jump and this isn't -> break block *)
      incr(block_no);
      blocks.(i) <- !block_no;
      walk (i+1) false
    | (false, false, b) ->
      (* Else just continue recursively *)
      blocks.(i) <- !block_no;
      walk (i+1) b
  in
  
  (* Function to Return the next block and various info *)
  let rec next_block i index acc =
    if (i >= length) then 
      (List.rev acc, (i+1))
    else if (blocks.(i) > index) then
      (List.rev acc, (i))
    else 
      next_block (i+1) index ((convert_jump im_code.(i))::acc)
  in

  (* Loops through all instructions forming the actual blocks *)
  let rec loop i acc =
    if (i >= length)
    then 
      Array.of_list (List.rev acc)
    else
      let next_block_id = blocks.(i) in
      let (quads, new_i) = next_block i next_block_id [] in
      loop new_i ((Array.of_list quads)::acc)
  in

  (* Main Point *)
  Array.iter handle_quad_break im_code;
  breakpoint.(0) <- false; (* Zero is allready a break point *)
  breakpoint.(1) <- true; (* Make sure unit is a single entry block *)
  breakpoint.(length-1) <- true; (* Make sure endu is a single end block *)
  walk 0 false;
  loop 0 []

(* Uses the function above to convert all function blocks to flow_blocks 
 * IN  : List of quad_t array grouped by Function
 * OUT : Array of Array of quad_t array grouped first by function, then flow
 *)
let convert_to_blocks fun_code_list =
  Array.of_list (List.fold_left 
    ( fun acc fun_block -> 
        (convert_function_to_blocks fun_block)::acc ) 
    [] fun_code_list )

(* Function to break initial im_code_list into function blocks 
 * IN  : Intermediate Code List
 * OUT : List of quad_t array grouped by Function
 *)
let convert_to_function_blocks im_code_list =
  let rec block_helper acc func_acc i = function
    | [] -> acc
    | (h::t) ->
        let new_func_acc = h::func_acc in
        match h with
        | Quad_endu _ ->
            let func_array = Array.of_list (List.rev new_func_acc) in
            block_helper (func_array::acc) [] 0 t
        | Quad_cond (_,_,_,offset)
        | Quad_jump offset ->
            offset := i + !offset;
            block_helper acc new_func_acc (i+1) t
        | _ -> 
            block_helper acc new_func_acc (i+1) t
  in block_helper [] [] 0 im_code_list

(* Export the following *)

(* Main conversion function:
 * IN  : Quad_t list
 * OUT : Quad_t array array array
 *)
let blocks_of_quad_t_list quad_list= 
   convert_to_blocks (convert_to_function_blocks quad_list)

(* Printing Function for intermediate code to out_chan
 * IN  : Array of Array of quad_t array... Lots of nesting...
 * OUT : Unit function
 *)
let output_block_code out_chan block_code=
  let output_single quad = 
    Printf.fprintf out_chan "%s\n" (string_of_quad_t quad)
  in let output_block i block =
    Printf.fprintf out_chan "Block %d\n" i;
    Array.iter output_single block
  in let output_function fun_block =
    Array.iteri output_block fun_block
  in Array.iter output_function block_code


