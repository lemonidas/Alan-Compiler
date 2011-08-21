open OptimizationSupport
open QuadTypes
open Quads
open Blocks
open Error

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
    | Quad_cond _ -> insert_edge i (i+1); walk_rev (code_length -1)
    | Quad_jump _
    | Quad_ret -> walk_rev (code_length - 1)
    | _ -> if (i < total_blocks - 1) then insert_edge i (i+1) in
  Array.iteri parse_block fun_block;
  flowgraph

(* Uses the above function to create an array of Flowgraphs,
 * one for each function *)
let flowgraph_array_of_quads quads =
  Array.of_list (
    Array.fold_left 
      (fun acc x -> (flowgraph_t_of_function_block_t x)::acc) 
      [] quads)

(* Converts a flowgraph to an array of arrays *)
let quads_of_flowgraph flowgraph =
  let n = Array.length flowgraph in
  let quads = Array.make n (Array.make 0 Quad_dummy) in
  for i = 0 to n-1 do
    quads.(i) <- flowgraph.(i).code_block;
  done;
  quads
  
let convert_back_to_quads flowgraphs =
  Array.of_list (
    Array.fold_left 
      (fun acc x -> (quads_of_flowgraph x)::acc)
      [] flowgraphs)

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
  (* n+1 sized arrays, .(n) is n0 *)
  let bucket = Array.make (n+1) Sint.empty in
  let sdno = Array.make (n+1) 0 in
  let idom = Array.make (n+1) 0 in
  let parent = Array.make (n+1) 0 in
  let ndfs = Array.make (n+1) 0 in
  let ancestor = Array.make (n+1) 0 in
  let child = Array.make (n+1) 0 in
  let label = Array.make (n+1) 0 in
  let size = Array.make (n+1) 0 in

  let debug_mode = false in

  let print_array a =
    Printf.printf "%s: " (fst a);
    Array.iter (Printf.printf "%d ") (snd a);
    Printf.printf "\n";
    in

  let print_state () =
    List.iter print_array 
      [("Sdno",sdno); ("Idom",idom); ("Parent", parent); 
       ("Ndfs",ndfs); ("Ancestor", ancestor); ("Child",child); 
       ("Label",label); ("Size",size)] in

  (* DFS function with initializations *)
  let rec dfs i cnt =
    sdno.(i) <- cnt;
    label.(i) <- i;
    ndfs.(cnt) <- i;
    ancestor.(i) <- n;
    child.(i) <- n;
    size.(i) <- n;
    let rec parse_children children cnt =
      match children with
      | [] -> cnt
      | (h::t) ->
          if (sdno.(h) = 0)
          then (
            parent.(h) <- i;
            let new_cnt = dfs h cnt in
            parse_children t new_cnt
          ) 
          else parse_children t cnt in
    parse_children flowgraph.(i).children (cnt+1) in

  (* Path compression Function *)
  let rec compress v = 
    if (ancestor.(ancestor.(v)) != n) 
    then (
      compress ancestor.(v);
      if (sdno.(label.(ancestor.(v))) < sdno.(label.(v)))
        then label.(v) <- label.(ancestor.(v));
      ancestor.(v) <- ancestor.(ancestor.(v));
    ) in (* End compress *)    

  (* Function to evaluate a node *)
  let eval v = 
    if ancestor.(v) = n
    then label.(v)
    else (
      compress v;
      if (sdno.(label.(ancestor.(v))) >= sdno.(label.(v)))
      then label.(v)
      else label.(ancestor.(v))
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
      if s < n
      then (
        ancestor.(s) <- v;
        go_to_n0 (child.(s))
      )
      else () in
    go_to_n0 new_s in (* End link *)
  
  let rec populate_buckets i =
    if i <= 0 then () 
    else (
      let w = ndfs.(i) in

      (* loop through parents to set semidominators *)
      let rec parse_parents parents =
        if(debug_mode) then Printf.printf "Parsing parents of %d\n" w;
        match parents with 
        | [] -> ()
        | (h::t) ->
            if (debug_mode) then Printf.printf "Going through %d\n" h;
            let u = eval h in
            if (sdno.(u) < sdno.(w))      
            then begin
              sdno.(w) <- sdno.(u);
              if (debug_mode) then (
                Printf.printf "Changing sdno.(%d) to sdno.(%d) = %d\n" 
                  w u sdno.(u); flush_all();
              )
            end;            
            parse_parents t in
      parse_parents flowgraph.(w).parents;
      let tmp = ndfs.(sdno.(w)) in
      bucket.(tmp) <- Sint.add w bucket.(tmp);

      let p = parent.(w) in
      link p w;

      let rec compute_for_bucket () =
        if (not (Sint.is_empty bucket.(parent.(w))))
        then (
          let v = Sint.choose bucket.(parent.(w)) in
          bucket.(parent.(w)) <- Sint.remove v bucket.(parent.(w));
          let u = eval v in
          idom.(v) <- if (sdno.(u) < sdno.(v)) then u else parent.(w);
          compute_for_bucket ()
        ) in
      compute_for_bucket ();

      populate_buckets (i-1)
    ) in
    
  let rec adjust_idoms i =
    if i >= n then ()
    else (
      let w = ndfs.(i) in
      if (idom.(w) != ndfs.(sdno.(w)))
      then idom.(w) <- idom.(idom.(w))
      else ();
      adjust_idoms (i+1);
    ) in

  (* Main Logic Now *)
  ignore (dfs 0 0);
  
  if(debug_mode) then (
    print_state (); Printf.printf "\n"; flush_all();
  );

  populate_buckets (n-1);
  
  if (debug_mode) then (
   print_state (); Printf.printf "\n"; flush_all();
  );

  adjust_idoms 1;
  
  if (debug_mode) then (
    print_state ();
    flush_all();
  );
  
  (* Return idom *)
  idom
  
  
