open Parser
open Error
open Lexing
open Quads
open Final
open Symbol
open QuadTypes

type mode_t = 
  | Normal 
  | Intermediate 
  | Final

let mode          = ref Normal
let optimizations = ref false
let in_file       = ref None

let usage = "Usage: alanc file [options] ...\n\nOptions:"

let spec = Arg.align [
  "-i", Arg.Unit    (function () -> mode := Intermediate)
      , "Output Intermediate Code";
  "-f", Arg.Unit    (function () -> mode := Final)       
      , "Output Final Code";
  "-O", Arg.Unit    (function () -> optimizations := true)
      , "Enable Optimizations";
]

let anon_fun str = in_file := Some str       

let optimize block_code =
  if (!optimizations)
  then (
  
    (* First optimization is allways immediate backward *)
    Optimizations.immediate_backward_propagation block_code;

    (* Constant Folding *)
    Optimizations.constant_folding block_code;

    (* Unreachable simple deletions *)
    CodeElimination.perform_deletions block_code;
    
    (* Compute some necessary information for inter-procedural stuff *)
    OptimizationSupport.compute_global_definitions block_code;
  
    (* Simplify jumps *)
    (*Optimizations.jump_simplification block_code;*)

    (* Dummy elimination *)
    Optimizations.dummy_elimination block_code;
    
    (* Convert to flowgraph for further optimizations *)
    let flowgraphs = ControlFlow.flowgraph_array_of_quads block_code in
    
    (* Unreachable Code Elimination *)
    CodeElimination.delete_unreachable_blocks flowgraphs;
  
    (* Copy Propagation *)
    Array.iter CopyPropagation.copy_propagation flowgraphs;

    (* Tail Recursion *)
    TailRecursion.tail_recursion_elimination flowgraphs;
    
    (* Reaching definitions *)
    UDChains.reaching_definitions flowgraphs.(1);

    (* Convert back *)
    let block_code = ControlFlow.convert_back_to_quads flowgraphs in
    
    block_code
    )
  else block_code
  
let main = 
  Arg.parse spec anon_fun usage;
  let in_channel = 
    match !in_file with
    |None -> stdin; 
    |Some(str) -> open_in str
  in 
  let lexbuf = Lexing.from_channel in_channel in
  let code_list = List.rev (Parser.program Lexer.lexer lexbuf) in
  let block_code = Blocks.blocks_of_quad_t_list code_list in
  let block_code = optimize block_code in
  match !mode with
  |Intermediate ->
    Blocks.output_block_code stdout block_code
  |Final ->
    output_final_code stdout block_code (!optimizations)
  |Normal ->
    match (!in_file) with
    |None -> 
      internal "Normal mode reads from source code";
      raise Terminate
    |Some(str) ->
      (* Extract main name *)
      let base = String.sub str 0 ((String.length str) - 5) in
      let imm_channel = open_out (base^".imm") in (
        Blocks.output_block_code imm_channel block_code;
        output_final_code (open_out (base^".asm")) block_code (!optimizations);
      )
