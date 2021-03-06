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

let flag_jump_simplification = ref false

let usage = "Usage: alanc file [options] ...\n\nOptions:"

let spec = Arg.align [
  "-i", Arg.Unit    (function () -> mode := Intermediate)
      , "Output Intermediate Code";
  "-f", Arg.Unit    (function () -> mode := Final)       
      , "Output Final Code";
  "-O", Arg.Unit    (function () -> optimizations := true)
      , "Enable Optimizations";
  "-fjmp", Arg.Unit (function () -> flag_jump_simplification := true)
      , "Enable Jump Simplification Optimization. (Caution: long jump errors possible";
  "-dlex", Arg.Unit (function () -> Debug.debug_lexer := true)
      , "Enable lexer debugging messages";
  "-dprs", Arg.Unit(function () -> Debug.debug_parser := true)
      , "Enable parser debugging messages";
  "-dc_fold", Arg.Unit(function () -> Debug.debug_constant_folding := true)
      , "Enable constant folding debugging messages";
  "-dunr_s", Arg.Unit(function () -> Debug.debug_unreachable_simple := true)
      , "Enable simple unreachable code elimination debugging messages";
  "-dg_defs", Arg.Unit(function () -> Debug.debug_global_def_computation := true)
      , "Enable global definition computation debugging messages";
  "-djmp", Arg.Unit(function () -> Debug.debug_jump_simplification := true)
      , "Enable jump simplification debugging messages";
  "-ddum", Arg.Unit(function () -> Debug.debug_dummy_elimination := true)
      , "Enable dummy elimination debugging messages";
  "-dflow", Arg.Unit(function () -> Debug.debug_flowgraph := true)
      , "Enable flowgraph creation debugging messages";
  "-dunr", Arg.Unit(function () -> Debug.debug_unreachable := true)
      , "Enable control-flow based unreachable code elimination debugging messages";
  "-dtail_r", Arg.Unit(function () -> Debug.debug_tail_recursion := true)
      , "Enable tail recursion elimination debugging messages";
  "-dreach", Arg.Unit(function () -> Debug.debug_reaching_definitions := true)
      , "Enable reaching definitions debugging messages";
  "-dssa", Arg.Unit(function () -> Debug.debug_ssa := true)
      , "Enable ssa debugging messages";
  "-ddead", Arg.Unit (function () -> Debug.debug_dead_code_elimination := true)
      , "Enable dead code elimination debugging messages";
  "-dtemp", Arg.Unit (function () -> Debug.debug_temporary_deletion := true)
      , "Enable temporary variables deletion debug messages"
]

let anon_fun str = in_file := Some str       

let rec optimize block_code =
  if (!optimizations)
  then (
  
    (* First optimization is allways immediate backward *)
    Optimizations.immediate_backward_propagation block_code;

    (* Constant Folding - No longer needed *)
    (* Optimizations.constant_folding block_code; *)

    (* Unreachable simple deletions *)
    CodeElimination.perform_deletions block_code;
    
    (* Compute some necessary information for inter-procedural stuff *)
    OptimizationSupport.compute_global_definitions block_code;
  
    (* Simplify jumps *)
    if (!flag_jump_simplification) then
    Optimizations.jump_simplification block_code;

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
    
    (* Reaching definitions to produce UD/DU Chains *)
    let temp_chains = OptimizationSupport.compute_temporary_info flowgraphs in
    let chains = UDChains.reaching_definitions flowgraphs in

    (* Check for uninitialized values *)
    UDChains.check_uninitialized_values chains;

    (* Constant propagation *)
    let simplified_jump = 
      ConstantPropagation.constant_propagation flowgraphs chains temp_chains in

    (* Use chains to perform dead code elimination *)
    DeadCodeElimination.dead_code_elimination flowgraphs chains temp_chains;

    (* Convert back *)
    let block_code = ControlFlow.convert_back_to_quads flowgraphs in

    CodeElimination.delete_temporary_variables block_code;    

    (* Dummy Elimination again to lighten code after eliminations *)
    Optimizations.dummy_elimination block_code;

    if (simplified_jump) then optimize block_code
    else block_code
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
