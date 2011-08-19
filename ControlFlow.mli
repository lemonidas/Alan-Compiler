open QuadTypes 
open OptimizationSupport

val flowgraph_array_of_quads : quad_t array array array -> flowgraph_t array
val convert_back_to_quads : flowgraph_t array -> quad_t array array array
val compute_immediate_dominators : flowgraph_t -> int array
