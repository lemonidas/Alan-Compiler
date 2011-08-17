type basic_block_t = QuadTypes.quad_t array
type function_block_t = basic_block_t array

val blocks_of_quad_t_list : QuadTypes.quad_t list -> function_block_t array
val output_block_code : out_channel -> function_block_t array -> unit
