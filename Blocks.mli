type basic_block_t = Quads.quad_t array
type function_block_t = basic_block_t array

val blocks_of_quad_t_list : Quads.quad_t list -> function_block_t array
val output_block_code : out_channel -> function_block_t array -> unit
