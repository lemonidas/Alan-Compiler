type quad_elem_t =
	|Quad_none                        (* Error Handling              *)
	|Quad_entry of Symbol.entry       (* Symbol Table Entries        *)
	|Quad_valof of Symbol.entry       (* Dereferenced Symbol Entries *)
	|Quad_int of string               (* Constant Integers           *)
	|Quad_char of string              (* Constant Characters         *)
	|Quad_string of string	          (* Constant Strings            *)

val string_of_quad_elem_t : quad_elem_t -> string

type quad_t =
	|Quad_dummy  
	|Quad_unit of Symbol.entry
	|Quad_endu of Symbol.entry
	|Quad_calc of string * quad_elem_t * quad_elem_t * quad_elem_t
	|Quad_set of quad_elem_t * quad_elem_t
	|Quad_array of quad_elem_t * quad_elem_t * Symbol.entry
	|Quad_cond of string * quad_elem_t * quad_elem_t * (int ref)
	|Quad_jump of (int ref)
	|Quad_call of Symbol.entry * (quad_elem_t list)
  |Quad_tailCall of Symbol.entry
	|Quad_par of quad_elem_t * Symbol.pass_mode
	|Quad_ret
	
type expr_ret_type = {
	code : quad_t list;
	place : quad_elem_t;
}

type cond_ret_type = {
	c_code : quad_t list;	
	q_true: int ref list;   
	q_false : int ref list;
}

val return_null : unit -> expr_ret_type

val find_opposite_condition : string -> string

val equal_quad_elems : quad_elem_t * quad_elem_t -> bool
