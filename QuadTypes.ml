open Types
open Identifier
open Printing
open Symbol
open Error

(* The main element in quads *)
type quad_elem_t =
  |Quad_none                        (* Error Handling              *)
  |Quad_entry of Symbol.entry       (* Symbol Table Entries        *)
  |Quad_valof of Symbol.entry       (* Dereferenced Symbol Entries *)
  |Quad_int of string               (* Constant Integers           *)
  |Quad_char of string              (* Constant Characters         *)
  |Quad_string of string            (* Constant Strings            *)

let string_of_quad_elem_t = function
  |Quad_none          -> ""
  |Quad_entry ent     -> id_name ent.entry_id
  |Quad_valof ent     -> Printf.sprintf "[%s]" (id_name ent.entry_id)
  |Quad_int str       -> str
  |Quad_char str      -> str
  |Quad_string str    -> Printf.sprintf "\"%s\"" str

(* All quad types of the intermediate code *)
type quad_t =
  |Quad_dummy  (* For optimization purposes *)
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
  
(* Return Type of an Expression *)
type expr_ret_type = {
  code : quad_t list;
  place : quad_elem_t;
}

(* Return Type of a Condition 
 * Jumps are handled as relative jumps at first, converted later *)
type cond_ret_type = {
  c_code : quad_t list; 
  q_true: int ref list;   
  q_false : int ref list;
}

(* Returning a "null" quad - error handling mostly *)
let return_null () = {code = []; place = Quad_none}

let find_opposite_condition = function
  | "==" -> "!="
  | "!=" -> "=="
  | "<=" -> ">"
  | ">"  -> "<="
  | ">=" -> "<"
  | "<"  -> ">="
  | _ -> internal "Not a conditional string"; raise Terminate

let equal_quad_elems = function
  | Quad_none, Quad_none -> true
  | Quad_valof e1, Quad_valof e2
  | Quad_entry e1, Quad_entry e2 -> Symbol.equalEntries e1 e2
  | Quad_int s1, Quad_int s2
  | Quad_char s1, Quad_char s2
  | Quad_string s1, Quad_string s2 -> s1 = s2
  | _ -> false
  
