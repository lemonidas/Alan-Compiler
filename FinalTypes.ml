(* Final Types *)
type register = Ax | Bx | Cx | Dx | Al | Ah | Di | Si | Bp | Sp

let string_of_register = function
	| Ax -> "ax"
	| Bx -> "bx"
	| Cx -> "cx"
	| Dx -> "dx"
	| Al -> "al"
	| Ah -> "ah"
	| Di -> "di"
	| Si -> "si"
	| Bp -> "bp"
	| Sp -> "sp"

type action_arg = 
	|Action_reg of register
	|Constant of string

let string_of_action_arg = function
	| Action_reg reg -> string_of_register reg
	| Constant str -> str

type mem_loc = 
	| Register of register 
	| Mem_loc of (string * register * int)
	| String_addr of string
	| Offset of string

let string_of_mem_loc = function
	| Register reg -> (string_of_register reg)
	| Mem_loc (size, reg, offset) ->
		Printf.sprintf "%s ptr [%s %+d]" 
			size (string_of_register reg) offset
	| String_addr str -> str
	| Offset str -> str

type final_t =
	| Start of string
	| End of string
	| Mov of (mem_loc * mem_loc)
	| Call of string
	| Lea of (mem_loc * mem_loc)
	| Jump of string
	| Cond_jump of (string * string)
	| Add of (action_arg * action_arg)
	| Sub of (action_arg * action_arg)
	| IMul of register
	| IDiv of register
	| Push of register
	| Pop of register
	| Pwd
	| Ret 
	| Label of string
	| Misc of string

let string_of_final_t = function
	| Start str -> str
	| End str -> str
	| Mov (m1, m2) -> 
		Printf.sprintf "\tmov\t%s, %s\n" 
		(string_of_mem_loc m1) 
		(string_of_mem_loc m2)
	| Call str -> 
		Printf.sprintf "\tcall\tnear ptr %s\n" str
	| Lea (m1,m2) ->
		Printf.sprintf "\tlea\t%s, %s\n" 
		(string_of_mem_loc m1) 
		(string_of_mem_loc m2)
	| Jump str -> 
		Printf.sprintf "\tjmp\t%s\n" str
	| Cond_jump (c,str) ->
		Printf.sprintf "\t%s\t%s\n" c str
	| Add (a1, a2) ->
		Printf.sprintf "\tadd\t%s, %s\n"
		(string_of_action_arg a1)
		(string_of_action_arg a2)
	| Sub (a1, a2) ->
		Printf.sprintf "\tsub\t%s, %s\n"
		(string_of_action_arg a1)
		(string_of_action_arg a2)
