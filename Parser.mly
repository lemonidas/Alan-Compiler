%{

open Format

open Lexing

open Error
open Identifier
open Types
open Symbol
open Printing
open Quads
open Parsing
open Semantic

(*Uncomment for more debugging...*)
(*Parsing.set_trace true;;*)

(* Simple Function to get Expression Position *)
let get_binop_pos () =
   (rhs_start_pos 1, 
	rhs_start_pos 3)
;;

(* Function to Register the parameter list of a function *)
let rec registerParams p = function
	|[]-> ()
	|((p_name, p_type, p_ref)::t) -> (
	ignore (newParameter(id_make p_name) p_type p_ref p true);
	registerParams p t;
	)				
;;


(* Function to Register a Function *)
let rec registerFunction id param_list ret_type sc=
	let p = newFunction (id_make id) true in
		openScope ret_type;
		registerParams p param_list;
		endFunctionHeader p ret_type;
		if(sc) then () else closeScope p;
		p
;;

(* Called to Register the external Library *)
let registerLibrary() =
	let p = registerFunction "writeInteger" 	
		[("n", TYPE_int, PASS_BY_VALUE)] 
		TYPE_proc true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "writeByte" 	
		[("b", TYPE_byte, PASS_BY_VALUE)] 
		TYPE_proc true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "writeChar" 	
		[("b", TYPE_byte, PASS_BY_VALUE)] 
		TYPE_proc true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "writeString" 	
		[("s", TYPE_array(TYPE_byte,0), PASS_BY_REFERENCE)] 
		TYPE_proc true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "readInteger" 	
		[] 
		TYPE_int true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "readByte" 	
		[] 
		TYPE_byte true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "readChar" 	
		[] 
		TYPE_byte true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "readString" 	
		[("n", TYPE_int, PASS_BY_VALUE);
		("s", TYPE_array(TYPE_byte,0), PASS_BY_REFERENCE)]
	 	TYPE_proc true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "extend" 
		[("b",TYPE_byte, PASS_BY_VALUE)] 
		TYPE_int true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "shrink" 
		[("i", TYPE_int, PASS_BY_VALUE)] 
		TYPE_byte true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "strlen" 
		[("s", TYPE_array(TYPE_byte,0), PASS_BY_REFERENCE)] 
		TYPE_int true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "strcmp"
		 [("s1", TYPE_array(TYPE_byte,0), PASS_BY_REFERENCE);
		 ("s2", TYPE_array(TYPE_byte,0), PASS_BY_REFERENCE)]
		 TYPE_int true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "strcpy"
		 [("trg", TYPE_array(TYPE_byte,0), PASS_BY_REFERENCE);
		 ("src", TYPE_array(TYPE_byte,0), PASS_BY_REFERENCE)]
		 TYPE_proc true in p.entry_scope.sco_nesting <- max_int;
	let p = registerFunction "strcat"
		 [("trg", TYPE_array(TYPE_byte,0), PASS_BY_REFERENCE);
		 ("src", TYPE_array(TYPE_byte,0), PASS_BY_REFERENCE)]
		 TYPE_proc true in p.entry_scope.sco_nesting <- max_int;
;;

%}


%token<(string*string)> T_Char
%token<(string*string)> T_String
%token<string> T_Const
%token<string> T_Id

%token T_Byte
%token T_Else
%token T_False
%token T_If
%token T_Int
%token T_Proc
%token T_Ref
%token T_Return
%token T_While
%token T_True

%token T_Set
%token T_Add
%token T_Sub
%token T_Mult
%token T_Div
%token T_Mod

%token T_Not
%token T_And
%token T_Or
%token T_Eq
%token T_Neq
%token T_Leq
%token T_Le
%token T_Geq
%token T_Ge

%token T_LParen
%token T_RParen
%token T_LSq_Bracket
%token T_RSq_Bracket
%token T_LCur_Bracket
%token T_RCur_Bracket

%token T_Comma
%token T_Colon
%token T_Semicolon

%token T_No_type
%token T_Eof

%left T_Or
%left T_And
%nonassoc T_Eq T_Neq T_Ge T_Geq T_Le T_Leq
%left T_Add T_Sub
%left T_Mult T_Div T_Mod
%nonassoc T_Not

%start program
%type <Quads.quad_t list> program
%type <expr_ret_type> expr
%type <expr_ret_type> func_call
%type <expr_ret_type> l_value
%type <cond_ret_type> cond
%type <Quads.quad_t list> local_def

%% 
program:			initialization first_func_def T_Eof {$2}
					|error T_Eof {
						fatal "Invalid program: \
						Invalid main function definition.";
						raise Terminate
					};

initialization:		{initSymbolTable 256};

first_func_def:		reg_lib func_header local_def compound_stmt {
						check_first_proc $2;
						handle_func_def (id_name $2.entry_id) $3 $4;
					};

reg_lib:			{registerLibrary();}

func_def:			func_header local_def compound_stmt {
						closeScope $1; 
						handle_func_def (id_name $1.entry_id) $2 $3;
					}

func_header:		T_Id T_LParen fpar_list T_RParen T_Colon r_type {
						registerFunction $1 $3 $6 true
					};

fpar_list:			{[]}
					|fpar_def fpar_list2 {($1::$2)}

fpar_list2:			{[]}
					|T_Comma fpar_def fpar_list2 {($2::$3)};

fpar_def:			T_Id T_Colon ttype {
						if (check_array_reference $1 $3 (symbol_start_pos ())) 
						then ($1, $3, PASS_BY_VALUE)
						else ($1, $3, PASS_BY_REFERENCE)
					}
					|T_Id T_Colon T_Ref ttype {
						($1, $4, PASS_BY_REFERENCE) 
					};

data_type:			T_Int { TYPE_int }
					|T_Byte { TYPE_byte };

ttype:				data_type {$1}
					|data_type T_LSq_Bracket T_RSq_Bracket {
						TYPE_array($1,0)
					}
					|data_type error T_RSq_Bracket {
						let pos = Parsing.symbol_start_pos () in
						error "Missing '[' in parameter declaration \
							in line: %d, position: %d." 
							pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
						TYPE_none
					};

r_type:				data_type {$1}
					|T_Proc {TYPE_proc};

local_def:			func_def local_def {$2@$1}
					|var_def local_def {$2}
					|{[]};

var_def:			T_Id T_Colon data_type T_Semicolon {
						ignore (newVariable(id_make $1) $3 true);
					}
					|T_Id T_Colon data_type T_LSq_Bracket T_Const 
											T_RSq_Bracket T_Semicolon{
						ignore(newVariable(id_make $1) 
							(TYPE_array($3,int_of_string $5)) true);
					}
					|T_Id T_Colon data_type error T_Semicolon {
						let pos = Parsing.symbol_start_pos () in
						error "Invalid Variable Definition [%s]: Expected \
							';' or '[const]' after datatype declaration: \
							[%s], at Line: %d, Position: %d"
							$1 (string_of_typ $3) 
							 pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
					}					
					|T_Id T_Colon error T_Semicolon {
						let pos = Parsing.symbol_start_pos () in
						error "Invalid Variable Definition [%s]: Wrong \
							datatype after ':', at Line: %d, Position: %d" 
							$1 pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
					}
					|T_Id error T_Semicolon {						
						let pos = Parsing.symbol_start_pos () in
						error "Invalid Variable Definition [%s]: Expected \
						 ':' after variable name, at Line: %d, Position: %d" 
						 $1 pos.pos_lnum (pos.pos_cnum - pos.pos_bol)}
					|T_Id error T_Eof {
						let pos = Parsing.symbol_start_pos () in
						fatal "Invalid local Definition at Line: %d, \
						Position: %d. Missing Ending Semicolon." 
						pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
					}

stmt:				T_Semicolon {[]}
					|l_value T_Set expr T_Semicolon {
						handle_assignment (dereference $1) $3 (get_binop_pos())
					}
					|compound_stmt {$1}
					|func_call T_Semicolon {
						check_func_proc $1 (symbol_start_pos ())
					}
					|if_stmt{$1}
					|T_While T_LParen cond T_RParen stmt {
						handle_while_stmt $3 $5
					}
					|T_Return expr T_Semicolon {
						handle_return_expr $2 (symbol_start_pos ())
					}
					|T_Return T_Semicolon {
						handle_return_proc (symbol_start_pos ())
					}
					|error T_Semicolon {
						let pos = Parsing.symbol_start_pos () in
						error "Invalid statement starting at line %d, \
							position %d"
							pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
						[]
					}
					|error T_RCur_Bracket {
						let pos = Parsing.symbol_start_pos () in
						fatal "Statement starting at Line: %d, \
							Position: %d is missing ending semicolon"
							pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
						[]
					}
					|error T_Eof {
						let pos = Parsing.symbol_start_pos () in
						fatal "Statement starting at Line: %d, \
							Position: %d is missing ending semicolon" 
							pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
						[]
					};						

if_stmt:			T_If T_LParen cond T_RParen stmt {
						handle_if_stmt $3 $5
					}
					|T_If T_LParen cond T_RParen stmt T_Else stmt {
						handle_if_else_stmt $3 $5 $7
					}
					|T_If T_LParen cond error T_Semicolon {
						let pos = Parsing.symbol_start_pos () in
						error "Invalid If-Statement: Expected ')' \
							after condition at Line: %d, Position: %d" 
							pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
						[]
					}
					|T_If T_LParen error T_Semicolon {
						let pos = Parsing.symbol_start_pos () in
						error "Invalid If-Statement: Expected ')' \
							after condition at Line: %d, Position: %d" 
							pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
						[]
					}
					|T_If error T_Semicolon {
						let pos = Parsing.symbol_start_pos () in
						error "Invalid If-Statement: Expected '(' \
							after 'if' at Line: %d, Position: %d"
							pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
						[]
					};
			
compound_stmt:		T_LCur_Bracket stmt_list T_RCur_Bracket {$2}	
					|T_LCur_Bracket T_RCur_Bracket {[]};

stmt_list:			stmt_list stmt { $2 @ $1 }
					|stmt { $1 };

func_call:			T_Id T_LParen T_RParen {
						let pos = Parsing.symbol_start_pos () in
						handle_func_call $1 pos []
					}
					|T_Id T_LParen expr_list T_RParen {
						let pos = Parsing.symbol_start_pos () in
						handle_func_call $1 pos $3
					};

expr_list:			expr{[$1]}
					|expr_list T_Comma expr{($3::$1)};
			
expr:				expr T_Add expr {
						handle_expression "+" $1 $3 (get_binop_pos ())
					}
					|expr T_Sub expr {
						handle_expression "-" $1 $3 (get_binop_pos ())
					}
					|expr T_Mult expr {
						handle_expression "*" $1 $3 (get_binop_pos ())
					}
					|expr T_Div expr {
						handle_expression "/" $1 $3 (get_binop_pos ())
					}
					|expr T_Mod expr {
						handle_expression "%" $1 $3 (get_binop_pos ())
					}
					|T_LParen expr T_RParen {$2}
					|T_Const {
						{code = [];	place = Quad_int($1)}
					}
					|T_Char {
						{code = []; place = Quad_char((snd $1))}
					}
					|T_String {
						{code = []; place = Quad_string((fst $1))}
					}
					|l_value { 	dereference $1 }			
					|func_call {
						check_func_expr $1 (symbol_start_pos())
					}
					|T_Add expr %prec T_Not{
						handle_unary_expression "+" $2 (rhs_start_pos 2)
					}
					|T_Sub expr %prec T_Not{
						handle_unary_expression "-" $2 (rhs_start_pos 2)
					};
			
l_value:			T_Id {
						handle_simple_lvalue $1 (symbol_start_pos ())
					}
					|T_Id T_LSq_Bracket expr T_RSq_Bracket {
						let context = (rhs_start_pos 3, rhs_end_pos 3) in
						let pos = symbol_start_pos () in
						handle_array_lvalue $1 pos context $3
					};

cond:				T_True {handle_cond_const true}
					|T_False{handle_cond_const false}
					|T_LParen cond T_RParen {$2}
					|T_Not cond {
						{$2 with q_true = $2.q_false; q_false = $2.q_true}
					}	
					|expr T_Eq expr {
						handle_comparison "==" $1 $3 (get_binop_pos())
					}
					|expr T_Neq expr {
						handle_comparison "!=" $1 $3 (get_binop_pos())
					}
					|expr T_Leq expr {
						handle_comparison "<=" $1 $3 (get_binop_pos())
					}
					|expr T_Geq expr {
						handle_comparison ">=" $1 $3 (get_binop_pos())
					}
					|expr T_Le expr {
						handle_comparison "<" $1 $3 (get_binop_pos())
					}
					|expr T_Ge expr {
						handle_comparison ">" $1 $3 (get_binop_pos())
					}
					|cond T_And cond {handle_and $1 $3}
					|cond T_Or cond {handle_or $1 $3}
					;;
