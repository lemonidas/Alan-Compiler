open Types
open Error
open Symbol
open Printing
open Identifier
open Semantic
open Lexing
open QuadTypes

(* Small Function To Check if Quad is en entry or not *)
let is_entry quad =
  match quad with
  | Quad_entry(_) -> true
  | _ -> false

let is_not_temporary quad =
  match quad with
  | Quad_entry ent -> (
    match ent.entry_info with
    | ENTRY_variable _ 
    | ENTRY_parameter _ -> true
    | _ -> false
    )
  | _ -> false

(* Handling [x] case *)
let dereference x = 
  match x.code with
  |(Quad_array(_, _, ent)::_) ->
    {x with place = Quad_valof(ent)}
  |_ -> x

(* Get Type of a quad_elem_t *)
let get_type = function
  |Quad_none -> TYPE_none
  |Quad_int (_) -> TYPE_int
  |Quad_char(_) -> TYPE_byte
  |Quad_string (str) -> TYPE_array(TYPE_byte, String.length str)
  |Quad_valof (ent) 
  |Quad_entry (ent) -> 
    match ent.entry_info with
    |ENTRY_none -> TYPE_none
    |ENTRY_variable (info) -> extractType info.variable_type
    |ENTRY_parameter (info) -> extractType info.parameter_type
    |ENTRY_function (info) -> extractType info.function_result
    |ENTRY_temporary (info) -> extractType info.temporary_type
  

(* Get Size Description from a quad_elem_t *)
let get_size q = 
  match (get_type q) with
  | TYPE_byte -> "byte"
  | _ -> "word"

(* Extract the Entry from a quad_elem_t *)
let extract_entry = function
  |Quad_entry (ent) -> ent
  |Quad_valof (ent) -> ent
  |_ -> internal "Not an entry"; raise Terminate

(* Get a string description of a quad_elem_t *)
let get_id = function
  |Quad_none -> internal "proc func call"; raise Terminate
  |Quad_int (i) -> i
  |Quad_char (c) -> c
  |Quad_string (s) -> s
  |Quad_valof (ent)
  |Quad_entry (ent) -> id_name ent.entry_id

(* Main function to convert a quad to a string *)
let string_of_quad_t = function
  |Quad_unit(ent) -> 
    Printf.sprintf "unit, %s, -, -"
    (id_name ent.entry_id)
  |Quad_endu(ent) -> 
    Printf.sprintf "endu, %s, -, -" 
    (id_name ent.entry_id)
  |Quad_calc (op, q1, q2, q) ->
    Printf.sprintf "%s, %s, %s, %s"
      (op)
      (string_of_quad_elem_t q1)
      (string_of_quad_elem_t q2)
      (string_of_quad_elem_t q)
  |Quad_set(q,qr) ->
    Printf.sprintf ":=, %s, -, %s" 
      (string_of_quad_elem_t q)
      (string_of_quad_elem_t qr)
  |Quad_array(q1, q2, e) ->
    Printf.sprintf "array, %s, %s, %s"
      (string_of_quad_elem_t q1)
      (string_of_quad_elem_t q2)
      (id_name e.entry_id)
  |Quad_cond(op, q1, q2, i) ->
    Printf.sprintf "%s, %s, %s, %d"
      (op)
      (string_of_quad_elem_t q1)
      (string_of_quad_elem_t q2)
      !i
  |Quad_jump i  ->
    Printf.sprintf "jump, -, -, %d" !i
  |Quad_tailCall ent ->
    Printf.sprintf "tailRecursiveCall, -, -, %s"
      (id_name ent.entry_id)
  |Quad_call (ent,_) ->
    Printf.sprintf "call, -, -, %s"
      (id_name ent.entry_id)
  |Quad_par(q,pm) ->
    Printf.sprintf "par, %s, %s, -"
      (string_of_quad_elem_t q)
      (string_of_pass_mode pm)
  |Quad_ret -> "ret, -, -, -" 
  |Quad_dummy -> ""

(* ----------------------------------------------------------------------------- *)

(* Functions to generate intermediate code in the parser *)

(* IMPORTANT: Intermediate code in the lists must be inverted *)

(* Handle an arithmetical expression 
 * Get the 2 types, semantically check them and create the intermediate code 
 * required *)
let handle_expression op e1 e2 (sp,ep) =
  let t1 = get_type e1.place in
  let t2 = get_type e2.place in
  if (check_types op t1 t2 sp ep)
  then let temp = newTemporary t1 in {
    code  = Quad_calc(op,e1.place, e2.place, Quad_entry(temp))
            ::(e2.code)@(e1.code);
    place = Quad_entry(temp);
  }
  else return_null ()

(* Handle signs in expression *)
let handle_unary_expression op exp pos =
  let t = get_type exp.place in
  if (t==TYPE_int) 
  then match op with
    |"+" -> 
      exp
    |"-" -> 
      let temp = newTemporary TYPE_int in
      let new_quad = Quad_calc("-",Quad_int("0"), exp.place, Quad_entry(temp)) in
        { code = (new_quad :: exp.code); place = Quad_entry(temp) }
    |_ -> internal "wrong unary expression"; raise Terminate
  else (
    print_unary_type_error op t pos;
    return_null ()
  )

(* Handle L-Values *)

(* Non-array l-value needs no code *)
let handle_simple_lvalue id pos =
  let (ent, _, correct) = check_lvalue id pos false in
  if (correct) 
    then {code = []; place = Quad_entry(ent)}
  else return_null ()

(* Handle an array lvalue 
 * Array lvalue needs to be dereferenced *)
let handle_array_lvalue id pos context q_t =
  let t = get_type q_t.place in
  if (t==TYPE_int) 
    then let (ent, l_typ, correct) = check_lvalue id pos true in
    if (correct) 
      (* The new temporary created is a Pointer to l_typ *)
      then let temp = newTemporary (TYPE_pointer l_typ)  in
      let new_quad =
         Quad_array(Quad_entry(ent), q_t.place, temp) in
      {code = new_quad::q_t.code ; place = Quad_entry(temp)}
    else return_null ()
  else let sp = fst context and ep = snd context in  
    error "Array index must be an integer in expression starting \
      at line %d, position %d and ending at line %d, position %d."
    (sp.pos_lnum) (sp.pos_cnum - sp.pos_bol)
    (ep.pos_lnum) (ep.pos_cnum - ep.pos_bol);
    return_null ()

(* Ugliest function yet - Handle function calls *)
let handle_func_call id pos expr_list =

  (* Get function entry from id *)
  let ent = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
  
  (* Unzip expression list 
   * Takes expression list - reverse order 
   * Returns a triplet : code, place and types, correct order *)
  let rec unzip_expr_list code_acc place_acc type_acc = function
    | [] -> 
        (code_acc, place_acc, type_acc)
    | (h::t) ->
        unzip_expr_list (h.code :: code_acc) (h.place::place_acc)
          ((get_type h.place)::type_acc) t in

  (* Create Par quads 
   * Takes function information and parameter list 
   * Returns a list of Par Quads - normal *)
  let rec create_par_quads acc = function
    | (_,[]) ->
      List.rev acc
    | (hfi::tfi, hp::tp) -> 
      begin
        match hfi.entry_info with
        | ENTRY_parameter (par_info) ->
          let new_quad = Quad_par (hp, par_info.parameter_mode) in
          if par_info.parameter_mode = PASS_BY_REFERENCE 
          then check_param_by_reference hp id;
          create_par_quads (new_quad::acc) (tfi, tp)
        | _ -> 
          internal "Function parameter not a parameter"; 
          raise Terminate
      end
    | _ -> 
      internal "Less args in create_par_quads"; 
      raise Terminate in

  (* Reverse the order of the code_list and add the par_quads *)
  let rec reverse_code_list acc = function
    | ([], []) -> acc
    | ((h::t), (hp::tp)) -> reverse_code_list (hp::h@acc) (t,tp) 
    | _ -> internal "Uneven args and code"; raise Terminate in

  
  (* Extract expr_list information *)        
  let (code_list, param_list, type_list) = 
    unzip_expr_list [] [] []  expr_list in

  
  match ent.entry_info with
  |ENTRY_function (info) ->
    (* Check for semantic correctness *)
    if (check_func_call info id type_list pos)
    then (
  
      (* Generate par_quads *)
      let par_code = create_par_quads [] 
        (info.function_paramlist, param_list) in 
  
      let entire_code = reverse_code_list [] (code_list, par_code) in

      (* Create code based on function result *)
      match (info.function_result) with
      | TYPE_proc ->
        {
          code = Quad_call(ent,param_list)::entire_code;
          place = Quad_none
        }
      | TYPE_int
      | TYPE_byte -> 
        let temp = newTemporary info.function_result in 
        let ret_place = Quad_entry temp in
        let par_q = Quad_par ( ret_place , PASS_RET) in {
          code = Quad_call(ent,(param_list@[ret_place]))::par_q::entire_code;
          place = Quad_entry(temp)
        }
      | _ -> return_null ()         
      )
    else 
      return_null ()
  |_ ->   
    error "Invalid Function call. Identifier %s is not a function \
      at line %d, position %d."
      id (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
    return_null ()
  
(* Handle Comparisons *)
let handle_comparison op e1 e2 (sp,ep) =
  (* First Check the types of the compared things *)
  let t1 = get_type e1.place in
  let t2 = get_type e2.place in
  if (check_types op t1 t2 sp ep) 
  then
    (* Invariant for Jumps :
     * Everything points to the beginning of the next block
     * with a relative offset. Backpatching is done additively *)
    let true_ref = ref 2 in 
    let false_ref = ref 1 in
    let code_true = (Quad_cond(op, e1.place, e2.place, true_ref))
    in let code_false = (Quad_jump (false_ref)) in {
      c_code = code_false::code_true::e2.code@e1.code;
      q_true = [true_ref];
      q_false = [false_ref];
    }
  else (
    internal "Can't recover from condition error";
    raise Terminate
  )
  
(* Handle boolean values 
 * Constant values means no jump in the "opposite" direction 
 * Extraneous code can be eliminated with dead code elimination optimization *)
let handle_cond_const is_true = 
  let x = ref 1 in {
    c_code = [Quad_jump(x)];
    q_true = if (is_true) then [x] else [];
    q_false = if (is_true) then [] else [x];
  }

(* Handle an "and" cond *)
let handle_and c1 c2 =
  (* The "next" quad will be left unchanged for c2 but c1 will point |c2.code| 
   * later. For immediate evaluation when c1 is false we need to go the end 
   * of everything, when c1 is true we need to evaluate c2. *)
  let len = List.length c2.c_code in
  List.iter (fun x -> x := !x + len) c1.q_false;
  { 
    c_code = c2.c_code @ c1.c_code;
    q_true = c2.q_true;
    q_false = c1.q_false @ c2.q_false;
  }

(* Handle an "or" cond *)
let handle_or c1 c2 = 
  (* Similarly, add |c2.code| to the relative jumps in c1 but now the "true" 
   * condition is the one that can "short-circuit" *)
  let len = List.length c2.c_code in
  List.iter (fun x -> x := !x + len) c1.q_true;
  {
    c_code = c2.c_code @ c1.c_code;
    q_true = c1.q_true @ c2.q_true;
    q_false = c2.q_false;
  }

(* Handle assignmenet *)
let handle_assignment lval expr (sp,ep) =
  let t1 = get_type lval.place in
  let t2 = get_type expr.place in
  if (check_types "=" t1 t2 sp ep) 
  then 
    let new_quad = 
      match lval.place with
      |Quad_valof (_)
      |Quad_entry (_) -> (Quad_set(expr.place,lval.place))    
      |_ -> internal "Assigning to something not an entry";
        raise Terminate
    in new_quad::lval.code@expr.code
  else []

(* Handle if statement *)
let handle_if_stmt cond stmt =
  (* An if statement (without an else) is executed when true. Therefore only the
   * "false" relative jumps are increased by the length of the statement *)
  let len = List.length stmt in
  List.iter (fun x -> x := !x + len) cond.q_false;
  stmt @ cond.c_code

(* Handle if-else statement *)
(* The true condition is executed directly, and then a jump is added to the end
 * of the entire code (including the else-part). The false-refs are increased by 
 * the if-part + 1 (the new jump quad) *)
let handle_if_else_stmt cond s1 s2 =
  let l1 = List.length s1 in
  let l2 = List.length s2 in
  let new_quad = Quad_jump (ref (l2+1)) in
  List.iter (fun x -> x := !x + l1 + 1) cond.q_false;
  s2 @ (new_quad::(s1 @ cond.c_code))

(* Handle while statement *)
(* The "false" jumps after all the statements plus the jump to the top. The jump to
 * the top must account for the re-evaluation of the condition *)
let handle_while_stmt cond stmt = 
  let l = List.length stmt in
  let lc = List.length cond.c_code in
  List.iter (fun x -> x := !x + l + 1) cond.q_false;
  let new_quad = Quad_jump (ref (-l-lc)) in
  new_quad :: (stmt @ cond.c_code)

(* Handle a return expression *)
(* After semantically checking the return types, and set to "$$" - the extra 
 * parameter by reference and then return (Quad_ret) *)
let handle_return_expr expr pos=
  let t = get_type expr.place in
  if (equalType t !currentScope.sco_ret_type) 
  then let ret_entry = lookupEntry (id_make "$$") LOOKUP_CURRENT_SCOPE true
    in Quad_ret ::(Quad_set(expr.place, Quad_entry(ret_entry))):: expr.code
  else (
    error "Attempting to return %s when %s was expected, \
      in line %d, position %d" 
      (string_of_typ t) (string_of_typ !currentScope.sco_ret_type)
      (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
    []
  )

(* Proc return *)
(* Make sure nothing should be returned and return *)
let handle_return_proc pos =
  if (equalType TYPE_proc !currentScope.sco_ret_type)
  then
    [Quad_ret]
  else (
    error "Attemping to return proc when %s was expected, \
      in line %d, position %d"
      (string_of_typ !currentScope.sco_ret_type)
      (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol);
    []
  )

(* Function definitions *)
(* Wrap the body around unit-endu and add the local definitions at the beginning *)
let handle_func_def id local_def stmt =
  let ent = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
  let s_quad = Quad_unit(ent) in
  let e_quad = Quad_endu(ent) in
  e_quad :: (stmt @ (s_quad :: local_def))

