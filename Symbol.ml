open Identifier
open Error
open Types

module H = Hashtbl.Make (
  struct
    type t = id
    let equal = (==)
    let hash = Hashtbl.hash
  end
)

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE | PASS_RET

type param_status =
  | PARDEF_COMPLETE
  | PARDEF_DEFINE
  | PARDEF_CHECK

type scope = {
  sco_parent : scope option;
  mutable sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_negofs : int;
  sco_ret_type : Types.typ;
}

and variable_info = {
  variable_type   : Types.typ;
  variable_offset : int
}

and function_info = {
  mutable function_isForward : bool;
  mutable function_paramlist : entry list;
  mutable function_redeflist : entry list;
  mutable function_result    : Types.typ;
  mutable function_pstatus   : param_status;
  mutable function_initquad  : int;
  mutable function_negoffs	 : int;
  function_isLibrary : bool
}

and parameter_info = {
  parameter_type           : Types.typ;
  mutable parameter_offset : int;
  parameter_mode           : pass_mode
}

and temporary_info = {
  temporary_type   : Types.typ;
  temporary_offset : int
}

and entry_info = ENTRY_none
               | ENTRY_variable of variable_info
               | ENTRY_function of function_info
               | ENTRY_parameter of parameter_info
               | ENTRY_temporary of temporary_info

and entry = {
  entry_id    : Identifier.id;
  entry_scope : scope;
  entry_info  : entry_info
}

type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES

let start_positive_offset = 8
let start_negative_offset = 0

let the_outer_scope = {
  sco_parent = None;
  sco_nesting = 0;
  sco_entries = [];
  sco_negofs = start_negative_offset;
  sco_ret_type = Types.TYPE_proc;
}

let no_entry id = {
  entry_id = id;
  entry_scope = the_outer_scope;
  entry_info = ENTRY_none
}

let currentScope = ref the_outer_scope
let quadNext = ref 1
let tempNumber = ref 1

let tab = ref (H.create 0)

let initSymbolTable size =
   tab := H.create size;
   currentScope := the_outer_scope

let openScope typ =
  let sco = {
    sco_parent = Some !currentScope;
    sco_nesting = if !currentScope.sco_nesting = max_int 
                  then 2
                  else !currentScope.sco_nesting + 1;
    sco_entries = [];
    sco_negofs = start_negative_offset;
	  sco_ret_type = typ;
  } in
  currentScope := sco;
  Printf.printf "opening new scope with nesting: %d\n" (!currentScope.sco_nesting)

let closeScope f =
  let sco = !currentScope in 
  begin
	match f.entry_info with
  	| ENTRY_function(info) ->
		  info.function_negoffs <- sco.sco_negofs;
  	| _ -> internal "Function not a function!"
  end;
  let manyentry e = H.remove !tab e.entry_id in
  List.iter manyentry sco.sco_entries;
  match sco.sco_parent with
  | Some scp ->
      currentScope := scp
  | None ->
      internal "cannot close the outer scope!"

exception Failure_NewEntry of entry

let newEntry id inf err =
  try
    if err then begin
      try
        let e = H.find !tab id in
        if e.entry_scope.sco_nesting = !currentScope.sco_nesting then
           raise (Failure_NewEntry e)
      with Not_found ->
        ()
    end;
    let e = {
      entry_id = id;
      entry_scope = !currentScope;
      entry_info = inf
    } in
    H.add !tab id e;
    !currentScope.sco_entries <- e :: !currentScope.sco_entries;
    e
  with Failure_NewEntry e ->
    error "duplicate identifier %a" pretty_id id;
    e

let lookupEntry id how err =
  let scc = !currentScope in
  let lookup () =
    match how with
    | LOOKUP_CURRENT_SCOPE ->
        let e = H.find !tab id in
        if e.entry_scope.sco_nesting = scc.sco_nesting then
          e
        else
          raise Not_found
    | LOOKUP_ALL_SCOPES ->
        H.find !tab id in
  if err then
    try
      lookup ()
    with Not_found ->
      error "unknown identifier %a (first occurrence)"
        pretty_id id;
      (* put it in, so we don't see more errors *)
      H.add !tab id (no_entry id);
      raise Exit
  else
    lookup ()

let newVariable id typ err =
  !currentScope.sco_negofs <- !currentScope.sco_negofs - sizeOfType typ;
  let inf = {
    variable_type = typ;
    variable_offset = !currentScope.sco_negofs
  } in
  newEntry id (ENTRY_variable inf) err

let newFunction id err lib =
  try
    let e = lookupEntry id LOOKUP_CURRENT_SCOPE false in
    match e.entry_info with
    | ENTRY_function inf when inf.function_isForward ->
        inf.function_isForward <- false;
        inf.function_pstatus <- PARDEF_CHECK;
        inf.function_redeflist <- inf.function_paramlist;
        e
    | _ ->
        if err then
          error "duplicate identifier: %a" pretty_id id;
          raise Exit
  with Not_found ->
    let inf = {
      function_isForward = false;
      function_paramlist = [];
      function_redeflist = [];
      function_result = TYPE_none;
      function_pstatus = PARDEF_DEFINE;
      function_initquad = 0;
  	  function_negoffs = 0;
      function_isLibrary = lib;
    } in
    newEntry id (ENTRY_function inf) false

let newParameter id typ mode f err =
  match f.entry_info with
  | ENTRY_function inf -> begin
      match inf.function_pstatus with
      | PARDEF_DEFINE ->
          let inf_p = {
            parameter_type = typ;
            parameter_offset = 0;
            parameter_mode = mode
          } in
          let e = newEntry id (ENTRY_parameter inf_p) err in
          inf.function_paramlist <- e :: inf.function_paramlist;
          e
      | PARDEF_CHECK -> begin
          match inf.function_redeflist with
          | p :: ps -> begin
              inf.function_redeflist <- ps;
              match p.entry_info with
              | ENTRY_parameter inf ->
                  if not (equalType inf.parameter_type typ) then
                    error "Parameter type mismatch in redeclaration \
                           of function %a" pretty_id f.entry_id
                  else if inf.parameter_mode != mode then
                    error "Parameter passing mode mismatch in redeclaration \
                           of function %a" pretty_id f.entry_id
                  else if p.entry_id != id then
                    error "Parameter name mismatch in redeclaration \
                           of function %a" pretty_id f.entry_id
                  else
                    H.add !tab id p;
                  p
              | _ ->
                  internal "I found a parameter that is not a parameter!";
                  raise Exit
            end
          | [] ->
              error "More parameters than expected in redeclaration \
                     of function %a" pretty_id f.entry_id;
              raise Exit
        end
      | PARDEF_COMPLETE ->
          internal "Cannot add a parameter to an already defined function";
          raise Exit
    end
  | _ ->
      internal "Cannot add a parameter to a non-function";
      raise Exit

let newTemporary typ =
  let id = id_make ("$" ^ string_of_int !tempNumber) in
  !currentScope.sco_negofs <- !currentScope.sco_negofs - sizeOfType typ;
  let inf = {
    temporary_type = typ;
    temporary_offset = !currentScope.sco_negofs
  } in
  incr tempNumber;
  newEntry id (ENTRY_temporary inf) false

let forwardFunction e =
  match e.entry_info with
  | ENTRY_function inf ->
      inf.function_isForward <- true
  | _ ->
      internal "Cannot make a non-function forward"

let endFunctionHeader e typ =
  match e.entry_info with
  | ENTRY_function inf ->
      begin
        match inf.function_pstatus with
        | PARDEF_COMPLETE ->
            internal "Cannot end parameters in an already defined function"
        | PARDEF_DEFINE ->
            inf.function_result <- typ;
            let offset = 
              match typ with 
  			    	| TYPE_proc -> ref start_positive_offset
	  			    | TYPE_byte
		  		    | TYPE_int -> 
			  		    ignore(newParameter (id_make "$$") 
				  				     typ PASS_BY_REFERENCE e true);
					      ref (start_positive_offset - 2);
				      | _ -> 
                internal "Return type must be int or byte";
	  				    raise Terminate
			      in 
            let fix_offset e =
              match e.entry_info with
              | ENTRY_parameter inf ->
                inf.parameter_offset <- !offset;
                let size =
                  match inf.parameter_mode with
                  | PASS_BY_VALUE     -> sizeOfType inf.parameter_type
                  | PASS_BY_REFERENCE -> 2
                  | PASS_RET -> (
                		internal "Parameter cannot be return parameter";
			      		   	raise Terminate
                  	) 
                in
                offset := !offset + size
              | _ ->
                internal "Cannot fix offset to a non parameter" 
            in
            List.iter fix_offset inf.function_paramlist;
            inf.function_paramlist <- List.rev inf.function_paramlist
        | PARDEF_CHECK ->
            if inf.function_redeflist <> [] then
              error "Fewer parameters than expected in redeclaration \
                     of function %a" pretty_id e.entry_id;
            if not (equalType inf.function_result typ) then
              error "Result type mismatch in redeclaration of function %a"
                    pretty_id e.entry_id;
      end;
      inf.function_pstatus <- PARDEF_COMPLETE
  | _ ->
      internal "Cannot end parameters in a non-function"

let equalEntries e1 e2 =
  e1.entry_scope.sco_nesting = e2.entry_scope.sco_nesting &&
  e1.entry_id = e2.entry_id

let isLibraryFunction ent =
  match ent.entry_info with
  | ENTRY_function fun_info -> fun_info.function_isLibrary
  | _ -> false
