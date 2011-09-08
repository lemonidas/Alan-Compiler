(* Symbol table *)

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE | PASS_RET

type global_status = GLOBAL_DEFINED | GLOBAL_USED

type param_status =
  | PARDEF_COMPLETE                             (* ������ �������     *)
  | PARDEF_DEFINE                               (* �� ���� �������    *)
  | PARDEF_CHECK                                (* �� ���� �������    *)

type scope = {
  sco_parent : scope option;
  mutable sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_negofs : int;
  sco_ret_type : Types.typ;
}

and variable_info = {                         (******* ��������� *******)
  variable_type   : Types.typ;                (* �����                 *)
  variable_offset : int                       (* Offset ��� �.�.       *)
}

and function_info = {                         (******* ��������� *******)
  mutable function_isForward  : bool;         (* ������ forward        *)
  mutable function_paramlist  : entry list;   (* ����� ����������      *)
  mutable function_redeflist  : entry list;   (* ����� ���������� (2�) *)
  mutable function_result     : Types.typ;    (* ����� �������������   *)
  mutable function_pstatus    : param_status; (* ��������� ����������  *)
  mutable function_initquad   : int;          (* ������ �������        *)
  mutable function_negoffs 	  : int;			    (* Negative offsets 	   *)
  function_isLibrary          : bool;         (* If it is a Lib func   *)
  function_global             : (entry, global_status) Hashtbl.t 
                                              (* Global Definitions    *)
}

and parameter_info = {                        (****** ���������� *******)
  parameter_type           : Types.typ;       (* �����                 *)
  mutable parameter_offset : int;             (* Offset ��� �.�.       *)
  parameter_mode           : pass_mode        (* ������ ����������     *)
}

and temporary_info = {                        (** ��������� ��������� **)
  temporary_type   : Types.typ;               (* �����                 *)
  temporary_offset : int                      (* Offset ��� �.�.       *)
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

val currentScope : scope ref              (* �������� ��������         *)
val quadNext : int ref                    (* ������� �������� �������� *)
val tempNumber : int ref                  (* �������� ��� temporaries  *)

val initSymbolTable  : int -> unit
val openScope        : Types.typ -> unit
val closeScope       : entry -> unit
val newVariable      : Identifier.id -> Types.typ -> bool -> entry
val newFunction      : Identifier.id -> bool -> bool -> entry
val newParameter     : Identifier.id -> Types.typ -> pass_mode ->
                                        entry -> bool -> entry
val newTemporary     : Types.typ -> entry

val forwardFunction   : entry -> unit
val endFunctionHeader : entry -> Types.typ -> unit
val lookupEntry       : Identifier.id -> lookup_type -> bool -> entry

val start_positive_offset : int   (* ������ ������ offset ��� �.�.   *)
val start_negative_offset : int   (* ������ �������� offset ��� �.�. *)

val equalEntries : entry -> entry -> bool
val isLibraryFunction : entry -> bool
