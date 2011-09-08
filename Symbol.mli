(* Symbol table *)

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE | PASS_RET

type global_status = GLOBAL_DEFINED | GLOBAL_USED

type param_status =
  | PARDEF_COMPLETE                             (* Πλήρης ορισμός     *)
  | PARDEF_DEFINE                               (* Εν μέσω ορισμού    *)
  | PARDEF_CHECK                                (* Εν μέσω ελέγχου    *)

type scope = {
  sco_parent : scope option;
  mutable sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_negofs : int;
  sco_ret_type : Types.typ;
}

and variable_info = {                         (******* Μεταβλητή *******)
  variable_type   : Types.typ;                (* Τύπος                 *)
  variable_offset : int                       (* Offset στο Ε.Δ.       *)
}

and function_info = {                         (******* Συνάρτηση *******)
  mutable function_isForward  : bool;         (* Δήλωση forward        *)
  mutable function_paramlist  : entry list;   (* Λίστα παραμέτρων      *)
  mutable function_redeflist  : entry list;   (* Λίστα παραμέτρων (2η) *)
  mutable function_result     : Types.typ;    (* Τύπος αποτελέσματος   *)
  mutable function_pstatus    : param_status; (* Κατάσταση παραμέτρων  *)
  mutable function_initquad   : int;          (* Αρχική τετράδα        *)
  mutable function_negoffs 	  : int;			    (* Negative offsets 	   *)
  function_isLibrary          : bool;         (* If it is a Lib func   *)
  function_global             : (entry, global_status) Hashtbl.t 
                                              (* Global Definitions    *)
}

and parameter_info = {                        (****** Παράμετρος *******)
  parameter_type           : Types.typ;       (* Τύπος                 *)
  mutable parameter_offset : int;             (* Offset στο Ε.Δ.       *)
  parameter_mode           : pass_mode        (* Τρόπος περάσματος     *)
}

and temporary_info = {                        (** Προσωρινή μεταβλητή **)
  temporary_type   : Types.typ;               (* Τύπος                 *)
  temporary_offset : int                      (* Offset στο Ε.Δ.       *)
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

val currentScope : scope ref              (* Τρέχουσα εμβέλεια         *)
val quadNext : int ref                    (* Αριθμός επόμενης τετράδας *)
val tempNumber : int ref                  (* Αρίθμηση των temporaries  *)

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

val start_positive_offset : int   (* Αρχικό θετικό offset στο Ε.Δ.   *)
val start_negative_offset : int   (* Αρχικό αρνητικό offset στο Ε.Δ. *)

val equalEntries : entry -> entry -> bool
val isLibraryFunction : entry -> bool
