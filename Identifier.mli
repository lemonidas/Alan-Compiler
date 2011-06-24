(* Hash consed identifiers *)

type id

val id_make  : string -> id
val id_name  : id -> string
val pretty_id : Format.formatter -> id -> unit
