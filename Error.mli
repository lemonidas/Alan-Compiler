(* Error handling *)

exception Terminate

exception CustomError of string

type verbose = Vquiet | Vnormal | Vverbose

type err_type = Fatal | Error | Warning | Internal

val flagVerbose      : verbose ref

val numErrors        : int ref
val maxErrors        : int ref
val flagWarnings     : bool ref
val numWarnings      : int ref
val maxWarnings      : int ref

val internal_raw     : (string * int) ->
                         ('a, Format.formatter, unit) format -> 'a
val fatal            : ('a, Format.formatter, unit) format -> 'a
val error            : ('a, Format.formatter, unit) format -> 'a
val warning          : ('a, Format.formatter, unit) format -> 'a
val message          : ('a, Format.formatter, unit) format -> 'a

type position

val position_point   : Lexing.position -> position
val position_context : Lexing.position -> Lexing.position -> position
val position_dummy   : position
val print_position   : Format.formatter -> position -> unit

val get_expression : Lexing.position -> Lexing.position -> string -> string
