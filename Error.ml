open Format
open Lexing

exception Terminate

type verbose = Vquiet | Vnormal | Vverbose

type err_type = Fatal | Error | Warning | Internal

exception CustomError of string;;

let flagVerbose = ref Vnormal

let numErrors = ref 0
let maxErrors = ref 10
let flagWarnings = ref true
let numWarnings = ref 0
let maxWarnings = ref 200

type position =
    PosPoint   of Lexing.position
  | PosContext of Lexing.position * Lexing.position
  | PosDummy

let position_point lpos = PosPoint lpos
let position_context lpos_start lpos_end = PosContext (lpos_start, lpos_end)
let position_dummy = PosDummy

let print_position ppf pos =
  match pos with
  | PosPoint lpos ->
      fprintf ppf "@[file \"%s\",@ line %d,@ character %d:@]@ "
        lpos.pos_fname lpos.pos_lnum (lpos.pos_cnum - lpos.pos_bol)
  | PosContext (lpos_start, lpos_end) ->
      if lpos_start.pos_fname != lpos_end.pos_fname then
        fprintf ppf "@[file \"%s\",@ line %d,@ character %d to@ \
                     file %s,@ line %d,@ character %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
          lpos_end.pos_fname lpos_end.pos_lnum
          (lpos_end.pos_cnum - lpos_end.pos_bol)
      else if lpos_start.pos_lnum != lpos_end.pos_lnum then
        fprintf ppf "@[file \"%s\",@ line %d,@ character %d to@ \
                     line %d,@ character %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
          lpos_end.pos_lnum
          (lpos_end.pos_cnum - lpos_end.pos_bol)
      else if lpos_start.pos_cnum - lpos_start.pos_bol !=
              lpos_end.pos_cnum - lpos_end.pos_bol then
        fprintf ppf "@[file \"%s\",@ line %d,@ characters %d to %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
          (lpos_end.pos_cnum - lpos_end.pos_bol)
      else
        fprintf ppf "@[file \"%s\", line %d, character %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
  | PosDummy ->
      ()

let get_expression start_pos end_pos file_name =
  let in_channel = open_in file_name in
  let starting_pos = start_pos.pos_cnum in
  let read_amount = (end_pos.pos_cnum - start_pos.pos_cnum) in
  let expr = String.create read_amount in
  seek_in in_channel starting_pos;
  ignore (input in_channel expr 0 read_amount);
  close_in in_channel;
  expr
;;

let no_out buf pos len = ()
let no_flush () = ()
let null_formatter = make_formatter no_out no_flush

let internal_raw (fname, lnum) fmt =
  let fmt = "@[<v 2>" ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  let cont ppf =
    raise Terminate in
  eprintf "Internal error occurred at %s:%d,@ " fname lnum;
  kfprintf cont err_formatter fmt

and fatal fmt =
  let fmt = "@[<v 2>Fatal error: " ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  let cont ppf =
    raise Terminate in
  kfprintf cont err_formatter fmt

and error fmt =
  let fmt = "@[<v 2>Error: " ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  if !numErrors >= !maxErrors then
    let cont ppf =
      eprintf "Too many errors, aborting...\n";
      raise Terminate in
    kfprintf cont err_formatter fmt
  else
    eprintf fmt

and warning fmt =
  let fmt = "@[<v 2>Warning: " ^^ fmt ^^ "@]@;@?" in
  if !flagWarnings then
  begin
    incr numWarnings;
    if !numWarnings >= !maxWarnings then
      let cont ppf =
        eprintf "Too many warnings, no more will be shown...\n";
        flagWarnings := false in
      kfprintf cont err_formatter fmt
    else
      eprintf fmt
  end
  else
    fprintf null_formatter fmt

and message fmt =
  let fmt = "@[<v 2>" ^^ fmt ^^ "@]@;@?" in
  eprintf fmt
