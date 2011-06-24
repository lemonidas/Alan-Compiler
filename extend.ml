open Pcaml
open Lexing

EXTEND
  expr: LEVEL "simple"
    [[ LIDENT "internal" ->
         let file = <:expr< $str:!input_file$ >>
         and line = <:expr< $int:string_of_int (Ploc.line_nb loc)$ >> in
         <:expr< internal_raw ($file$, $line$) >>
    |  UIDENT "Error"; "."; LIDENT "internal" ->
         let file = <:expr< $str:!input_file$ >>
         and line = <:expr< $int:string_of_int (Ploc.line_nb loc)$ >> in
         <:expr< Error.internal_raw ($file$, $line$) >>;
    ]];
END
