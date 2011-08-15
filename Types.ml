type typ = TYPE_none
         | TYPE_int
         | TYPE_byte
         | TYPE_array of
             typ *
             int
         | TYPE_proc
         | TYPE_pointer of typ

let rec sizeOfType t =
   match t with
   | TYPE_int            -> 2
   | TYPE_byte           -> 1
   | TYPE_array (et, sz) -> sz * sizeOfType et
   | TYPE_pointer (_)    -> 2
   | _                   -> 0

let rec equalType t1 t2 =
   match t1, t2 with
   | TYPE_array (et1, _), TYPE_array (et2, _) -> equalType et1 et2
   | TYPE_pointer e1 , TYPE_pointer e2        -> equalType e1 e2
   | _                                        -> t1 = t2

let rec extractType = function
  | TYPE_pointer typ -> typ
  | x -> x

let sizeOfArrayElem t =
	match t with
	| TYPE_int -> 2
	| TYPE_byte -> 1
	| TYPE_array (et,_) -> sizeOfType et
  | TYPE_pointer e -> sizeOfType e
	| _ -> 0
