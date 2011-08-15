open FinalTypes

let rec handle_load_store code acc =
  match code with 
  | [] -> List.rev acc
  | (Mov(m1,m2)::Mov(m3,m4)::t) when m1 = m4 && m2 = m3 ->
    handle_load_store (Mov(m1,m2)::t) acc
  | h::t -> handle_load_store t (h::acc)

let optimize low_level_code = 
  handle_load_store low_level_code []
