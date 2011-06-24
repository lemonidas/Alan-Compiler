type 'a hash_consed = {
  node : 'a;
  tag  : int
}

let hash_value x = x.node
let hash_tag x = x.tag

module type Comp = 
  sig
    type t
    val equal : t -> t -> bool
    val hash  : t -> int
  end

module type S =
  sig
    type t
    val  f : unit -> (t -> t hash_consed)
  end

module Make (X : Comp) : (S with type t = X.t) = 
  struct
    type t = X.t
    type h_table = t hash_consed list array
    let create n = Array.create n [] 
    let f () =
      let gen_tag = ref 0 in
      let table = create 397 in
      function x ->
        let i = (X.hash x) mod (Array.length table) in
        let rec look_and_add =
          function
          | [] ->
              let hv = {tag = !gen_tag; node = x} in
              table.(i) <- hv :: table.(i);
              incr gen_tag;
              hv
          | hd :: tl ->
              if X.equal hd.node x then
                hd
              else
                look_and_add tl in
        look_and_add table.(i)
  end

let hashcons_resets = ref []
let init () = List.iter (fun f -> f()) !hashcons_resets

let register_hcons h u =
  let hf = ref (h u) in
  let reset () = hf := h u in
  hashcons_resets := reset :: !hashcons_resets;
  (function x -> !hf x)
