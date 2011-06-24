(* Polymorphic hash consing,
   you don't need to understand what this is all about... *)

type 'a hash_consed

val hash_value : 'a hash_consed -> 'a
val hash_tag : 'a hash_consed -> int

module type Comp =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
  end

module type S =
  sig
    type t
    val f : unit -> t -> t hash_consed
  end

module Make (X : Comp) : (S with type t = X.t)

val init : unit -> unit
val register_hcons : (unit -> 'a -> 'a hash_consed) ->
                     (unit -> 'a -> 'a hash_consed)
