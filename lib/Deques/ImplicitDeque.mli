(** Double-ended queues with implicit recursive slowdown after Okasaki.
    

Parameterized by a queue module.
*)

module type QS = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val snoc : 'a -> 'a t -> 'a t
  val last : 'a t -> 'a option
  val poplast : 'a t -> 'a t option
  val peek : 'a t -> 'a option
  val pop : 'a t -> 'a t option
end

module Make : functor (_ : QS) -> sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val cons : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a option
  val tail : 'a t -> 'a t option
  val snoc : 'a -> 'a t -> 'a t
  val last : 'a t -> 'a option
  val init : 'a t -> 'a t option
end
