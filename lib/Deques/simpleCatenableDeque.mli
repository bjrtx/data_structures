module type QS = sig
  type 'a t

  val empty : 'a t
  val size : 'a t -> int
  val is_empty : 'a t -> bool
  val cons : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a option
  val tail : 'a t -> 'a t option
  val snoc : 'a -> 'a t -> 'a t
  val last : 'a t -> 'a option
  val init : 'a t -> 'a t option
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
  val append : 'a t -> 'a t -> 'a t
end
