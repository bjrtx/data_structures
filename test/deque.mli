module type Deque = sig
  type 'a t

  val empty : 'weak1376 t
  val is_empty : 'a t -> bool
  val cons : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a option
  val tail : 'a t -> 'a t option
  val snoc : 'a -> 'a t -> 'a t
  val last : 'a t -> 'a option
  val init : 'weak6 t -> 'weak6 t option
end
