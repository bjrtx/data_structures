(** Banker's deques are parameterized by a constant [c] which must be larger than 1. It controls the maximum permitted imbalance. *)

module Make (_ : sig
  val c : int
end) : sig
  val c : int

  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val queue : 'a t -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a option
  val tail : 'a t -> 'a t option
  val snoc : 'a -> 'a t -> 'a t
  val last : 'a t -> 'a option
  val init : 'a t -> 'a t option
end
