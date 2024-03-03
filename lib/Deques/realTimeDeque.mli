module Make (_ : sig
  val c : int
end) : sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val cons : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a option
  val queue : 'a t -> 'a t
end
