module type S = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a option
  val pop : 'a t -> 'a t option
end

module CatenableList : functor (_ : S) -> sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val append : 'a t -> 'a t -> 'a t
  val ( @ ) : 'a t -> 'a t -> 'a t
  val peek : 'a t -> 'a option
  val pop : 'a t -> 'a t option
end
