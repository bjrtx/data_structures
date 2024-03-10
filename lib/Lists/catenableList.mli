(** Catenable lists after Okasaki.
    
Catenable lists are parameterised by a queue data structure. *)

(** Input type of the Make functor: queue interface. *)
module type S = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a option
  val pop : 'a t -> 'a t option
end

(** Functor for building catenable lists from a queue module. *)
module Make : functor (_ : S) -> sig
  type 'a t

  val empty : 'a t

  val is_empty : 'a t -> bool
  (** [is_empty q] is [true] if and only if [q] has no elements. *)

  val append : 'a t -> 'a t -> 'a t
  val ( @ ) : 'a t -> 'a t -> 'a t
  val peek : 'a t -> 'a option
  val pop : 'a t -> 'a t option
  val cons : 'a -> 'a t -> 'a t
  val snoc : 'a -> 'a t -> 'a t
end
