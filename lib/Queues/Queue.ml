(** Common interface for all queues (FIFO structures). *)

module type S = sig
  type 'a t

  val empty : 'a t

  val is_empty : 'a t -> bool
  (** [is_empty q] is [true] if and only if [q] has no elements. *)

  val push : 'a -> 'a t -> 'a t
  (** [push x q] is obtained by the insertion of [x] into [q]. *)

  val peek : 'a t -> 'a option
  (** [peep x q] is either [None] if [q] is empty or [Some x] where [x] is the first-out element of [q]. *)

  val pop : 'a t -> 'a t option
end
