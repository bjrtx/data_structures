(** List implementation with constant-time concatenation. *)

type !'a t

val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val empty : 'a t
val append : 'a t -> 'a t -> 'a t
val cons : 'a -> 'a t -> 'a t
val push : 'a -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val snoc : 'a t -> 'a -> 'a t
val concat : 'a t list -> 'a t
val size : 'a t -> int
val mem : 'a -> 'a t -> bool
val to_seq : 'a t -> 'a Seq.t
val to_arbitrary_seq : 'a t -> 'a Seq.t
val is_empty : 'a t -> bool
val backspace : 'a t
