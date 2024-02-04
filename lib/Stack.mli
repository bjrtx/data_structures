(** Functional FIFO stacks. *)

type 'a t
(** The type of stacks with elements of type ['a]. *)

val empty : 'a t
(** The empty stack. *)
  
val to_list : 'a t -> 'a list
val push : 'a -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
(** [to_seq (map f stack) = Seq.map f (to_seq stack)] *)

val pop : 'a t -> 'a t option
val peek : 'a t -> 'a option
val is_empty : 'a t -> bool
val size : 'a t -> int
val mem : 'a -> 'a t -> bool
val to_seq : 'a t -> 'a Stdlib.Seq.t
val to_arbitrary_seq : 'a t -> 'a Stdlib.Seq.t
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
