(** Lazy streams. *)

type 'a cell = Nil | Cons of 'a * 'a t
and 'a t = 'a cell Lazy.t

val append : 'a t -> 'a t -> 'a t

val ( @ ) : 'a t -> 'a t -> 'a t
(** [s @ t] is [append s t].*)

val take : int -> 'a t -> 'a t
val drop : int -> 'a t -> 'a t
val reverse : 'a t -> 'a t
val empty : 'a t
val is_empty : 'a t -> bool
val to_list : 'a t -> 'a list
val to_seq : 'a t -> 'a Seq.t

val map : ('a -> 'b) -> 'a t -> 'b t
(** [to_seq (map f s) = Seq.map f (to_seq s)] *)

val peek : 'a t -> 'a option
val cons : 'a -> 'a t -> 'a t
(*

val push : 'a -> 'a t -> 'a t


val pop : 'a t -> 'a t option

 *)
