(** Lists with constant-time size computation. *)

type !+'a t

val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list

val empty : 'a t
(** The empty list. *)

val append : 'a t -> 'a t -> 'a t
val rev : 'a t -> 'a t
val cons : 'a -> 'a t -> 'a t
val tail : 'a t -> 'a t option
val push : 'a -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val concat : 'a t list -> 'a t
val size : 'a t -> int
val mem : 'a -> 'a t -> bool
val to_seq : 'a t -> 'a Seq.t
val to_arbitrary_seq : 'a t -> 'a Seq.t
val is_empty : _ t -> bool
