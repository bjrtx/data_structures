type 'a t

val size : 'a t -> int
val to_stream : 'a t -> 'a Stream.t
val empty : 'a t
val is_empty : 'a t -> bool
val cons : 'a -> 'a t -> 'a t
val append : 'a t -> 'a t -> 'a t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val ( @ ) : 'a t -> 'a t -> 'a t
val take : int -> 'a t -> 'a t
val drop : int -> 'a t -> 'a t
val reverse : 'a t -> 'a t
val to_list : 'a t -> 'a list
val to_seq : 'a t -> 'a Seq.t
val map : ('a -> 'b) -> 'a t -> 'b t
val peek : 'a t -> 'a option
val tail : 'a t -> 'a t option
