(** Lazy streams. *)

type 'a cell = Nil | Cons of 'a * 'a t and 'a t = 'a cell Lazy.t

val append : 'a t -> 'a t -> 'a t
val take : int -> 'a t -> 'a t
val drop : int -> 'a t -> 'a t
val reverse : 'a t -> 'a t
val empty :'a t  
val to_list : 'a t -> 'a list
(*

val push : 'a -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
(** [to_seq (map f stack) = Seq.map f (to_seq stack)] *)

val pop : 'a t -> 'a t option
val peek : 'a t -> 'a option
val is_empty : 'a t -> bool


 *)
