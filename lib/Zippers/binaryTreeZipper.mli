type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

type 'a context =
  | Context of { isRight : bool; parent : 'a; sibling : 'a tree }

type 'a t = 'a tree * 'a context list

val empty : 'a t
val left : 'a t -> 'a t
val right : 'a t -> 'a t
val up : 'a t -> 'a t
val down_left : 'a t -> 'a t
val down_right : 'a t -> 'a t
val change : 'a tree -> 'a t -> 'a t
