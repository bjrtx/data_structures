type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

type 'a context =
  | Context of { isRight : bool; parent : 'a; sibling : 'a tree }

type 'a t = 'a tree * 'a context list

val empty : 'a t
val of_tree : 'a tree -> 'a t
val to_tree : 'a t -> 'a tree

val left : 'a t -> 'a t
(** Moves the cursor to the left sibling, if applicable. *)

val right : 'a t -> 'a t
(** Moves the cursor to the right sibling, if applicable. *)

val up : 'a t -> 'a t
(** Moves the cursor to the parent, if applicable. *)

val down_left : 'a t -> 'a t
(** Moves the cursor to the left child, if applicable. *)

val down_right : 'a t -> 'a t
(** Moves the cursor to the right child, if applicable. *)

val change : 'a tree -> 'a t -> 'a t
(** Changes the value at the current cursor position. *)
