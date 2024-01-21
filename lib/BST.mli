(** Purely functional implementations of binary search trees. *)

(** Input signature of the various heap-building functors. *)
module type OrderedType = sig
  type t
  (** The type of heap elements. *)

  val compare : t -> t -> int
  (** [compare] must describe a total ordering on values of type [t] by having [compare x y] be positive, negative or zero as [x] is greater, lesser or equal to [y]. For example, [Int.compare] is a suitable comparison function on [int]. *)
end

(** Output signature of the various BST-building functors. *)
module type BST = sig
  type elt
  (** The type of elements. *)

  type t

  val size : t -> int
  (** Number of elements in the BST. *)

  val empty : t
  (** Create an empty priority BST. *)

  val to_arbitrary_seq : t -> elt Seq.t
  (** Linear-time traversal in arbitrary order. *)

  val push : elt -> t -> t
  (** Insert element. *)

  val of_list : elt list -> t
  (** Initialize from a list. *)

  val map : (elt -> elt) -> t -> t
  (** Apply a function to all elements and return the resulting tree. *)

  val to_ordered_seq : t -> elt Seq.t
  (** Traversal in the order given by [Ord.compare]. Possibly inefficient. *)

  val mem : elt -> t -> bool
  (** Membership query. *)

  val sort : elt list -> elt list
  (** Using a BST to sort a list. *)

  val is_empty : t -> bool
  (** Return [true] if the BST is empty else [false] *)
end

module DefaultBST (Ord : OrderedType) : BST with type elt = Ord.t
module RBTree (Ord : OrderedType) : BST with type elt = Ord.t
