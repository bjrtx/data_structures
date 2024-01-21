(** Purely functional implementations of binary search trees. *)


(** Input signature of the various heap-building functors. *)
module type OrderedType = sig
  type t
  (** The type of heap elements. *)
     
  val compare: t -> t -> int
                           (** [compare] must describe a total ordering on values of type [t] by having [compare x y] be positive, negative or zero as [x] is greater, lesser or equal to [y]. For example, [Int.compare] is a suitable comparison function on [int]. *)
end


(** Output signature of the various BST-building functors. *)
module type BST = sig

  
  (** The type of elements. *)
  type elt

  type t

  (** Number of elements in the BST. *)
  val size : t -> int

  (** Create an empty priority BST. *)
  val empty : t

  (** Linear-time traversal in arbitrary order. *)
  val to_arbitrary_seq : t -> elt Seq.t

  (** Insert element. *)
  val push : elt -> t -> t
    
  (** Initialize from a list. *)
  val of_list : elt list -> t
    
    
  (** Apply a function to all elements and return the resulting tree. *)
  val map: (elt -> elt) -> t -> t
  
  (** Traversal in the order given by [Ord.compare]. Possibly inefficient. *)
  val to_ordered_seq : t -> elt Seq.t

  (** Membership query. *)
  val mem : elt -> t -> bool

  (** Using a BST to sort a list. *)
  val sort : elt list -> elt list
  
  (** Return [true] if the BST is empty else [false] *)
  val is_empty : t -> bool
    
end

module DefaultBST(Ord : OrderedType) : BST with type elt = Ord.t
