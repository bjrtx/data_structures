(** Heap-based purely functional implementations of priority queues. *)


(** Input signature of the various heap-building functors. *)
module type OrderedType = sig
  type t
  (** The type of heap elements. *)
     
  val compare: t -> t -> int
                           (** [compare] must describe a total ordering on values of type [t] by having [compare x y] be positive, negative or zero as [x] is greater, lesser or equal to [y]. For example, [Int.compare] is a suitable comparison function on [int]. *)
end


(** Output signature of the various heap-building functors. *)
module type PriorityQueue = sig

  
  (** The type of elements. *)
  type elt

  type t

  (** Number of elements in the priority queue. *)
  val size : t -> int

  (** Create an empty priority queue. *)
  val empty : t

  (** Linear-time traversal in arbitrary order. *)
  val to_arbitrary_seq : t -> elt Seq.t

  (** Insert element. *)
  val push : elt -> t -> t

  (** Merge two queues. *)
  val merge : t -> t -> t
    
  (** Initialize from a list. For mergeable heaps, [of_list l] may be faster
      than [List.fold_left (fun h x -> push h x) empty]. *)
  val of_list : elt list -> t
    
    
  (** Apply a function to all priority queue elements and return the resulting priority queue. *)
  val map: (elt -> elt) -> t -> t

  (** Coupled results of [peek] and [pop]. *)
  val step : t -> (elt * t) option

  (** Get the top element as an option: [peek empty = None]. *)
  val peek : t -> elt option
  
  (** Traversal in the order given by [Ord.compare]. Possibly inefficient. *)
  val to_ordered_seq : t -> elt Seq.t

  (** Membership query. *)
  val mem : elt -> t -> bool

  (** Using a heap structure to sort a list. *)
  val sort : elt list -> elt list
  
  (** Remove the top element if the queue is not empty: [pop empty = None]. *)
  val pop : t -> t option

  (** Return [true] if the priority queue is empty else [false] *)
  val is_empty : t -> bool
    
end

module LeftistTree (Ord : OrderedType) : PriorityQueue with type elt = Ord.t
(** Leftist trees. *)
     
module SkewHeap (Ord : OrderedType) : PriorityQueue with type elt = Ord.t
(** Skew heaps. *)
     
module PairingHeap (Ord : OrderedType) : PriorityQueue with type elt = Ord.t
                                                                 (** Pairing heaps. *)

(** Binomial heaps. *)
module BinomialHeap (Ord:OrderedType) : PriorityQueue with type elt = Ord.t

