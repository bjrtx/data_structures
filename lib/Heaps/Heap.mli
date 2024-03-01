(** Heap-based purely functional implementations of priority queues. *)

(** Input signature of the various heap-building functors. *)
module type OrderedType = sig
  type t
  (** The type of heap elements. *)

  val compare : t -> t -> int
  (** [compare] must describe a total ordering on values of type [t] by having [compare x y] be positive, negative or zero as [x] is greater, lesser or equal to [y]. For example, [Int.compare] is a suitable comparison function on [int]. *)
end

(** Output signature of the various heap-building functors. *)
module type PriorityQueue = sig
  type elt
  (** The type of elements. *)

  type t

  val size : t -> int
  (** Number of elements in the priority queue. *)

  val empty : t
  (** Create an empty priority queue. *)

  val to_arbitrary_seq : t -> elt Seq.t
  (** Linear-time traversal in arbitrary order. *)

  val push : elt -> t -> t
  (** Insert element. *)

  val merge : t -> t -> t
  (** Merge two queues. *)

  val of_list : elt list -> t
  (** Initialize from a list. For mergeable heaps, [of_list l] may be faster
      than [List.fold_left (fun h x -> push h x) empty]. *)

  val map : (elt -> elt) -> t -> t
  (** Apply a function to all priority queue elements and return the resulting priority queue. *)

  val step : t -> (elt * t) option
  (** Coupled results of [peek] and [pop]. *)

  val peek : t -> elt option
  (** Get the top element as an option: [peek empty = None]. *)

  val to_ordered_seq : t -> elt Seq.t
  (** Traversal in the order given by [Ord.compare]. Possibly inefficient. *)

  val mem : elt -> t -> bool
  (** Membership query. *)

  val sort : elt list -> elt list
  (** Using a heap structure to sort a list. *)

  val pop : t -> t option
  (** Remove the top element if the queue is not empty: [pop empty = None]. *)

  val is_empty : t -> bool
  (** Return [true] if the priority queue is empty else [false] *)
end

module type Base = sig
  type elt
  type t

  val size : t -> int
  val empty : t
  val to_arbitrary_seq : t -> elt Seq.t
  val push : elt -> t -> t
  val merge : t -> t -> t
  val of_list : elt list -> t
  val map : (elt -> elt) -> t -> t
  val step : t -> (elt * t) option
  val peek : t -> elt option
end

module AddOps (B : Base) : PriorityQueue with type elt = B.elt

