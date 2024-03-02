(**  Functor building an implementation of the skew heap structure given a totally ordered type.*)
module Make (Ord : Heap.OrderedType) : Heap.PriorityQueue with type elt = Ord.t
