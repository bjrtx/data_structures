module Make (Ord : Heap.OrderedType) : Heap.PriorityQueue with type elt = Ord.t
(**  Functor building an implementation of the leftist-tree structure given a totally ordered type.*)