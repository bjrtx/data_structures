module Make (Ord : Heap.OrderedType) : Heap.PriorityQueue with type elt = Ord.t
