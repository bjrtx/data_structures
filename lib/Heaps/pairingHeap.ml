module MakeBase (Ord : Heap.OrderedType) = struct
  type elt = Ord.t
  type t = elt Tree.Multiway.t

  open Tree.Multiway

  let size = Tree.Multiway.size
  let empty = Tree.Multiway.empty
  let to_arbitrary_seq = Tree.Multiway.to_arbitrary_seq
  let leaf = Tree.Multiway.leaf

  (** In [map f], [f] {b should} be non-decreasing to preserve the invariant. Arbitrary choice of [f] may break the invariant. *)
  let map = Tree.Multiway.map

  let merge_trees (Node (v1, l1)) (Node (v2, l2)) =
    if Ord.compare v1 v2 <= 0 then Node (v1, Node (v2, l2) :: l1)
    else Node (v2, Node (v1, l1) :: l2)

  let merge a b =
    match (a, b) with
    | None, x | x, None -> x
    | Some na, Some nb -> Some (merge_trees na nb)

  let push x heap = merge (leaf x) heap

  let rec merge_in_pairs = function
    | a :: b :: tl -> merge (Some (merge_trees a b)) (merge_in_pairs tl)
    | [ hd ] -> Some hd
    | [] -> None

  let leaf v = Node (v, [])
  let step heap = node_func (fun v l -> (v, merge_in_pairs l)) heap
  let peek heap = node_func (fun v _ -> v) heap
  let of_list l = l |> List.map leaf |> merge_in_pairs
end

module Make (Ord : Heap.OrderedType) = Heap.AddOps (MakeBase (Ord))
