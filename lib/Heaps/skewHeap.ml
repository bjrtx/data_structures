module MakeBase (Ord : Heap.OrderedType) = struct
  module BTree = Trees.Binary
  open BTree

  type elt = Ord.t
  type t = elt BTree.t

  let size = BTree.size
  let empty = BTree.empty
  let to_arbitrary_seq = BTree.to_arbitrary_seq
  let map = BTree.map

  let rec merge a b =
    match (a, b) with
    | Empty, x | x, Empty -> x
    | Node (va, la, ra), Node (vb, lb, rb) ->
        if Ord.compare va vb <= 0 then Node (va, merge ra b, la)
        else Node (vb, merge rb a, lb)

  let push elt = merge (BTree.leaf elt)
  let step heap = BTree.node_func (fun v l r -> (v, merge l r)) heap
  let peek heap = BTree.node_func (fun v _ _ -> v) heap

  let rec merge_in_pairs = function
    | a :: b :: tl -> merge (merge a b) (merge_in_pairs tl)
    | [ hd ] -> hd
    | [] -> Empty

  let of_list l = l |> List.map BTree.leaf |> merge_in_pairs
end

module Make (Ord : Heap.OrderedType) = Heap.AddOps (MakeBase (Ord))
