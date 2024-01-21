module BinaryTree = Tree.Binary
module MultiwayTree = Tree.Multiway

module type OrderedType = sig
  type t

  val compare : t -> t -> int
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

module type PriorityQueue = sig
  include Base

  val to_ordered_seq : t -> elt Seq.t
  val mem : elt -> t -> bool
  val sort : elt list -> elt list
  val pop : t -> t option
  val is_empty : t -> bool
end

module AddOps
    (Q : Base)
(*: PriorityQueue with type elt = Q.elt and type t = Q.t *) =
struct
  include Q

  let to_ordered_seq = Seq.unfold step
  let pop t = Option.map snd @@ step t
  let mem x heap = heap |> to_arbitrary_seq |> List.of_seq |> List.mem x
  let sort l = l |> of_list |> to_ordered_seq |> List.of_seq
  let is_empty t = Option.is_none @@ peek t
end

module LeftistTreeMake (Ord : OrderedType) = struct
  type elt = Ord.t
  type t = (int * elt) BinaryTree.t (* the first parameter is the svalue *)

  open BinaryTree

  let empty = BinaryTree.Empty
  let map f = BinaryTree.map (fun (svalue, x) -> (svalue, f x))
  let size = BinaryTree.size
  let svalue = function Empty -> -1 | Node ((s, _), _, _) -> s

  let rec merge a b =
    match (a, b) with
    | Empty, x | x, Empty -> x
    | Node ((_, va), _, _), Node ((_, vb), _, _) when Ord.compare va vb > 0 ->
        merge b a
    | Node ((_, x), l, r), b ->
        let r = merge r b in
        (* of l and r, the subtree with the larger s-value goes to the left *)
        let l, r = if svalue l >= svalue r then (l, r) else (r, l) in
        Node ((1 + min (svalue l) (svalue r), x), l, r)

  let leaf elt = BinaryTree.leaf (0, elt)
  let push elt = merge (leaf elt)
  let step = BinaryTree.node_func (fun (_, x) l r -> (x, merge l r))
  let peek t = BinaryTree.node_func (fun (_, x) _ _ -> x) t
  let to_arbitrary_seq tree = tree |> BinaryTree.to_arbitrary_seq |> Seq.map snd

  let rec merge_in_pairs = function
    | a :: b :: tl -> merge (merge a b) (merge_in_pairs tl)
    | [ hd ] -> hd
    | [] -> Empty

  let of_list l = l |> List.map leaf |> merge_in_pairs

  (* Todo: Leftist invariant *)
end

module SkewHeapMake (Ord : OrderedType) = struct
  open BinaryTree

  type elt = Ord.t
  type t = elt BinaryTree.t

  let size = BinaryTree.size
  let empty = BinaryTree.empty
  let to_arbitrary_seq = BinaryTree.to_arbitrary_seq
  let map = BinaryTree.map

  let rec merge a b =
    match (a, b) with
    | Empty, x | x, Empty -> x
    | Node (va, la, ra), Node (vb, lb, rb) ->
        if Ord.compare va vb <= 0 then Node (va, merge ra b, la)
        else Node (vb, merge rb a, lb)

  let push elt = merge (BinaryTree.leaf elt)
  let step heap = BinaryTree.node_func (fun v l r -> (v, merge l r)) heap
  let peek heap = BinaryTree.node_func (fun v _ _ -> v) heap

  let rec merge_in_pairs = function
    | a :: b :: tl -> merge (merge a b) (merge_in_pairs tl)
    | [ hd ] -> hd
    | [] -> Empty

  let of_list l = l |> List.map BinaryTree.leaf |> merge_in_pairs
end

module PairingHeapMake (Ord : OrderedType) = struct
  type elt = Ord.t
  type t = elt MultiwayTree.t

  open MultiwayTree

  let size = MultiwayTree.size
  let empty = MultiwayTree.empty
  let to_arbitrary_seq = MultiwayTree.to_arbitrary_seq
  let leaf = MultiwayTree.leaf

  (** In [map f], [f] {b should} be non-decreasing to preserve the invariant. Arbitrary choice of [f] may break the invariant. *)
  let map = MultiwayTree.map

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

module BinomialHeapMake (Ord : OrderedType) = struct
  type elt = Ord.t
  type t = elt MultiwayTree.t list
  (* list of possibly empty trees, which will be in decreasing order *)

  type heap = t

  open MultiwayTree

  let size h =
    let zero_one = function None -> 0 | Some _ -> 1 in
    h |> List.map zero_one |> List.fold_left (fun a b -> (2 * a) + b) 0

  let empty : heap = []

  let to_arbitrary_seq h =
    h |> List.to_seq |> Seq.concat_map MultiwayTree.to_arbitrary_seq

  let leaf elt = [ MultiwayTree.leaf elt ]
  let order (Node (_, l)) = List.length l

  (* Merge two binomial trees of the same order k, returning a binomial tree of order k + 1. *)
  let merge_trees_of_same_order (Node (va, la) as ta) (Node (vb, lb) as tb) =
    let () = assert (order ta = order tb) in
    if Ord.compare va vb <= 0 then Node (va, tb :: la) else Node (vb, ta :: lb)

  let merge (heapA : heap) (heapB : heap) =
    (* add two lists of possibly empty trees. In each list the orders go up by one at each step *)
    let rec add (listA : elt MultiwayTree.t list) listB =
      match (listA, listB) with
      | [], l | l, [] -> l
      | hda :: tla, hdb :: tlb -> (
          match (hda, hdb) with
          | x, None | None, x -> x :: add tla tlb
          | Some a, Some b ->
              assert (order a = order b);
              None :: add (add tla [ Some (merge_trees_of_same_order a b) ]) tlb
          )
    in
    List.rev @@ add (List.rev heapA) (List.rev heapB)

  let push elt = merge (leaf elt)

  let rec merge_in_pairs = function
    | a :: b :: tl -> merge (merge a b) (merge_in_pairs tl)
    | [ hd ] -> hd
    | [] -> empty

  let of_list l = l |> List.map leaf |> merge_in_pairs
  let map f = List.map (MultiwayTree.map f)

  (* find the tree with the smallest root value *)
  let min_of_node_list = function
    | [] -> None
    | hd :: tl ->
        Some
          (List.fold_left
             (fun (Node (va, _) as a) (Node (vb, _) as b) ->
               if Ord.compare va vb <= 0 then a else b)
             hd tl)

  let make_heap node_list =
    (* insert empty trees at all empty orders *)
    (* sort by decreasing order *)
    let node_list =
      List.sort
        (fun (Node (_, la)) (Node (_, lb)) ->
          compare (List.length lb) (List.length la))
        node_list
    in
    let rec aux expected_order = function
      | [] -> List.init (succ expected_order) (fun _ -> None)
      | n :: tl ->
          if order n = expected_order then
            Some n :: aux (pred expected_order) tl
          else None :: aux (pred expected_order) (n :: tl)
    in
    match node_list with
    | [] -> []
    | Node (_, l) :: _ -> aux (List.length l) node_list

  (* smallest tree in h *)
  let min_tree h =
    h |> List.filter Option.is_some |> List.map Option.get |> min_of_node_list

  let peek h = Option.map (fun (Node (v, _)) -> v) (min_tree h)

  let step h =
    match min_tree h with
    | None -> None (* empty heap *)
    | Some (Node (v, l)) ->
        let order = List.length l in
        let other_nodes : heap =
          List.map
            (function
              | Some (Node (_, l)) when List.length l = order -> None | x -> x)
            h
        in
        Some (v, merge other_nodes (make_heap l))
end

module LeftistTree (Ord : OrderedType) = AddOps (LeftistTreeMake (Ord))
module SkewHeap (Ord : OrderedType) = AddOps (SkewHeapMake (Ord))
module PairingHeap (Ord : OrderedType) = AddOps (PairingHeapMake (Ord))
module BinomialHeap (Ord : OrderedType) = AddOps (BinomialHeapMake (Ord))
