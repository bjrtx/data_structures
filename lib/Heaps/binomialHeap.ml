module MakeBase (Ord : Heap.OrderedType) = struct
  type elt = Ord.t
  type t = elt Trees.Multiway.t list
  (* list of possibly empty trees, which will be in decreasing order *)

  type heap = t

  open Trees.Multiway

  let size h =
    List.fold_left (fun a b -> (2 * a) + Bool.to_int (Option.is_some b)) 0 h

  let empty : heap = []

  let to_arbitrary_seq h =
    h |> List.to_seq |> Seq.concat_map Trees.Multiway.to_arbitrary_seq

  let leaf elt = [ Trees.Multiway.leaf elt ]
  let order (Node (_, l)) = List.length l

  (* Merge two binomial trees of the same order k, returning a binomial tree of order k + 1. *)
  let merge_trees_of_same_order (Node (va, la) as ta) (Node (vb, lb) as tb) =
    assert (order ta = order tb);
    if Ord.compare va vb <= 0 then Node (va, tb :: la) else Node (vb, ta :: lb)

  let merge (heapA : heap) (heapB : heap) =
    (* add two lists of possibly empty trees. In each list the orders go up by one at each step *)
    let rec add (listA : elt Trees.Multiway.t list) listB =
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
  let map f = List.map (Trees.Multiway.map f)

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
  let min_tree h = h |> List.filter_map Fun.id |> min_of_node_list
  let peek h = Option.map (fun (Node (v, _)) -> v) (min_tree h)

  let step h =
    Option.map
      (fun (Node (v, l)) ->
        let order = List.length l in
        let other_nodes : heap =
          List.map
            (function
              | Some (Node (_, l)) when List.length l = order -> None | x -> x)
            h
        in
        (v, merge other_nodes (make_heap l)))
      (min_tree h)
end

module Make (Ord : Heap.OrderedType) = Heap.AddOps (MakeBase (Ord))
