module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type BST = sig
  type elt
  type t

  val size : t -> int
  val empty : t
  val to_arbitrary_seq : t -> elt Seq.t
  val push : elt -> t -> t
  val of_list : elt list -> t
  val map : (elt -> elt) -> t -> t
  val to_ordered_seq : t -> elt Seq.t
  val mem : elt -> t -> bool
  val sort : elt list -> elt list
  val is_empty : t -> bool
end

module DefaultBST (Ord : OrderedType) = struct
  type elt = Ord.t
  type t = Empty | Node of elt * t * t

  let node_func f = function Empty -> None | Node (v, l, r) -> Some (f v l r)
  let leaf elt = Node (elt, Empty, Empty)
  let empty = Empty
  let is_empty = function Empty -> true | _ -> false

  let rec fold (f : elt -> 'b -> 'b -> 'b) (initial : 'b) : t -> 'b = function
    | Empty -> initial
    | Node (v, l, r) -> f v (fold f initial l) (fold f initial r)

  let rec map f = function
    | Empty -> Empty
    | Node (v, l, r) -> Node (f v, map f l, map f r)

  let height tree = fold (fun _ l r -> max l r) (-1) tree
  let size tree = fold (fun _ l r -> l + r + 1) 0 tree
  let breadth tree = fold (fun _ l r -> if l + r = 0 then 1 else 0) 0 tree
  let%test _ = height Empty = -1
  let%test _ = size Empty = 0
  let%test _ = breadth Empty = 0

  let preorder t =
    let open Seq in
    let f v l r = cons v (append l r) in
    fold f empty t

  let postorder t =
    let open Seq in
    let f v l r = append (append l r) (return v) in
    fold f empty t

  let inorder t =
    let open Seq in
    let f v l r = append l (cons v r) in
    fold f empty t

  let to_seq = preorder
  let to_ordered_seq = to_seq
  let to_arbitrary_seq = preorder
  let exists pred t = t |> fold (fun v l r -> pred v || l || r) false

  let%test "No predicate is satisfiable on Empty" =
    not (exists (fun _ -> true) Empty)

  let rec mem elt = function
    | Empty -> false
    | Node (v, l, r) ->
        let cmp = Ord.compare elt v in
        cmp = 0 || (cmp < 0 && mem elt l) || (cmp > 0 && mem elt r)

  let rec push elt = function
    | Empty -> leaf elt
    | Node (v, l, r) ->
        let cmp = Ord.compare elt v in
        if cmp <= 0 then Node (v, push elt l, r) else Node (v, l, push elt r)

  let of_list = List.fold_left (fun t x -> push x t) Empty
  let sort l = l |> of_list |> to_ordered_seq |> List.of_seq

  let rec bounds x =
    let open Option in
    let or_else a b = some @@ value a ~default:b in
    function
    | Empty -> (None, None)
    | Node (v, l, r) ->
        let cmp = Ord.compare x v in
        if cmp = 0 then (Some v, Some v)
        else if cmp < 0 then
          let a, b = bounds x l in
          (a, or_else b v)
        else
          let a, b = bounds x r in
          (or_else a v, b)
end

module RBTree (Ord : OrderedType) = struct
  type elt = Ord.t
  type color = Red | Black

  (* A node has color, height, value, and children *)
  type t = Empty | Node of color * int * elt * t * t

  let empty = Empty
  let is_empty = function Empty -> true | _ -> false
  let height = function Empty -> -1 | Node (_, h, _, _, _) -> h
  let leaf x = Node (Black, 0, x, Empty, Empty)

  let rec fold (f : elt -> 'b -> 'b -> 'b) (initial : 'b) : t -> 'b = function
    | Empty -> initial
    | Node (_, _, v, l, r) -> f v (fold f initial l) (fold f initial r)

  let rec map f = function
    | Empty -> Empty
    | Node (c, h, v, l, r) -> Node (c, h, f v, map f l, map f r)

  let size tree = fold (fun _ l r -> l + r + 1) 0 tree
  let breadth tree = fold (fun _ l r -> if l + r = 0 then 1 else 0) 0 tree

  let rec min_opt = function
    | Empty -> None
    | Node (_, v, _, l, _) -> if is_empty l then Some v else min_opt l

  let%test _ = height Empty = -1
  let%test _ = size Empty = 0
  let%test _ = breadth Empty = 0

  let is_constant seq =
    let open Seq in
    let rec const_equal v s =
      match s () with Nil -> true | Cons (x, xs) -> x = v && const_equal v xs
    in
    match seq () with Nil -> true | Cons (x, xs) -> const_equal x xs

  let black_vals =
    let rec aux n = function
      | Empty -> Seq.return n
      | Node (c, v, _, l, r) ->
          let n = if c = Black then succ n else n in
          Seq.append (aux n l) (aux n r)
    in
    aux 0

  let reds_are_separate =
    let rec aux prev t =
      match (prev, t) with
      | _, Empty -> true
      | Red, Node (Red, _, _, _, _) -> false
      | Black, Node (c, _, _, l, r) -> aux c l && aux c r
      | _, _ -> true
    in
    (* unreachable *)
    aux Black

  let is_balanced t = (is_constant @@ black_vals t) && reds_are_separate t

  (*todo : is_ordered *)

  let preorder t =
    let open Seq in
    let f v l r = cons v (append l r) in
    fold f empty t

  let postorder t =
    let open Seq in
    let f v l r = append (append l r) (return v) in
    fold f empty t

  let inorder t =
    let open Seq in
    let f v l r = append l (cons v r) in
    fold f empty t

  let to_seq = preorder
  let to_ordered_seq = to_seq
  let to_arbitrary_seq = preorder
  let exists pred t = t |> fold (fun v l r -> pred v || l || r) false

  let%test "No predicate is satisfiable on Empty" =
    not (exists (fun _ -> true) Empty)

  let rec mem elt = function
    | Empty -> false
    | Node (_, _, v, l, r) ->
        let cmp = Ord.compare elt v in
        cmp = 0 || (cmp < 0 && mem elt l) || (cmp > 0 && mem elt r)

  let rec push elt = function
    | Empty -> leaf elt
    | Node (c, h, v, l, r) ->
        let cmp = Ord.compare elt v in
        if cmp <= 0 then Node (c, h, v, push elt l, r)
        else Node (c, h, v, l, push elt r)

  let of_list = List.fold_left (fun t x -> push x t) Empty
  let sort l = l |> of_list |> to_ordered_seq |> List.of_seq
end
