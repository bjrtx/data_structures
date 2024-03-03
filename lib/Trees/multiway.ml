type +'a tree = Node of 'a * 'a tree list
type +'a t = 'a tree option

let node_func (f : 'a -> 'a tree list -> 'b) =
  Option.map (fun (Node (v, l)) -> f v l)

let leaf_tree elt = Node (elt, [])
let leaf elt = Some (leaf_tree elt)

let push elt = function
  | None -> leaf elt
  | Some node -> Some (Node (elt, [ node ]))

let empty = None
let is_empty = Option.is_none

(* let%test "a leaf is not empty" = leaf 6 <> None *)

let rec reduce_tree (f : 'a -> 'b list -> 'b) (Node (v, l)) : 'b =
  f v @@ List.map (reduce_tree f) l

let fold (f : 'a -> 'b list -> 'b) (initial : 'b) : 'a t -> 'b = function
  | None -> initial
  | Some n -> reduce_tree f n

let map f =
  let rec tree_map f (Node (v, l)) = Node (f v, List.map (tree_map f) l) in
  Option.map (tree_map f)

let height tree = fold (fun _ l -> 1 + List.fold_left max 0 l) (-1) tree
let size_tree tree = reduce_tree (fun _ l -> 1 + List.fold_left ( + ) 0 l) tree
let size tree = fold (fun _ l -> 1 + List.fold_left ( + ) 0 l) 0 tree
let%test _ = height None = -1
let%test _ = size None = 0

let preorder t =
  Seq.(fold (fun v l -> cons v @@ concat @@ List.to_seq l) empty t)

let postorder t =
  Seq.(fold (fun v l -> append (concat @@ List.to_seq l) (return v)) empty t)

let to_seq = preorder
let to_arbitrary_seq = preorder
let mem elt t = t |> to_seq |> List.of_seq |> List.mem elt
(* not optimal, better solutions in 4.14+ *)
