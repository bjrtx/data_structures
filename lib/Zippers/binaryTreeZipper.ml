(* Binary trees with a zipper. *)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

type 'a context =
  | Context of { isRight : bool; parent : 'a; sibling : 'a tree }

type 'a t = 'a tree * 'a context list

let empty = (Empty, [])
let of_tree t = (t, [])

let to_tree (t, contexts) =
  List.fold_left
    (fun tree (Context { isRight; parent; sibling }) ->
      let l, r = if isRight then (sibling, tree) else (tree, sibling) in
      Node (l, parent, r))
    t contexts

let left = function
  | t, Context { sibling; isRight = true; parent } :: tl ->
      (sibling, Context { sibling = t; isRight = false; parent } :: tl)
  | z -> z

let right = function
  | t, Context { sibling; isRight = false; parent } :: tl ->
      (sibling, Context { sibling = t; isRight = true; parent } :: tl)
  | z -> z

let up = function
  | t, Context { sibling; isRight; parent } :: tl ->
      let l, r = if isRight then (sibling, t) else (t, sibling) in
      (Node (l, parent, r), tl)
  | z -> z

let down_left = function
  | Node (l, a, r), contexts ->
      (l, Context { isRight = false; parent = a; sibling = r } :: contexts)
  | z -> z

let down_right = function
  | Node (l, a, r), contexts ->
      (r, Context { isRight = true; parent = a; sibling = l } :: contexts)
  | z -> z

let change t (_, contexts) = (t, contexts)
