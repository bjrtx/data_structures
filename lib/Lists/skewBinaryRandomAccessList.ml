type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree
type 'a t = (int * 'a tree) list

let empty = []

let cons x = function
  | (w1, t1) :: (w2, t2) :: tl when w1 = w2 ->
      (1 + w1 + w2, Node (x, t1, t2)) :: tl
  | xs -> (1, Leaf x) :: xs

let head = function [] -> None | (_, (Leaf x | Node (x, _, _))) :: _ -> Some x

let tail = function
  | [] -> None
  | (_, Leaf _) :: tl -> Some tl
  | (w, Node (_, l, r)) :: tl ->
      let h = w / 2 in
      Some ((h, l) :: (h, r) :: tl)

let rec lookupTree = function
  | _, (Leaf x | Node (x, _, _)), 0 -> Some x
  | _, Leaf _, _ -> None
  | w, Node (_, l, r), i ->
      let h = w / 2 in
      if i < h then lookupTree (h, l, pred i) else lookupTree (h, r, pred i - h)

let rec updateTree = function
  | _, Leaf _, 0, y -> Some (Leaf y)
  | _, Leaf _, _, _ -> None
  | _, Node (_, l, r), 0, y -> Some (Node (y, l, r))
  | w, Node (x, l, r), i, y ->
      Some
        (let h = w / 2 in
         if i < h then Node (x, Option.get @@ updateTree (h, l, pred i, y), r)
         else Node (x, l, Option.get @@ updateTree (h, r, pred i - h, y)))

let rec lookup = function
  | [], _ -> None
  | (w, t) :: tl, i -> if i < w then lookupTree (w, t, i) else lookup (tl, i - w)

let rec update = function
  | [], _, _ -> None
  | (w, t) :: tl, i, y ->
      if i < w then
        updateTree (w, t, i, y) |> Option.map (fun x -> (w, x) :: tl)
      else update (tl, i - w, y) |> Option.map (List.cons (w, t))
