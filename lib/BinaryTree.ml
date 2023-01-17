type 'a t = Empty | Node of 'a * 'a t * 'a t

let node_func (f: 'a -> 'a t -> 'a t -> 'b) = function
  | Empty -> None
  | Node (v, l, r) -> Some (f v l r)
                    
let leaf elt = Node (elt, Empty, Empty)
let empty = Empty

let%test "a leaf is not empty" = leaf 6 <> Empty
          
let rec fold (f: 'a -> 'b -> 'b -> 'b) (initial: 'b): 'a t -> 'b = function
  | Empty -> initial
  | Node (v, l, r) ->
     let l = fold f initial l in
     let r = fold f initial r in
     f v l r

let rec map f = function
  | Empty -> Empty
  | Node (v, l, r) -> Node(f v, map f l, map f r)
                    
let height tree = fold (fun _ l r -> max l r) (-1) tree
let size tree = fold (fun _ l r -> l + r + 1) 0 tree
let breadth tree = fold (fun _ l r -> if l + r = 0 then 1 else 0) 0 tree

let%test _ = height Empty = -1
let%test _= size Empty = 0
let%test _= breadth Empty = 0
                 
let preorder t =
  let open Seq in
  let f v l r = cons v (append l r) in
  fold f empty t

let postorder t =
  let open Seq in
  let f v l r = append (append l r) (return v) in
  fold f empty t

let inorder t=
  let open Seq in
  let f v l r = append l (cons v r) in
  fold f empty t
  
let to_seq = preorder

let exists pred t = t |> fold (fun v l r -> (pred v) || l || r) false

let%test _ = not (exists (fun _ -> true) Empty)
                  
let mem t elt = exists ((=) elt) t


                  


                  

