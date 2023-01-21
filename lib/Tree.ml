module Binary = struct
  type 'a t = Empty | Node of 'a * 'a t * 'a t

  let node_func (f: 'a -> 'a t -> 'a t -> 'b) = function
    | Empty -> None
    | Node (v, l, r) -> Some (f v l r)
                      
  let leaf elt = Node (elt, Empty, Empty)

  let push elt tree = Node(elt, tree, Empty)
                    
  let empty = Empty

  let is_empty tree= tree = Empty
                   
  (*let%test "a leaf is not empty" = leaf 6 <> Empty*)
                   
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

  (*let%test _ = height Empty = -1
let%test _= size Empty = 0
let%test _= breadth Empty = 0 *)
                   
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
  let to_arbitrary_seq = preorder

  let exists pred t = t |> fold (fun v l r -> (pred v) || l || r) false

  (* let%test _ = not (exists (fun _ -> true) Empty) *)
                    
  let mem elt = exists ((=) elt)
end

module Multiway = struct
  type +'a tree = Node of 'a * 'a tree list
  type +'a t = 'a tree option

  let node_func (f: 'a -> 'a tree list -> 'b) =
    Option.map (fun (Node(v, l)) -> f v l)

  let leaf_tree elt = Node (elt, [])
  let leaf elt = Some (leaf_tree elt)

  let push elt = function
    | None -> leaf elt
    | Some node -> Some (Node(elt, [node]))
                 
  let empty = None
            
  let is_empty = Option.is_none
                    
  (* let%test "a leaf is not empty" = leaf 6 <> None *)

  let rec reduce_tree (f: 'a -> 'b list -> 'b) (Node (v, l)) : 'b = 
    f v @@ List.map (reduce_tree f) l
                    
  let fold (f: 'a -> 'b list -> 'b) (initial : 'b) : 'a t -> 'b = function
    | None -> initial
    | Some n -> reduce_tree f n 

  let map f =
    let rec tree_map f (Node (v, l)) =
      Node(f v, List.map (tree_map f) l)
    in Option.map (tree_map f)
                  
  let height tree = fold (fun _ l -> 1 + List.fold_left max 0 l) (-1) tree
  let size_tree tree = reduce_tree (fun _ l -> 1 + List.fold_left (+) 0 l) tree
  let size tree = fold (fun _ l -> 1 + List.fold_left (+) 0 l) 0 tree


  (* let%test _ = height None = -1
let%test _= size None = 0
   *)
                
  let preorder t =
    let f v l = Seq.cons v @@ Seq.concat @@ List.to_seq l in
    fold f Seq.empty t

  let postorder t =
    let open Seq in
    let f v l =append (Seq.concat @@ List.to_seq l) (return v) in
    fold f empty t

  let to_seq = preorder
  let to_arbitrary_seq = preorder

                       
  let mem elt t =
    t |> to_seq |> List.of_seq |> List.mem elt 
                                           (* not optimal, better solutions in 4.14+ *)
end


                    

