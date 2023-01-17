  type 'a info = {svalue : int; value : 'a}
  type 'a t = 'a info BinaryTree.t
  open BinaryTree
  let empty = Empty
  let map f =
    let g info = {info with value = f info.value} in
    BinaryTree.map g
  let forget tree = BinaryTree.map (fun {value; _} -> value) tree  
  let mem tree elt = BinaryTree.exists (fun {value; _} -> value = elt) tree
  
  let rec merge ta tb = match (ta, tb) with
    | Empty, x | x, Empty -> x
    | Node(info1, l1, r1), Node(info2, _, _) ->
       if info1.value > info2.value then merge tb ta
       else
         let r = merge r1 tb in
         match (l1, r) with
         | Empty, _ -> (* swap *) Node(info1, r, empty)
         | Node(info3, _, _), Node(info4, _, _) ->
            if info3.svalue < info4.svalue then (* swap *)
              Node({info1 with svalue = 1 + info3.svalue}, r, l1)
            else (* don't swap *)
              Node({info1 with svalue = 1 + info4.svalue}, l1, r)
         | _ -> assert false

  let leaf elt = BinaryTree.leaf {svalue = 0; value = elt}
  let push tree elt = merge tree @@ leaf elt 
  let peek tree = node_func (fun {value; _} _ _ -> value) tree
  let pop tree = node_func (fun _ l r -> merge l r) tree

  let to_seq tree =
    (* Return the sequence of all elements in increasing order *)
    (* 4.11 *)
    let step = node_func (fun {value; _} l r -> (value, merge l r)) in
    Seq.unfold step tree 
                           

  let of_list l =
      let rec aux q =
        let open Queue in
        match (take_opt q, take_opt q) with
        | None, None -> BinaryTree.Empty
        | Some x, None | None, Some x -> x
        | Some a, Some b -> (push (merge a b) q; aux q)
      in
      l |> List.to_seq |> Seq.map leaf |> Queue.of_seq |>  aux
      
                            (*  let leftist_invariant : 'a t -> bool *)


  let%test _ = [4; 7; -5; 32; 8] |> of_list |> peek = Some (-5)
  let%test _ = [4; 7; -5; 32; 8] |> of_list |> to_seq |> List.of_seq = [-5; 4; 7; 8; 32]
  let%test _ = ['a'; 'b'; 'x'; 'g'; 'a'] |> of_list |> to_seq |> List.of_seq = ['a'; 'a'; 'b'; 'g'; 'x']
  let%test _ = ['a'; 'b'; 'x'; 'g'; 'a'] |> of_list |> pop = Some (['a'; 'b'; 'g'; 'x'] |> of_list)
                                                                              
                                                                              
