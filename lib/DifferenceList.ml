type !'a dlist =  'a list -> 'a list

let of_list = List.append
let to_list (dl : 'a dlist) = dl []
let empty (l: _ list) = l
                      
let append (d1: _ dlist) (d2 : _ dlist) l = d1 @@ d2 l

let cons x (d : _ dlist) (l : _ list) = x :: (d l)
let push = cons

let map f dl = dl |> to_list |> List.map f |> of_list 
let snoc d x l = d (x :: l)
let concat dls = List.fold_left append empty dls

let size d = List.length @@ to_list d
let mem x d = List.mem x @@ to_list d
let to_seq d = List.to_seq @@ to_list d
let to_arbitrary_seq = to_seq
let is_empty d = d [] = []



