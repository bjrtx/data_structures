type !'a t = 'a list -> 'a list

let of_list = List.append
let to_list (dl : 'a t) = dl []
let empty = Fun.id
let%test _ = to_list empty = []
let append (d1 : _ t) (d2 : _ t) l = d1 @@ d2 l
let cons x (d : _ t) (l : _ list) = x :: d l
let push = cons
let map f dl = dl |> to_list |> List.map f |> of_list
let snoc d x l = d (x :: l)
let concat dls = List.fold_left append empty dls
let size d = List.length @@ to_list d
let mem x d = List.mem x @@ to_list d
let to_seq d = List.to_seq @@ to_list d
let to_arbitrary_seq = to_seq
let is_empty d = d [] = []
let%test _ = is_empty @@ of_list []
let backspace = function _ :: l -> l | [] -> []
let%test _ = to_list backspace = []
let%test _ = to_list @@ append backspace (of_list [ 1; 2; 3 ]) = [ 2; 3 ]
let copy l = l @ l
