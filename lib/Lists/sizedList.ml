type 'a t = 'a list * int

let of_list l = (l, List.length l)
let to_list (l, _) = l
let empty = ([], 0)
let append (l1, n1) (l2, n2) = (l1 @ l2, n1 + n2)
let rev (l, n) = (List.rev l, n)
let cons x (l, n) = (x :: l, succ n)
let push = cons
let tail = function _ :: tl, n -> Some (tl, pred n) | [], _ -> None
let fold_left f acc (l, _) = List.fold_left f acc l
let map f (l, n) = (List.map f l, n)
let concat ls = List.fold_left append empty ls
let size (_, n) = n
let mem x (l, _) = List.mem x l
let to_seq (l, _) = List.to_seq l
let to_arbitrary_seq = to_seq
let is_empty (_, n) = n = 0
