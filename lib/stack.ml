type 'a t = 'a list * int

let empty = ([], 0)
let to_list (l, _) = l
let push x (l, n) = (x :: l, succ n)
let map f (l, n) = (List.map f l, n)
let pop = function _ :: tl, n -> Some (tl, pred n) | _ -> None
let peek = function x :: _, _ -> Some x | _ -> None
let is_empty (_, n) = n = 0

(** [size] is constant-time *)
let size (_, n) = n

let mem x (l, _) = List.mem x l
let to_seq (l, _) = List.to_seq l
let to_arbitrary_seq = to_seq
let iter (f : _ -> unit) (l, _) = List.iter f l
let fold f accu (l, _) = List.fold_left f accu l
