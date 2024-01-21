(** Queue with amortized constant time insertion and removal. *)


type 'a q = 'a list * 'a list

let empty = ([], [])
let is_empty q = q = ([], [])
let mem  elt (l, r) = (List.mem elt l) || (List.mem elt r)
let size (l, r) = List.length l + List.length r
let rev (l, r) = (r, l)
let push x (l, r) = (x :: l, r)
let map f (l, r) = (List.map f l, List.map f r)
let peek = function
  | (_, x :: _) -> Some x
  | ([], []) -> None
  | (l, []) -> Some (List.hd @@ List.rev l)
                 
let rec pop = function
  | (l, _ :: tl) -> Some (l, tl)
  | ([], []) -> None
  | (l, []) -> pop ([], List.rev l)

let to_seq (l, r) = Seq.append (List.to_seq l) List.(to_seq @@ rev r) 

let to_arbitrary_seq (l, r) = Seq.append (List.to_seq l) (List.to_seq r)
