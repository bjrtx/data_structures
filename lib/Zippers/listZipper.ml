type 'a t = 'a list * 'a list

let empty = ([], [])
let is_empty = function [], [] -> true | _ -> false
let map f (l, r) = List.(map f l, map f r)
let of_list l = (l, [])
let of_list_end l = ([], List.rev l)
let rec to_list = function l, [] -> l | l, hd :: tl -> to_list (hd :: l, tl)
let current = function hd :: _, _ -> Some hd | _ -> None
let left = function (_, []) as z -> z | l, hd :: tl -> (hd :: l, tl)
let right = function ([], _) as z -> z | hd :: tl, r -> (tl, hd :: r)
let reverse (l, r) = (r, l)
let insert x (l, r) = (x :: l, r)
let change x = function _ :: l, r -> (x :: l, r) | [], _ -> failwith "Empty"
