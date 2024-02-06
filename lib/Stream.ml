(** Lazy streams. *)

type 'a cell = Nil | Cons of 'a * 'a t
and 'a t = 'a cell Lazy.t

let rec append s t =
  lazy
    (match s with
    | (lazy Nil) -> Lazy.force t
    | (lazy (Cons (hd, tl))) -> Cons (hd, append tl t))

let rec take n s =
  lazy
    (match (n, s) with
    | 0, _ -> Nil
    | _, (lazy Nil) -> Nil
    | _, (lazy (Cons (hd, tl))) -> Cons (hd, take (pred n) tl))

let drop n s =
  let rec aux = function
    | 0, (lazy tl) -> tl
    | _, (lazy Nil) -> Nil
    | n, (lazy (Cons (_, tl))) -> aux (pred n, tl)
  in
  lazy (aux (n, s))

let reverse s =
  let rec aux = function
    | (lazy Nil), r -> r
    | (lazy (Cons (hd, tl))), r -> aux (tl, Cons (hd, lazy r))
  in
  lazy (aux (s, Nil))

let empty = lazy Nil
let is_empty = function (lazy Nil) -> true | _ -> false

let rec to_list = function
  | (lazy Nil) -> []
  | (lazy (Cons (hd, tl))) -> hd :: to_list tl

let rec to_seq = function
  | (lazy Nil) -> Seq.empty
  | (lazy (Cons (hd, tl))) -> fun () -> Seq.Cons (hd, to_seq tl)

let rec map f = function
  | (lazy Nil) -> lazy Nil
  | (lazy (Cons (hd, tl))) -> lazy (Cons (f hd, map f tl))

let peek = function
  | (lazy Nil) -> None
  | (lazy (Cons (hd, _))) -> Some hd
(* val push : 'a -> 'a t -> 'a t

   val map : ('a -> 'b) -> 'a t -> 'b t
   (** [to_seq (map f stack) = Seq.map f (to_seq stack)] *)

   val pop : 'a t -> 'a t option
   val peek : 'a t -> 'a option
   val is_empty : 'a t -> bool
*)
