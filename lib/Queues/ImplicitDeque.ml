module type QS = sig
  type 'a t

  val empty : 'a t

  val is_empty : 'a t -> bool
  (** [is_empty q] is [true] if and only if [q] has no elements. *)

  val push : 'a -> 'a t -> 'a t
  (** [push x q] is obtained by the insertion of [x] into [q]. *)
  val cons : 'a -> 'a t -> 'a t
  val snoc : 'a -> 'a t -> 'a t
  val last : 'a t-> 'a option
  val poplast: 'a t -> 'a t option

  val peek : 'a t -> 'a option
  (** [peep x q] is either [None] if [q] is empty or [Some x] where [x] is the first-out element of [q]. *)

  val pop : 'a t -> 'a t option
end

module ImplicitDequeue (Q : QS) = struct
  type 'a shallow = Zero | One of 'a | Two of 'a * 'a | Three of 'a * 'a * 'a

  type 'a t =
    | Shallow of 'a shallow
    | Deep of { f : 'a shallow; m : ('a * 'a) Q.t Lazy.t; r : 'a shallow }

  let empty = Shallow Zero
  let is_empty = function Shallow Zero -> true | _ -> false
  let dcons x = function
  | Zero -> One x
  | One y -> Two(x, y)
  | Two (y, z) -> Three(x,y,z)
  | _ -> failwith ""
  let dsnoc x = function
  | Zero -> One x
  | One y -> Two(y, x)
  | Two (y, z) -> Three(y,z,x)
  | _ -> failwith ""

  let dhead  = function
  | Zero -> None
  | One x | Two (x, _) | Three (x, _,_) -> Some x
  let dlast = function
  | Zero -> None
  | One x | Two (_, x) | Three (_,_, x) -> Some x
  let dtail = function
  | Zero -> None
  | One _ -> Some Zero
  | Two (_, y) -> Some (One y)
  | Three (_, y, z) -> Some (Two (y, z))
  let dinit = function
  | Zero -> None
  | One _ -> Some Zero
  | Two (x, _) -> Some (One x)
  | Three (x, y, _) -> Some (Two (x, y))

  let cons x = function
  | Shallow (Three(a, b, c)) -> Deep {f = Two(x, a); m = lazy Q.empty; r = Two(b, c)}
  | Shallow d -> Shallow (dcons x d)
  | Deep {f = Three(a,b,c); m; r} -> Deep {f = Two(x, a); m = lazy (Q.cons (b,c) (Lazy.force m)); r}
  | Deep deep -> Deep {deep with f = dcons x deep.f}

  let head = function Shallow d -> dhead d | Deep {f; _} -> dhead f

  let tail = function
  | Shallow d -> Some (Shallow (Option.get @@ dtail d))
  | Deep {f = One _; m = lazy ps; r} -> begin
      match Q.peek ps with 
      | None -> Some (Shallow r)
      | Some (b, c) -> Some (Deep {f = Two(b, c); m = lazy(Option.get @@ Q.pop ps); r})
    end
  | Deep deep -> Some (Deep {deep with f = Option.get @@ dtail deep.f})

  let snoc x = function
  | Shallow (Three(a, b, c)) -> Deep {f = Two(a, b); m = lazy Q.empty; r = Two(c, x)}
  | Shallow d -> Shallow (dsnoc x d)
  | Deep {r = Three(a,b,c); m; f} -> Deep {f; m = lazy (Q.snoc (a, b) (Lazy.force m)); r=Two(c,x)}
  | Deep deep -> Deep {deep with r = dsnoc x deep.r}

  let last  = function
  | Shallow d -> dlast d
  | Deep {r; _} -> dlast r

  let init = function
  | Shallow d -> Some (Shallow (Option.get @@ dinit d))
  | Deep {r = One _; m = lazy ps; f} -> begin
      match Q.last ps with 
      | None -> Some (Shallow f)
      | Some (b, c) -> Some (Deep {r = Two(b, c); m = lazy(Option.get @@ Q.poplast ps); f})
    end
  | Deep deep -> Some (Deep {deep with r = Option.get @@ dinit deep.r})
end
