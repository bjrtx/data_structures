module type QS = sig
  type 'a t

  val empty : 'a t

  val is_empty : 'a t -> bool
  (** [is_empty q] is [true] if and only if [q] has no elements. *)

  val push : 'a -> 'a t -> 'a t
  (** [push x q] is obtained by the insertion of [x] into [q]. *)

  val peek : 'a t -> 'a option
  (** [peep x q] is either [None] if [q] is empty or [Some x] where [x] is the first-out element of [q]. *)

  val pop : 'a t -> 'a t option
end

module ImplicitQueue (Q : QS) = struct
  type 'a zero_one = Zero | One of 'a
  type 'a one_two = One of 'a | Two of 'a * 'a

  type 'a t =
    | Shallow of 'a zero_one
    | Deep of { f : 'a one_two; m : ('a * 'a) Q.t Lazy.t; r : 'a zero_one }

  let empty = Shallow Zero
  let is_empty = function Shallow Zero -> true | _ -> false

  let snoc y = function
    | Shallow Zero -> Shallow (One y)
    | Shallow (One x) -> Deep { f = Two (x, y); m = lazy Q.empty; r = Zero }
    | Deep ({ r = Zero; _ } as deep) -> Deep { deep with r = One y }
    | Deep ({ r = One x; m = (lazy q); _ } as deep) ->
        Deep { deep with m = lazy (Q.push (x, y) q); r = Zero }

  let peek = function
    | Shallow Zero -> None
    | Shallow (One x) | Deep { f = One x; _ } | Deep { f = Two (x, _); _ } ->
        Some x

  let pop = function
    | Shallow Zero -> None
    | Shallow _ -> Some empty
    | Deep ({ f = Two (_, y); _ } as deep) ->
        Some (Deep { deep with f = One y })
    | Deep { m = (lazy q); r; _ } ->
        Some
          (match Q.peek q with
          | None -> Shallow r
          | Some (y, z) ->
              Deep { f = Two (y, z); m = lazy (Option.get @@ Q.pop q); r })
end
