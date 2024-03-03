type 'a zero_one = Zero | One of 'a
type 'a one_two = One of 'a | Two of 'a * 'a

type 'a t =
  | Shallow of 'a zero_one
  | Deep of { f : 'a one_two; m : ('a * 'a) t Lazy.t; r : 'a zero_one }

let empty = Shallow Zero
let is_empty = function Shallow Zero -> true | _ -> false

let rec push : 'a. 'a -> 'a t -> 'a t =
 fun y -> function
  | Shallow Zero -> Shallow (One y)
  | Shallow (One x) -> Deep { f = Two (x, y); m = lazy empty; r = Zero }
  | Deep ({ r = Zero; _ } as deep) -> Deep { deep with r = One y }
  | Deep ({ r = One x; m; _ } as deep) ->
      Deep { deep with m = Lazy.map (push (x, y)) m; r = Zero }

let peek = function
  | Shallow Zero -> None
  | Shallow (One x) | Deep { f = One x; _ } | Deep { f = Two (x, _); _ } ->
      Some x

let rec pop : 'a. 'a t -> 'a t option = function
  | Shallow Zero -> None
  | Shallow _ -> Some empty
  | Deep ({ f = Two (_, y); _ } as deep) -> Some (Deep { deep with f = One y })
  | Deep { m = (lazy q); r; _ } ->
      Some
        (match peek q with
        | None -> Shallow r
        | Some (y, z) ->
            Deep { f = Two (y, z); m = lazy (Option.get @@ pop q); r })
