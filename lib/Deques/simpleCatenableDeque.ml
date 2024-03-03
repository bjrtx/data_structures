module type QS = sig
  include Deque.S

  val size : 'a t -> int
end

module Make (Q : QS) = struct
  type 'a t =
    | Shallow of 'a Q.t
    | Deep of { f : 'a Q.t; m : 'a Q.t t Lazy.t; r : 'a Q.t }

  let empty = Shallow Q.empty
  let is_empty = function Shallow d -> Q.is_empty d | _ -> false

  let cons x = function
    | Shallow d -> Shallow (Q.cons x d)
    | Deep ({ f; _ } as d) -> Deep { d with f = Q.cons x f }

  let head = function Shallow f | Deep { f; _ } -> Q.head f

  let rec tail : 'a. 'a t -> 'a t option = function
    | Shallow d -> Q.tail d |> Option.map (fun x -> Shallow x)
    | Deep { f; m; r } ->
        Some
          (if Q.size f > 2 then Deep { f = Q.tail f |> Option.get; m; r }
           else
             let (lazy ms) = m in
             if is_empty ms then Shallow r
             else
               Deep
                 {
                   f = Q.cons (Q.last f |> Option.get) (head ms |> Option.get);
                   m = lazy (tail ms |> Option.get);
                   r;
                 })

  let snoc x = function
    | Shallow d -> Shallow (Q.snoc x d)
    | Deep ({ r; _ } as d) -> Deep { d with r = Q.cons x r }

  let last = function Shallow f -> Q.last f | Deep { r; _ } -> Q.head r

  let rec init : 'a. 'a t -> 'a t option = function
    | Shallow d -> Q.init d |> Option.map (fun x -> Shallow x)
    | Deep { f; m; r } ->
        Some
          (if Q.size r > 2 then Deep { r = Q.init r |> Option.get; m; f }
           else
             let (lazy ms) = m in
             if is_empty ms then Shallow f
             else
               Deep
                 {
                   r = Q.cons (Q.last r |> Option.get) (last ms |> Option.get);
                   m = lazy (init ms |> Option.get);
                   f;
                 })

  let short_append_l a b =
    if Q.is_empty a then b else Q.cons (Q.head a |> Option.get) b

  let short_append_r a b =
    if Q.is_empty b then a else Q.snoc (Q.last b |> Option.get) a

  let rec append : 'a. 'a t -> 'a t -> 'a t =
   fun s t ->
    match (s, t) with
    | Shallow f, Shallow r ->
        if Q.size f < 2 then Shallow (short_append_l f r)
        else if Q.size r < 2 then Shallow (short_append_r f r)
        else Deep { f; m = lazy empty; r }
    | Shallow d, Deep { f; m; r } ->
        if Q.size d < 2 then Deep { f = short_append_l d f; m; r }
        else Deep { f = d; m = Lazy.map (cons f) m; r }
    | Deep { f; m; r }, Shallow d ->
        if Q.size d < 2 then Deep { r = short_append_r r d; m; f }
        else Deep { f; m = Lazy.map (snoc r) m; r = d }
    | Deep { f; m = m1; r = r1 }, Deep { f = f2; m = m2; r } ->
        Deep
          {
            f;
            m =
              lazy (append (snoc r1 (Lazy.force m1)) (cons f2 (Lazy.force m2)));
            r;
          }

  let rec to_seq q =
    match head q with
    | Some x -> Seq.(cons x (tail q |> Option.get |> to_seq))
    | None -> Seq.empty
end
