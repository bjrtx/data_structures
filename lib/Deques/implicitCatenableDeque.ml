module type QS = sig
  include Deque.S

  val size : 'a t -> int
end

module Make (Q : QS) = struct
  (* invariants: |f|, |r| >= 3 and |m| >= 2 *)
  type 'a t =
    | Shallow of 'a Q.t
    | Deep of {
        f : 'a Q.t;
        a : 'a cmpdelem t Lazy.t;
        m : 'a Q.t;
        b : 'a cmpdelem t Lazy.t;
        r : 'a Q.t;
      }

  (* invariants: queues have >= 2 elements *)
  and 'a cmpdelem =
    | Simple of 'a Q.t
    | CE of { f : 'a Q.t; a : 'a cmpdelem t Lazy.t; r : 'a Q.t }

  let empty = Shallow Q.empty
  let is_empty = function Shallow d -> Q.is_empty d | _ -> false

  let cons x = function
    | Shallow d -> Shallow (Q.cons x d)
    | Deep ({ f; _ } as d) -> Deep { d with f = Q.cons x f }

  let head = function Shallow f | Deep { f; _ } -> Q.head f

  let snoc x = function
    | Shallow d -> Shallow (Q.snoc x d)
    | Deep ({ r; _ } as d) -> Deep { d with r = Q.snoc x r }

  let last = function Shallow r | Deep { r; _ } -> Q.last r

  let unsnoc q =
    if Q.is_empty q then None
    else Some (Q.init q |> Option.get, Q.last q |> Option.get)

  let uncons q =
    if Q.is_empty q then None
    else Some (Q.head q |> Option.get, Q.tail q |> Option.get)

  let share (f : _ Q.t) (r : _ Q.t) =
    match (unsnoc f, uncons r) with
    | Some (init, lst), Some (hd, tl) ->
        (init, Q.(cons lst @@ cons hd empty), tl)
    | _ -> failwith "Parameters should not be empty"

  let rec short_append_l a b =
    match unsnoc a with
    | None -> b
    | Some (init, last) -> short_append_l init (Q.cons last b)

  let rec short_append_r a b =
    match uncons b with
    | None -> a
    | Some (head, tail) -> short_append_r (Q.snoc head a) tail

  let append : 'a. 'a t -> 'a t -> 'a t =
   fun s t ->
    match (s, t) with
    | Shallow f, Shallow r ->
        if Q.size f < 4 then Shallow (short_append_l f r)
        else if Q.size r < 4 then Shallow (short_append_r f r)
        else
          let f, m, r = share f r in
          Deep { f; a = lazy empty; m; b = lazy empty; r }
    | Shallow d, Deep ({ f; a; _ } as deep) ->
        if Q.size d < 3 then Deep { deep with f = short_append_l d f }
        else Deep { deep with f = d; a = Lazy.map (cons (Simple f)) a }
    | Deep ({ b; r; _ } as deep), Shallow d ->
        if Q.size d < 3 then Deep { deep with r = short_append_r r d }
        else Deep { deep with b = Lazy.map (snoc @@ Simple r) b; r = d }
    | ( Deep { f; a = a1; m = m1; b = b1; r = r1 },
        Deep { f = f2; a = a2; m = m2; b = b2; r } ) ->
        let rr, m, ff = share r1 f2 in
        let aa = Lazy.map (snoc (CE { f = m1; a = b1; r = rr })) a1 in
        let bb = Lazy.map (cons (CE { f = ff; a = a2; r = m2 })) b2 in
        Deep { f; a = aa; m; b = bb; r }

  let replace_head x = function
    | Shallow d -> Shallow Q.(cons x (tail d |> Option.get))
    | Deep d -> Deep { d with f = Q.(cons x (tail d.f |> Option.get)) }

  let replace_last x = function
    | Shallow d -> Shallow Q.(snoc x (init d |> Option.get))
    | Deep d -> Deep { d with r = Q.(snoc x (init d.r |> Option.get)) }

  let rec tail : 'a. 'a t -> 'a t option =
    let tail_exn x = tail x |> Option.get in
    function
    | Shallow d -> Q.tail d |> Option.map (fun x -> Shallow x)
    | Deep ({ f; a; m; b; r } as deep) ->
        let ftail = Q.tail f |> Option.get in
        Some
          (if Q.size f > 3 then Deep { deep with f = ftail }
           else
             match (Lazy.map head a, Lazy.map head b) with
             | (lazy (Some (Simple d))), _ ->
                 Deep
                   {
                     deep with
                     f = short_append_l ftail d;
                     a = Lazy.map tail_exn a;
                   }
             | (lazy (Some (CE { f = ff; a = aa; r = rr }))), _ ->
                 Deep
                   {
                     f = short_append_l ftail ff;
                     a =
                       lazy
                         (append (Lazy.force aa)
                            (replace_head (Simple rr) (Lazy.force a)));
                     m;
                     b;
                     r;
                   }
             | (lazy None), (lazy (Some (Simple d))) ->
                 Deep
                   {
                     f = short_append_l ftail m;
                     a = lazy empty;
                     m = d;
                     b = Lazy.map tail_exn b;
                     r;
                   }
             | (lazy None), (lazy (Some (CE { f = ff; a = aa; r = rr }))) ->
                 Deep
                   {
                     f = short_append_l ftail m;
                     a = Lazy.map (cons (Simple ff)) aa;
                     m = rr;
                     b = Lazy.map tail_exn b;
                     r;
                   }
             | (lazy None), (lazy None) ->
                 append (Shallow (short_append_l ftail m)) (Shallow r))

  let rec init : 'a. 'a t -> 'a t option =
    let init_exn x = init x |> Option.get in
    function
    | Shallow d -> Q.init d |> Option.map (fun x -> Shallow x)
    | Deep ({ f; a; m; b; r } as deep) ->
        let rinit = Q.init r |> Option.get in
        Some
          (if Q.size r > 3 then Deep { deep with r = rinit }
           else
             match (Lazy.map last b, Lazy.map last a) with
             | (lazy (Some (Simple d))), _ ->
                 Deep
                   {
                     deep with
                     r = short_append_r d rinit;
                     b = Lazy.map init_exn b;
                   }
             | (lazy (Some (CE { f = ff; a = aa; r = rr }))), _ ->
                 Deep
                   {
                     r = short_append_r rr rinit;
                     b =
                       lazy
                         (append
                            (replace_last (Simple ff) (Lazy.force a))
                            (Lazy.force aa));
                     m;
                     a;
                     f;
                   }
             | (lazy None), (lazy (Some (Simple d))) ->
                 Deep
                   {
                     r = short_append_r m rinit;
                     b = lazy empty;
                     m = d;
                     a = Lazy.map init_exn a;
                     f;
                   }
             | (lazy None), (lazy (Some (CE { f = ff; a = aa; r = rr }))) ->
                 Deep
                   {
                     r = short_append_r m rinit;
                     b = Lazy.map (snoc (Simple rr)) aa;
                     m = ff;
                     a = Lazy.map init_exn a;
                     f;
                   }
             | (lazy None), (lazy None) ->
                 append (Shallow f) (Shallow (short_append_r m rinit)))

  let rec to_seq q =
    match head q with
    | Some x -> Seq.(cons x (tail q |> Option.get |> to_seq))
    | None -> Seq.empty
end
