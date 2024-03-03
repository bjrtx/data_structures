module type Deque = DataStructures.Deques.Deque.S

module Tester (D : Deque) = struct
  let open D in
  let to_list d = D.to_seq d |> List.of_seq in
  assert (is_empty empty);
  assert (not (is_empty @@ cons 'a' empty));
  assert (head empty == None);
  assert (head @@ cons 5 @@ cons 6 empty = Some 5);
  assert (tail empty == None);
  assert (
    tail @@ cons 3.1 @@ cons 4.6 @@ cons 7.2 empty
    |> Option.map to_list
    = Some [ 4.6; 7.2 ]);
  assert (last @@ cons 3.1 @@ cons 4.6 @@ cons 7.2 empty = Some 7.2);
  assert (
    init @@ cons 3.1 @@ cons 4.6 @@ cons 7.2 empty
    |> Option.map to_list
    = Some [ 3.1; 4.6 ])
end

module _ = Tester (DataStructures.Deques.ImplicitDeque)

module Bankers2 = DataStructures.Deques.BankersDeque.Make (struct
let c = 2
end)

module _ = Tester (Bankers2)

module _ = Tester (DataStructures.Deques.RealTimeDeque.Make (struct
  let c = 2
end))
module _ = Tester (DataStructures.Deques.SimpleCatenableDeque.Make(Bankers2))