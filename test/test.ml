include DataStructures

module Tester (Q : Heap.PriorityQueue with type elt = int) = struct
  let print_int_list l =
    List.iter
      (fun x ->
        print_int x;
        print_string " ")
      l;
    print_newline ()

  let () = Random.self_init ()
  let l : int list = List.init 50 (fun _ -> Random.int 100000)
  let b = Sys.time ()
  let qsort : int list = Q.sort l
  let () = Printf.printf "Sorting time: %fs\n" (Sys.time () -. b)
  let b = Sys.time ()
  let sorted = List.sort Int.compare l
  let () = Printf.printf "Stdlib Sorting time: %fs\n" (Sys.time () -. b)

  let () =
    if qsort <> sorted then
      let () =
        print_int_list qsort;
        print_int_list sorted
      in
      assert false
end

module _ = Tester (Heap.LeftistTree (Int))
module _ = Tester (Heap.SkewHeap (Int))
module _ = Tester (Heap.PairingHeap (Int))
module _ = Tester (Heap.BinomialHeap (Int))
