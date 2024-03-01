include DataStructures

let print_int_list l =
  List.iter
    (fun x ->
      print_int x;
      print_string " ")
    l;
  print_newline ()

module Tester (Q : Heaps.Heap.PriorityQueue with type elt = int) = struct
  let () = Random.self_init ()
  let l : int list = List.init 100000 (fun _ -> Random.int 1073741823)
  let b = Sys.time ()
  let qsort : int list = Q.sort l
  let first = Sys.time () -. b
  let () = Printf.printf "Sorting time: %fs\n" first
  let b = Sys.time ()
  let sorted = List.sort Int.compare l
  let second = Sys.time () -. b
  let () = Printf.printf "Stdlib Sorting time: %fs\n" second
  let () = Printf.printf "Ratio: %f\n" (first /. second)

  let () =
    if qsort <> sorted then
      let () =
        print_int_list qsort;
        print_int_list sorted
      in
      assert false
end

module _ = Tester (Heaps.LeftistTree.Make (Int))
module _ = Tester (Heaps.Heap.SkewHeap (Int))
module _ = Tester (Heaps.Heap.PairingHeap (Int))
module _ = Tester (Heaps.Heap.BinomialHeap (Int))

module BUMSTester (Q : BottomUpMergeSort.Sortable with type elt = int) = struct
  let () = Random.self_init ()
  let l = List.init 50000 (fun _ -> Random.int 230)
  let b = Sys.time ()
  let qsort = List.fold_left (fun acc x -> Q.add x acc) Q.empty l |> Q.sort
  let first = Sys.time () -. b
  let () = Printf.printf "Sorting time: %fs\n" first
  let b = Sys.time ()
  let sorted = List.sort Int.compare l
  let second = Sys.time () -. b
  let () = Printf.printf "Stdlib Sorting time: %fs\n" second
  let () = Printf.printf "Ratio: %f\n" (first /. second)

  let () =
    if qsort <> sorted then
      let () =
        print_int_list qsort;
        print_int_list sorted
      in
      assert false
end

module _ = BUMSTester (BottomUpMergeSort.Make (Int))
module _ = BUMSTester (ScheduleBottomUpMergeSort.Make (Int))
