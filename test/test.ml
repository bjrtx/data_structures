include DataStructures

module Tester(Q : Heap.Queue with type elt = int) = struct
  let () = Random.self_init ()
  let l : int list = List.init 5000 (fun _ -> Random.int(100000))
  let b = Sys.time()
  let qsort : int list = Q.sort l
  let () = Printf.printf "Sorting time: %fs\n" (Sys.time() -. b)
  let b = Sys.time()
  let _  = List.sort (Int.compare) l
  let () = Printf.printf "Stdlib Sorting time: %fs\n" (Sys.time() -. b)         
  let () = assert (qsort = List.sort (Int.compare) l)   
end

module _ = Tester(Heap.LeftistTree(Int))
module _ = Tester(Heap.SkewHeap(Int))
module _ = Tester(Heap.PairingHeap(Int))
         
