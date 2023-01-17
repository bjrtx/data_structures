type 'a t = Empty | Node of 'a * 'a t * 'a t
val empty: 'a t
val mem: 'a t -> 'a -> bool
val to_seq: 'a t -> 'a Seq.t
val map: ('a -> 'b) -> 'a t -> 'b t
val preorder : 'a t -> 'a Seq.t
val postorder : 'a t -> 'a Seq.t
val inorder : 'a t -> 'a Seq.t
val fold : ('a -> 'b -> 'b -> 'b) -> 'b -> 'a t -> 'b
val exists: ('a -> bool) -> 'a t -> bool
val leaf: 'a -> 'a t
val node_func : ('a -> 'a t -> 'a t -> 'b) -> 'a t -> 'b option
val height : 'a t -> int
val breadth : 'a t -> int
val size : 'a t -> int
