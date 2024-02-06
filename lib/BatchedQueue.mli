type !+'a q

val empty : 'a q
val is_empty : 'a q -> bool
val mem : 'a -> 'a q -> bool
val size : 'a q -> int
val rev : 'a q -> 'a q
val push : 'a -> 'a q -> 'a q
val map : ('a -> 'b) -> 'a q -> 'b q
val peek : 'a q -> 'a option
val pop : 'a q -> 'a q option
val to_seq : 'a q -> 'a Seq.t
val to_arbitrary_seq : 'a q -> 'a Seq.t
