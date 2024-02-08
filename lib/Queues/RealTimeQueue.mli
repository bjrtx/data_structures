(** Real-time queues based on scheduling after Okasaki.
    

Section 4.2 in Chris Okasaki's PhD thesis. Worst-case constant-time operations. Quoting from the thesis:

{i Hint to Practitioners: These queues are not particularly fast when used ephemerally, because
of overheads associated with memoizing values that are never looked at again, but are the fastest
known real-time implementation when used persistently.}
*)

include Queue.S
