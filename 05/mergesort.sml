(* run with:
*  sml mergesort.sml
*)

(* tag::funcSplit[]
 // Splitting function *)
fun split []        = ([], [])                    (* <1> *)
  | split [h]       = ([h], [])                   (* <2> *)
  | split (x::y::t) = let val (s1, s2) = split t  (* <3> *)
                       in (x::s1, y::s2)          (* <4> *)
                      end
(* end::funcSplit[]
 *)

(* tag::funcMerge[]
 // Merge function *)
fun merge ([], x)          = x:int list           (* <1> *)
  | merge (x, [])          = x                    (* <2> *)
  | merge (h1::t1, h2::t2) =                      (* <3> *)
    if h1 < h2    then h1::merge(    t1, h2::t2)  (* <4> *)
                  else h2::merge(h1::t1,     t2)  (* <5> *)
(* end::funcMerge[]
 *)

(* tag::funcSort1[]
 // Sort function (first try) *)
fun sort_1 []  = []                              (* <1> *)
  | sort_1 x   = let val (p, q) = split x        (* <2> *)
                  in merge (sort_1 p, sort_1 q)  (* <3> *)
                 end
(* end::funcSort1[]
 *)

(* tag::funcSort2[]
 // Sort function (second try) *)
fun sort_2 []  = []
  | sort_2 [x] = [x]                             (* <1> *)
  | sort_2 x   = let val (p, q) = split x
                  in merge (sort_2 p, sort_2 q)
                 end
(* end::funcSort2[]
 *)
