val take : int -> 'a list -> 'a list
(** Return a list with the first i elements of lst *)

val drop : int -> 'a list -> 'a list
(** Return a list with the first i elements of lst removed *)

val remove : int -> 'a list -> 'a list
(** Remove the ith element of lst, example:

    # remove 2 [1; 2; 3];;
    - : int list = [1; 3]
*)

val range : int -> int -> int list
(** Generate a list of numbers from i to j, inclusive

    # range 3 4;;
    - : int list = [3; 4]
*)

val string_of_meth : [> `GET | `POST ] -> string
(** Convert an HTTP request method into a human-readable form *)
