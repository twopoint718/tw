val take           : int -> 'a list -> 'a list
val drop           : int -> 'a list -> 'a list
val remove         : int -> 'a list -> 'a list

val range          : int -> int -> int list

val string_of_meth : [> `GET | `POST ] -> string
