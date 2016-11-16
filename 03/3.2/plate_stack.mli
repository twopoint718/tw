(** This is a simle Stack module. Push, pop, and peek are supported. *)

type 'a t
(** This is an abstract stack of plates, no peeking! *)

val empty : unit -> 'a t
(** When all the plates are gone you just have the empty plate elevator.
    Is that what this thing is called, a "plate elevator?" *)

val push : 'a -> 'a t -> unit
(** Put a plate back on the stack. You know, this isn't hygenic. *)

val pop : 'a t -> 'a option
(** Yeah! Get that plate and get in line for food. *)

val peek : 'a t -> 'a option
(** Look at the plate on the top of the stack. Oh no, is that a hair? *)
