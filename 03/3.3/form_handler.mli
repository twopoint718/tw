(** The type of rides that we're storing *)
type ride =
  { name : string;
    distance : float;
    date : string
  }

(** Convert a www-form-urlencoded string into a ride *)
val ride_of_formdata : string -> ride option

(** Convert a www-form-urlencoded string into an optional int *)
val ride_num_of_formdata : string -> int option
