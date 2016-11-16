type ride =                                                   (* <1> *)
  { name : string;
    distance : float;
    date : string
  }

let parse_formdata formdata =                                 (* <2> *)
  Stringext.split ~on:'&' formdata
  |> List.map (fun kv_pair ->
      match Stringext.cut ~on:"=" kv_pair with
      | Some x -> x
      | None -> ("", ""))

let make_ride params =                                        (* <3> *)
  try
    Some {
      name = List.assoc "name" params;
      distance = List.assoc "distance" params |> float_of_string;
      date = List.assoc "date" params;
    }
  with
  | Not_found -> None

let ride_of_formdata formdata =                               (* <4> *)
  parse_formdata formdata
  |> make_ride

let ride_num_of_formdata formdata =                           (* <5> *)
  parse_formdata formdata
  |> (fun params ->
      try Some (List.assoc "ride_num" params |> int_of_string)
      with Not_found -> None)
