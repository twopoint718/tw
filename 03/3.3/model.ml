module F = Form_handler

(* Semi-permanent (in-memory) storage between requests *)
type store =                                                  (* <1> *)
  { mutable rides : F.ride list;
    mutable flash : string option
  }

let store =                                                   (* <2> *)
  { rides =
      [ {F.name = "Chris"; F.distance = 35.0; F.date = "2016-07-11" };
        {F.name = "Sarah"; F.distance = 35.0; F.date = "2016-07-11" };
        {F.name = "Ralph"; F.distance = 5.2; F.date = "2016-08-11" }
      ];
    flash = None
  }

let add_ride store ride =                                     (* <3> *)
  store.rides <- ride :: store.rides

let remove_ride store index =                                 (* <4> *)
  store.rides <- Util.remove index store.rides
