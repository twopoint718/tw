let string_of_stream stream =
  Lwt_stream.to_list stream
  |> Lwt_main.run
  |> String.concat ""


(* tag::ControllerWithPostbody[] *)
let with_postbody server body handler =
  let module Server = (val server : Cohttp_lwt.Server) in     (* <1> *)
  let b s = Cohttp_lwt_body.of_string s in
  let err400 = `Bad_request in
  match body with
  | `Stream stream ->
      handler server (string_of_stream stream)                (* <2> *)
  | `Empty ->
      Server.respond ~status:err400 ~body:(b "Empty body") ()
  | `String _s ->
      Server.respond ~status:err400 ~body:(b "Not a stream") ()
  | `Strings _ss ->
      Server.respond ~status:err400 ~body:(b "Multilple strings") ()
(* end::ControllerWithPostbody[] *)


(* tag::ControllerAdd[] *)
let add server formdata =
  let module Server = (val server : Cohttp_lwt.Server) in
  (match Form_handler.ride_of_formdata formdata with          (* <1> *)
   | Some ride ->
       Model.add_ride Model.store ride;                       (* <2> *)
       Model.store.Model.flash <- Some "Ride added";          (* <3> *)
   | None ->
       Model.(store.flash <- Some "Ride couldn't be added"));
  Server.respond_redirect ~uri:(Uri.of_string "/") ()         (* <4> *)
(* end::ControllerAdd[] *)


(* tag::ControllerRemove[] *)
let remove server formdata =
  let module Server = (val server : Cohttp_lwt.Server) in
  (match Form_handler.ride_num_of_formdata formdata with      (* <1> *)
  | None ->
      Model.(store.flash <- Some "No action taken")
  | Some i ->
      Model.(remove_ride store i);
      Model.(store.flash <- Some "Ride removed"));
  Server.respond_redirect ~uri:(Uri.of_string "/") ()
(* end::ControllerRemove[] *)
