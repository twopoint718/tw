module H = Cow.Html
module F = Form_handler
let (++) = H.(++)

let head_content =
  H.link
    ~rel:"stylesheet"
    ~href:(Uri.of_string "css/normalize.css") H.empty ++
  H.link
    ~rel:"stylesheet"
    ~href:(Uri.of_string "css/skeleton.css") H.empty ++
  H.link
    ~rel:"stylesheet"
    ~href:(Uri.of_string "css/style.css") H.empty ++
  H.link
    ~rel:"icon"
    ~href:(Uri.of_string "/favicon.ico") H.empty ++
  H.script ~src:"js/index.js" H.empty


(* tag::ViewRideTable[] *)
let ride_table rides =
  let the_row (ride, i) =                                     (* <1> *)
    let index = string_of_int i in
    [| H.string ride.F.name;
       H.string (string_of_float ride.F.distance);
       H.string ride.F.date;
       H.tag "a"                                              (* <2> *)
         ~attrs:[
           ("onclick", "removeRide(event)");
           ("href", "#");
           ("data-ride-num", index)]
         (H.string "remove")
    |]
  in
  let table_header =                                          (* <3> *)
    [| H.string "Rider";
       H.string "Distance";
       H.string "Date";
       H.string "Remove?"
    |] in
  let rides_with_index =
    List.combine rides (Util.range 1 (List.length rides)) in  (* <4> *)
  let table_body =
    Array.of_list (List.map the_row rides_with_index) in      (* <5> *)
  H.html_of_table ~headings:true @@
  Array.append [| table_header |] table_body                  (* <6> *)
(* end::ViewRideTable[] *)


(* tag::ViewRideList[] *)
let ride_list rides flash_message =
  let flash = match flash_message with                        (* <1> *)
    | None -> []
    | Some msg ->
        [H.div ~attrs:[("class", "flash")] (H.string msg)]
  in
  H.to_string @@                                              (* <2> *)
  H.list [
    H.head head_content;
    H.body
      (H.div ~cls:"container" (H.list @@
        List.append
          flash                                               (* <3> *)
          [ H.h1 (H.string "Current rides");
            ride_table rides;                                 (* <4> *)
            H.a ~href:(Uri.of_string "/form")
              (H.string "Add ride")
          ])
      )
  ]
(* end::ViewRideList[] *)
