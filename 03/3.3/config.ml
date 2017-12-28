open Mirage

let data = generic_kv_ro "htdocs"                             (* <1> *)
let stack = generic_stackv4 default_network                   (* <2> *)
let http_srv = http_server (conduit_direct ~tls:false stack)  (* <3> *)

let port =                                                    (* <4> *)
  let doc = Key.Arg.info ~doc:"Listening port." ["port"] in
  Key.(create "port" Arg.(opt ~stage:`Both int 80 doc))

let main =
  let packages = List.map Mirage.package
    [ "cohttp";
      "cow";
      "magic-mime";
      "mirage-kv-lwt";
      "mirage-kv";
      "mirage-logs"
    ] in
  let keys = [ Key.abstract port ] in
  foreign                                                     (* <5> *)
    ~packages ~keys
    "Biking.Main" (pclock @-> kv_ro @-> http @-> job)

let () =
  register "biking"
    [main $ default_posix_clock $ data $ http_srv]            (* <6> *)
