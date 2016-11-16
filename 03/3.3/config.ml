open Mirage


let data = generic_kv_ro "htdocs"                             (* <1> *)
let stack = generic_stackv4 default_console tap0              (* <2> *)
let http_srv = http_server (conduit_direct ~tls:false stack)  (* <3> *)

let port =                                                    (* <4> *)
  let doc = Key.Arg.info ~doc:"Listening port." ["port"] in
  Key.(create "port" Arg.(opt ~stage:`Both int 80 doc))

let main =
  let libraries = [ "cow"; "magic-mime"; "mirage-logs" ] in
  let packages = [ "cow"; "magic-mime"; "mirage-logs" ] in
  let keys = [ Key.abstract port ] in
  foreign                                                     (* <5> *)
    ~libraries ~packages ~keys
    "Biking.Main" (clock @-> kv_ro @-> http @-> job)

let () =
  register "biking" [main $ default_clock $ data $ http_srv]  (* <6> *)
