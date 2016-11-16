open Lwt.Infix

let server_src = Logs.Src.create "biking" ~doc:"HTTP server"
module Server_log = (val Logs.src_log server_src : Logs.LOG)

(* tag::BikingMain[] *)
module Main (Clock:V1.CLOCK) (FS:V1_LWT.KV_RO) (S:Cohttp_lwt.Server)
= struct
(* end::BikingMain[] *)

  module Logs_reporter = Mirage_logs.Make(Clock)


  (* tag::BikingReadFs[] *)
  let read_fs fs name =
    FS.size fs name >>= function                              (* <1> *)
    | `Error (FS.Unknown_key _) ->
      Lwt.fail (Failure ("read " ^ name))
    | `Ok size ->
      FS.read fs name 0 (Int64.to_int size) >>= function
      | `Error (FS.Unknown_key _) ->
        Lwt.fail (Failure ("read " ^ name))
      | `Ok bufs -> Lwt.return (Cstruct.copyv bufs)
  (* end::BikingReadFs[] *)


  (* tag::BikingDispatcher[] *)
  let rec dispatcher fs meth path body =
    Server_log.info (fun f ->
      f "[%s] %s" (Util.string_of_meth meth) path);
    let server = (module S : Cohttp_lwt.Server) in
    match (meth, path) with                                   (* <1> *)
    | ((`GET, "") | (`GET, "/")) ->
        S.respond_string
          ~status:`OK
          ~body:(View.ride_list                               (* <2> *)
                   Model.(store.rides) Model.(store.flash)) ()

    | (`GET, "/form") ->                                      (* <3> *)
        dispatcher fs meth "form.html" body

    | (`POST, "/add") ->
        Controller.(with_postbody server body add)            (* <4> *)

    | (`POST, "/remove") ->
        Controller.(with_postbody server body remove)         (* <4> *)

    | (_, path) ->                                            (* <5> *)
        Lwt.catch (fun () ->
          read_fs fs path >>= fun body ->
          let mime_type = Magic_mime.lookup path in
          let headers =
            Cohttp.Header.init_with "content-type" mime_type in
          Server_log.info (fun f -> f "serving file: '%s'" path);
          S.respond_string ~status:`OK ~body ~headers ()
          )
          (fun _exn -> S.respond_not_found ())
  (* end::BikingDispatcher[] *)


  (* tag::BikingStart[] *)
  let start _clock ro_fs http =                               (* <1> *)
    Logs.(set_level (Some Info));
    Logs_reporter.(create () |> run) @@ fun () ->

    let callback (_,cid) request body =                       (* <2> *)
      let cid = Cohttp.Connection.to_string cid in
      let meth = Cohttp.Request.meth request in
      let path = request |> Cohttp.Request.uri |> Uri.path in
      Server_log.info (fun f -> f "[%s] connect" cid);
      dispatcher ro_fs meth path body in                      (* <3> *)

    let conn_closed (_, cid) =                                (* <4> *)
      let cid = Cohttp.Connection.to_string cid in
      Server_log.info (fun f -> f "[%s] closing" cid) in

    let port = Key_gen.port () in
    Server_log.info (fun f -> f "listening on %d/TCP" port);
    http (`TCP port) (S.make ~conn_closed ~callback ())       (* <5> *)
  (* end::BikingStart[] *)


end
