open Lwt.Infix
open Mirage_clock

module Util = Biking_util

let server_src = Logs.Src.create "biking" ~doc:"HTTP server"
module Server_log = (val Logs.src_log server_src : Logs.LOG)

(* tag::BikingMain[] *)
module Main (PClock:PCLOCK) (FS:Mirage_kv_lwt.RO) (S:Cohttp_lwt.S.Server)
= struct
(* end::BikingMain[] *)

  module Logs_reporter = Mirage_logs.Make(PClock)
  let failf fmt = Fmt.kstrf Lwt.fail_with fmt

  (* tag::BikingReadFs[] *)
  let read_fs fs name =
    FS.size fs name >>= function                              (* <1> *)
    | Error err -> failf "Size: %a" FS.pp_error err
    | Ok size ->
       FS.read fs name Int64.zero size >>= function           (* <2> *)
       | Error err -> failf "Read: %a" FS.pp_error err
       | Ok bufs -> Lwt.return (Cstruct.copyv bufs)           (* <3> *)
  (* end::BikingReadFs[] *)


  (* tag::BikingDispatcher[] *)
  let rec dispatch fs meth path body =
    Server_log.info (fun f ->
      f "[%s] %s" (Util.string_of_meth meth) path);
    let server = (module S : Cohttp_lwt.S.Server) in
    match (meth, path) with                                   (* <1> *)
    | ((`GET, "") | (`GET, "/")) ->
        S.respond_string
          ~status:`OK
          ~body:(View.ride_list                               (* <2> *)
                   Model.(store.rides) Model.(store.flash)) ()

    | (`GET, "/form") ->                                      (* <3> *)
        dispatch fs meth "form.html" body

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
  let start clock ro_fs http =                                (* <1> *)
    Logs.(set_level (Some Info));
    Logs_reporter.(create clock |> run) @@ fun () ->

    let callback (_,cid) request body =                       (* <2> *)
      let cid = Cohttp.Connection.to_string cid in
      let meth = Cohttp.Request.meth request in
      let path = request |> Cohttp.Request.uri |> Uri.path in
      Server_log.info (fun f -> f "[%s] connect" cid);
      dispatch ro_fs meth path body in                        (* <3> *)

    let conn_closed (_, cid) =                                (* <4> *)
      let cid = Cohttp.Connection.to_string cid in
      Server_log.info (fun f -> f "[%s] closing" cid) in

    let port = Key_gen.port () in
    Server_log.info (fun f -> f "listening on %d/TCP" port);
    http (`TCP port) (S.make ~conn_closed ~callback ())       (* <5> *)
  (* end::BikingStart[] *)


end
