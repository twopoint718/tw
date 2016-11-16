val with_postbody :
  (module Cohttp_lwt.Server) ->
  [< `Empty | `Stream of string Lwt_stream.t | `String of 'a | `Strings of 'b ] ->
  ((module Cohttp_lwt.Server) ->
   string -> (Cohttp_lwt.Response.t * Cohttp_lwt_body.t) Lwt.t) ->
  (Cohttp_lwt.Response.t * Cohttp_lwt_body.t) Lwt.t
(** Accepts a server module, a raw request body, and a handler. This
    function applies the handler to the string derived from the raw
    request body (if it can be parsed). All handlers should make some
    kind of HTTP response.
*)

val add :
  (module Cohttp_lwt.Server) ->
  string ->
  (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
(** Add a ride to the (in-memory) database *)

val remove :
  (module Cohttp_lwt.Server) ->
  string ->
  (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
(** Remove the referenced ride (in the params) from the (in-memory)
    database.
*)
