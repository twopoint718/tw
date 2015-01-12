// tag::controllerModule[]
module Controller {
  dispatcher = {
    parser {
    case (.*) : View.default_page()                   // <1>
    }
  }
}
// end::controllerModule[]

// tag::controllerServer[]
resources = @static_resource_directory("resources")   // <1>

Server.start(Server.http, [
  { register:
    [ { doctype: { html5 } },
      { js: [ ] },
      { css: [ "/resources/css/style.css" ] }         // <2>
    ]
  },
  { ~resources },                                     // <3>
  { custom: Controller.dispatcher }                   // <4>
])
// end::controllerServer[]
