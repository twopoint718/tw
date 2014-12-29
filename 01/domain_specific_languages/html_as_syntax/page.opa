// tag::page[]
function page() {
    <h1>Hello, World</h1>
    <p>This is just syntax</p>
}
// end::page[]

Server.start(
  Server.http,
  { ~page, title: "Hello, World" }
)
