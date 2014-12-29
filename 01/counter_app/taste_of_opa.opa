import stdlib.themes.bootstrap

database count {
  int /value                                          // <1>
}

function void count_up(_) {
  /count/value++                                      // <2>
  #counter = /count/value                             // <3>
}

function void count_down(_) {
  /count/value--                                      // <2>
  #counter = /count/value                             // <3>
}

function page() {                                     // <4>
  <h1 class=jumbotron>Counting</h1>
  <h2 class=smaller id=counter>{/count/value}</h2>

  <button onclick={count_up}>Up</button>
  <button onclick={count_down}>Down</button>
}

Server.start(
  Server.http,
  { title: "Counter"
  , page: page
  }
)
