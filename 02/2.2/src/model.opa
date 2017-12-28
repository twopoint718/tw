// tag::modelImports[]
import stdlib.core.date
// end::modelImports[]

// tag::modelTypes[]
type ride_id = int                     // <1>

type ride = {                          // <2>
  ride_id id,
  string user_name,
  float distance,
  Date.date date
}
// end::modelTypes[]

// tag::modelDatabase[]
database biking {
  ride /rides[{id}]                    // <1>
  int /index = 0                       // <2>
}
// end::modelDatabase[]

// tag::modelModule[]
module Model {
  function create_ride(string user_name,
                       string dist,
                       string raw_date) {
    distance = Float.of_string(dist)                         // <1>

    scanner = Date.generate_scanner("%Y-%m-%d")
    result  = Date.of_formatted_string(scanner, raw_date)    // <2>
    date = match(result) {                                   // <3>
      case {some: d} : d                                     // <4>
      case {none}    : Date.now()                            // <5>
    }

    int id = /biking/index
    /biking/rides[~{id}] <- ~{id, user_name, distance, date} // <6>
    /biking/index <- id + 1
  }
}
// end::modelModule[]
