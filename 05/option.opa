// tag::typeFruit[]
type fruit = {apple} or {banana} or {cherry}
// end::typeFruit[]

// tag::funcPrepare[]
function prepare(fruit f) {
  match(f) {
    case {apple}: "chop"
    case {banana}: "peel"
    case {cherry}: "remove pit from"
  }
}
// end::funcPrepare[]

function fruit_to_string(fruit f) {
  match(f) {
    case {apple}: "apple"
    case {banana}: "banana"
    case {cherry}: "cherry"
  }
}

// tag::unsafeDiv[]
function div(int n, int d) {
  n / d
}

div(1, 0) // Error: Exception : Division by zero
// end::unsafeDiv[]

function fruit_smoothie(fruit a) {
  "{prepare(a)} {fruit_to_string(a)}"
}

// tag::safeDiv[]
function div(int n, int d) {
  if(d == 0) {
    {none}
  } else {
    some(n / d)
  }
}
// end::safeDiv[]

println("{div(1, 0)}")

println(fruit_smoothie({banana}))
