// tag::foldFunction[]
function fold(f, z, xs) {
  if (xs.length < 1) { return z; }          // <1>
  return fold(f, f(z, xs[0]), xs.slice(1)); // <2>
}
// end::foldFunction[]

// tag::usingFold[]
var strConcat = function (x, accum) { return x + accum; }
var s = fold(strConcat, '', ['this ', 'is ', 'a ', 'string']);
s; // => "this is a string"
// end::usingFold[]

// tag::mapWithFold[]
var myMap = function(f, xs) {
  function step(accum, x) { return [f(x)].concat(accum); }
  return fold(step, [], xs).reverse();
}
// end::mapWithFold[]
