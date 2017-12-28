// composition.js

function bar(x)  { return x + 1; }
function baz(x)  { return x * 2; }
function quux(x) { return x + 3; }

// tag::first[]
function foo1(input) {
    var x, y, z;

    x = bar(input);
    y = baz(x);
    z = quux(y);

    return z;
}
// end::first[]

// tag::second[]
function foo2(input, f, g, h) {
    var x, y, z;

    x = f(input); y = g(x); z = h(y);

    return z;
}
// end::second[]

// tag::third[]
function foo3(input, f, g, h) {
    return h(g(f(input)));
}
// end::third[]

// tag::fourth[]
function compose(f, g) {
  return function(x) { return f(g(x)); };
}

function chain(funcs) {
  if (funcs.length === 1) {
    return funcs[0];
  }
  return compose(chain(funcs.slice(1)), funcs[0]); // <1>
}

var foo4 = chain([bar, baz, quux]);
// end::fourth[]

module.exports = {
  // supporting
  bar: bar,
  baz: baz,
  quux: quux,

  // main examples
  foo1: foo1,
  foo2: foo2,
  foo3: foo3,
  foo4: foo4
};
