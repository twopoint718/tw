function empty(lst) {
  return lst.length == 0;
}
function append(lst, x) { return lst.concat([x]); }
function tail(lst) { return lst.slice(1); }

// tag::zipWithFunction[]
function zipWith(f, lst1, lst2) {
  function zipHelper(_lst1, _lst2, acc) {        // <1>
    if (empty(_lst1) || empty(_lst2)) {
      return acc;                                // <2>
    }
    return zipHelper(
      tail(_lst1), tail(_lst2),                  // <3>
      append(acc, f(_lst1[0], _lst2[0]))         // <4>
    );
  }
  return zipHelper(lst1, lst2, []);              // <5>
}
// end::zipWithFunction[]

// tag::zipFunction[]
var pair = function(a, b) { return [a, b]; }     // <1>

var zip = function(lst1, lst2) {
  return zipWith(pair, lst1, lst2);              // <2>
};
// end::zipFunction[]

// tag::usingZip[]
var l1 = [1, 2, 3];
var l2 = ['a', 'b', 'c'];
var l3 = zip(l1, l2); // => [[1,'a'], [2,'b'], [3,'c']]
// end::usingZip[]

function toArray(x) {
  return Array.prototype.slice.call(x);
}

// tag::curry[]
function curry(f) {
  var arity = f.length;
  var args = [];
  function curryHelper(x) {                   // <1>
    if (args.length < arity) {
      args = args.concat(toArray(arguments)); // <2>
      return args.length >= arity ?           // <3>
        f.apply(this, args) : curryHelper;    // <4>
    }
    return f.apply(this, args);               // <5>
  }
  return curryHelper;
}
// end::curry[]

// tag::usingCurry[]
var curriedZip = curry(zipWith)(pair);
curriedZip(l1, l2); // => same as before
// end::usingCurry[]

module.exports = {
  curry: curry,
  zip: zip,
  curriedZip: curriedZip
}
