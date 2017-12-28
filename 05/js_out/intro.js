"use strict";
function compose(f, g) {
    return function (x) { return f(g(x)); };
}
var add1 = function (x) { return x + 1; };
var times2 = function (x) { return x * 2; };
var exclaim = function (x) { return x + '!'; };
console.log(compose(times2, add1)(1));
