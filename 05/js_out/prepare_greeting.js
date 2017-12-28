"use strict";
function prepare_greeting_2(name) {
    var greeting;
    if (true) {
        greeting = "Hello, ";
    }
    else {
        greeting = 5;
    }
    return greeting.concat(name, "!");
}
console.log(prepare_greeting_2("Chris"));
