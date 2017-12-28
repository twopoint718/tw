"use strict";
function lookup_retinue(name) {
    var advisors = {
        'Idaho': { name: 'Duncan Idaho', role: 'Swordmaster' },
        'Hawat': { name: 'Thufir Hawat', role: 'Mentat' },
        'Yueh': { name: 'Wellington Yueh', role: 'Suk doctor' },
        'Halleck': { name: 'Gurney Halleck', role: 'Soldier' }
    };
    return advisors[name];
}
var s = lookup_retinue('Bob').role; // <3>
var t = lookup_retinue('Idaho').role; // <4>
console.log(s, t);
