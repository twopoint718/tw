"use strict";
function None() { return { tag: "none" }; } // <4>
function Some(x) {
    return { tag: "some", value: x };
}
// end::optionDef[]
// tag::lookupWithOption[]
function lookup_retinue_opt(name) {
    var advisors = {
        'Idaho': { name: 'Duncan Idaho', role: 'Swordmaster' },
        'Hawat': { name: 'Thufir Hawat', role: 'Mentat' },
        'Yueh': { name: 'Wellington Yueh', role: 'Suk doctor' },
        'Halleck': { name: 'Gurney Halleck', role: 'Soldier' }
    };
    var result = advisors[name];
    if (result != undefined)
        return Some(result); // <2>
    return None(); // <3>
}
function map(f, opt) {
    switch (opt.tag) {
        case "some": return Some(f(opt.value));
        case "none": return opt;
    }
}
function unwrap(def, opt) {
    switch (opt.tag) {
        case "none": return def;
        case "some": return opt.value;
    }
}
