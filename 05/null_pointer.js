function lookup_retinue(name) {                                   // <1>
    var advisors = {
        'Idaho': { name: 'Duncan Idaho', role: 'Swordmaster' }, 
        'Hawat': { name: 'Thufir Hawat', role: 'Mentat' },
        'Yueh': { name: 'Wellington Yueh', role: 'Suk doctor' },
        'Halleck': { name: 'Gurney Halleck', role: 'Soldier' }
    };
    return advisors[name];                                        // <2>
}
var s = lookup_retinue('Bob').role;                               // <3>
console.log(s);
