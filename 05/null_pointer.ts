type Advisor = { name: string, role: string }                    // <1>

function lookup_retinue(name : string) : Advisor | undefined {   // <2>
  let advisors : {[_:string]:Advisor} = {
    'Idaho' : { name: 'Duncan Idaho', role: 'Swordmaster' },
    'Hawat' : { name: 'Thufir Hawat', role: 'Mentat' },
    'Yueh' : { name: 'Wellington Yueh', role: 'Suk doctor' },
    'Halleck' : { name: 'Gurney Halleck', role: 'Soldier' }
  }
  return advisors[name]
}

let s : string = lookup_retinue('Bob').role                      // <3>
let t : string = lookup_retinue('Idaho').role                    // <4>
console.log(s, t)
