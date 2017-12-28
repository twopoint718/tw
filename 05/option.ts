// tag::optionDef[]
interface None {                                                // <1>     
  tag: "none"
}

interface Some<T> {                                             // <2>
  tag: "some"
  value: T
}

type Option<T> = Some<T> | None                                 // <3>

function None<T>() : Option<T> { return { tag: "none"} }        // <4>
function Some<T>(x:T) : Option<T> {                             // <5>
  return {tag: "some", value: x}
}
// end::optionDef[]

// tag::lookupWithOption[]
function lookup_retinue_opt(name : string) : Option<Advisor> {  // <1>
  let advisors : {[_:string]:Advisor} = {
    'Idaho' : { name: 'Duncan Idaho', role: 'Swordmaster' },
    'Hawat' : { name: 'Thufir Hawat', role: 'Mentat' },
    'Yueh' : { name: 'Wellington Yueh', role: 'Suk doctor' },
    'Halleck' : { name: 'Gurney Halleck', role: 'Soldier' }
  }
  let result = advisors[name]
  if (result != undefined)
    return Some(result)                                         // <2>
  return None()                                                 // <3>
}
// end::lookupWithOption[]
