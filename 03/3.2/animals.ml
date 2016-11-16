module type ANIMAL = sig                     (* <1> *)
  val name : string
  (** The name of the animal *)

  val speak : string
  (** The sound the animal makes *)
end

module Hawk : ANIMAL = struct                (* <2> *)
  let name = "Hawk"
  let speak = "Screeech!"
end

module Dog : ANIMAL = struct                 (* <2> *)
  let name = "Dog"
  let speak = "Woof!"
end

module SeeNSay (Animal:ANIMAL) = struct      (* <3> *)
  let pull_cord () =
    print_endline ("The " ^ Animal.name ^ " says " ^ Animal.speak)
end

module H = SeeNSay(Hawk)                     (* <4> *)
module D = SeeNSay(Dog)

let main =
  H.pull_cord ();                            (* <5> *)
  D.pull_cord ()
