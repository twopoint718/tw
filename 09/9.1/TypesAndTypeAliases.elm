module TypesAndTypeAliases exposing (..)

import List


-- tag::Maybe[]
type Maybe a = Just a | Nothing
-- end::Maybe[]


-- tag::Result[]
type Result error value = Err error | Ok value
-- end::Result[]


-- tag::DifferentTypes[]
type Enemy
  = Dragon                   -- <1>
  | Squid
  | Orc

damage : Enemy -> Int
damage enemy =
  case enemy of              -- <2>
    Dragon -> 20
    Squid -> 15
    Orc -> 6

damages : List Int
damages = List.map damage [Orc, Orc, Dragon]
-- end::DifferentTypes[]
