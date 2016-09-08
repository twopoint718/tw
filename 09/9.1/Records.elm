module Records exposing (..)


-- tag::Monster[]
type alias Monster = -- <1>
  { hitPoints : Int
  , attack : String
  , damage : String
  }

squid : Monster
squid =
  { hitPoints = 34   -- <2>
  , attack = "beak"
  , damage = "3d6"
  }
-- end::Monster[]

-- tag::LiteralRecord[]
orc : { hitPoints : Int, attack : String, damage : String}
orc = { hitPoints = 10, attack = "ax", damage = "1d6" }
-- end::LiteralRecord[]

-- tag::Update[]
giantSquid = { squid | hitPoints = 55, damage = "5d6" }
-- end::Update[]

-- tag::Extensible[]
type alias Enemy a =
  { a | name : String, hitPoints : Int }

kobold : Enemy { inventory : List String }
kobold =
  { name = "kobold"
  , hitPoints = 4
  , inventory = ["3 silver", "leather armor"]
  }

calcHitPoints : List { a | hitPoints : Int } -> List Int
calcHitPoints enemies =
  List.map .hitPoints enemies
-- end::Extensible[]
