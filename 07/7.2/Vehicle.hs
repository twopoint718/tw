module Vehicle where

data Category = Car | Truck | Motorcycle | RV | Other -- <1>
  deriving Show                                       -- <2>

data Emissions = Exempt | MPG Float                   -- <3>
  deriving Show

data License = License                                -- <4>
  { plate :: String
  , category :: Category
  , emissions :: Emissions
  } deriving Show

newCar :: License
newCar = License                                      -- <5>
  { plate = "ABC-123"
  , category = Car
  , emissions = MPG 35.2
  }
