module Signals where

import Signal exposing (Signal)


-- tag::[]
-- end::[]

interruptingCow : Signal String
interruptingCow = Signal.constant "moo"
