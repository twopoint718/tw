{-# LANGUAGE MagicHash #-}

module Foo where

import GHC.Prim

f :: Int# -> Int# -> Int#
f x y = x +# y

