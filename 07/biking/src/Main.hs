{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Web.Spock.Safe (SpockT, get, post, root, runSpock, spockT) -- <1>
import Web.Spock.Shared ()

import DB (getConnection)                                         -- <2>
import Types (AppM, formRoute, submitRoute)
import qualified Controllers


main :: IO ()
main = runSpock 8080 (spockT getConnection routes)               -- <3>

routes :: SpockT AppM ()                                         -- <4>
routes = do
  get  root        Controllers.index
  get  formRoute   Controllers.form
  post submitRoute Controllers.submitForm
