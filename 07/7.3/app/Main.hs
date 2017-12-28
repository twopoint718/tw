{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where


import           Database.Persist.Postgresql
import           Network.Wai.Handler.Warp    (run)
import           Safe                        (readMay)
import           System.Environment          (getArgs, lookupEnv)
import           Text.Printf                 (printf)

import           Config
import           Ride                        (Ride, doMigrations)
import           ToDecoder                   (toDecoder)
import           ToElm                       (toElm)
import           Web                         (app)


-- tag::MainGenerateElm[]
generateElmModule :: IO ()
generateElmModule = do
    tmpl <- readFile "templates/module.tmpl"
    let rideProxy = undefined :: Ride
        fileContent =
            printf tmpl (toElm rideProxy) (toDecoder rideProxy) -- <1>
    writeFile "elm/Ride.elm" fileContent
-- end::MainGenerateElm[]


lookupSetting :: Read a => String -> a -> IO a
lookupSetting env defaultSetting = do
    maybeVal <- lookupEnv env
    case maybeVal of
        Nothing -> return defaultSetting
        Just str ->
            maybe (failMessage str) return (readMay str)
  where
    failMessage str =
        error (printf "Failed to read '%s' from ENV: '%s'" str env)

-- tag::MainMain[]
main :: IO ()
main = do
    args <- getArgs
    case args of                                      -- <1>
        ["gen"] -> generateElmModule                  -- <2>
        _ -> do                                       -- <3>
            env <- lookupSetting "ENV" Development
            port <- lookupSetting "PORT" 8080
            pool <- makePool env
            let cfg = Config
                    { configPool = pool
                    , configEnv = env
                    , configLogEnv = logLevel env
                    }
            runSqlPool doMigrations pool
            app' <- app cfg
            putStrLn $ "Visit: http://localhost:" ++ show port
            run port $ (setLogger env) app'           -- <4>
-- end::MainMain[]
