{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module ToElm where


import           Data.Char
import           Data.Proxy
import qualified Data.Text    as Text
import           Data.Time
import           GHC.Generics
import           Text.Printf


--------------------------------------------------------------------------------
-- TYPECLASSES


-- tag::ToElmToElm[]
class ToElm a where
    toElm :: a -> String                              -- <1>

    default toElm :: (Generic a, GToElm (Rep a)) => a -> String
    toElm _ = g_ToElm (Proxy :: Proxy a)              -- <2>


g_ToElm :: forall a. (Generic a, GToElm (Rep a)) => Proxy a -> String
g_ToElm _ = gToElm (Proxy :: Proxy (Rep a))           -- <3>


class GToElm a where
    gToElm :: Proxy a -> String                       -- <4>
-- end::ToElmToElm[]


--------------------------------------------------------------------------------
-- TYPECLASS INSTANCES


-- ToElm


instance ToElm a => ToElm (Maybe a) where
    toElm _ = toElm (undefined :: a)


-- tag::ToElmSimpleInstances[]
instance ToElm String where
    toElm _ = "String"                                -- <1>


instance ToElm Day where
    toElm _ = "Date"                                  -- <2>


instance ToElm Double where
    toElm _ = "Float"                                 -- <3>


instance ToElm Text.Text where
    toElm _ = "String"                                -- <4>
-- end::ToElmSimpleInstances[]

-- GToElm

-- Datatype
instance GToElm f => GToElm (M1 D x f) where
    gToElm _ = gToElm (Proxy :: Proxy f)


-- tag::ToElmConstructorMetadata[]
instance (GToElm f, Constructor c) => GToElm (M1 C c f) where
    gToElm _
        | conIsRecord m = printf                                  -- <1>
            "type alias %s =\n    { id : Int\n    , %s\n    }\n"
            (conName m) (gToElm (Proxy :: Proxy f))
        | otherwise = ""
        where m = undefined :: t c f a
-- end::ToElmConstructorMetadata[]


-- Selector Metadata
instance (GToElm f, Selector c) => GToElm (M1 S c f) where
    gToElm _ = selName' ++ " : " ++ gToElm (Proxy :: Proxy f)
        where
            m = undefined :: t c f a
            selName' = unCamel (selName m)



-- Constructor Paramater (Rec0)
instance (ToElm f) => GToElm (K1 R f) where
    gToElm _ = toElm (undefined :: f)


-- Sum branch
instance (GToElm a, GToElm b) => GToElm (a :+: b) where
    gToElm _ = gToElm (Proxy :: Proxy a) ++ gToElm (Proxy :: Proxy b)


-- Product branch
instance (GToElm a, GToElm b) => GToElm (a :*: b) where
    gToElm _ =
        gToElm (Proxy :: Proxy a) ++ "\n    , " ++ gToElm (Proxy :: Proxy b)


-- Void branch
instance GToElm U1 where
    gToElm _ = " UNIT "


--------------------------------------------------------------------------------
-- HELPER FUNCTIONS


lowerCase :: String -> String
lowerCase []     = []
lowerCase (c:cs) = toLower c : cs


-- camelCase -> case
unCamel :: String -> String
unCamel s = lowerCase (dropWhile isLower s)
