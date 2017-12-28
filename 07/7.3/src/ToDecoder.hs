{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module ToDecoder where


import           Data.Char
import           Data.Proxy
import qualified Data.Text    as Text
import           Data.Time
import           GHC.Generics
import           Text.Printf


--------------------------------------------------------------------------------
-- TYPECLASSES


class ToDecoder a where
    toDecoder :: a -> String

    default toDecoder :: (Generic a, GToDecoder (Rep a)) => a -> String
    toDecoder _ = g_ToDecoder (Proxy :: Proxy a)


g_ToDecoder :: forall a. (Generic a, GToDecoder (Rep a)) => Proxy a -> String
g_ToDecoder _ = gToDecoder (Proxy :: Proxy (Rep a))


class GToDecoder a where
    gToDecoder :: Proxy a -> String


--------------------------------------------------------------------------------
-- TYPECLASS INSTANCES


-- ToDecoder


instance ToDecoder a => ToDecoder (Maybe a) where
    toDecoder _ = toDecoder (undefined :: a)


instance ToDecoder String where
    toDecoder _ = "string"


instance ToDecoder Day where
    toDecoder _ = "date"


instance ToDecoder Double where
    toDecoder _ = "float"


instance ToDecoder Text.Text where
    toDecoder _ = "string"


-- GToDecoder

-- Datatype
instance GToDecoder f => GToDecoder (M1 D x f) where
    gToDecoder _ = gToDecoder (Proxy :: Proxy f)


-- Constructor Metadata
instance (GToDecoder f, Constructor c) => GToDecoder (M1 C c f) where
    gToDecoder _
        | conIsRecord m = printf
            "%s : Decoder %s\n%s = succeed %s\n    <*> field \"id\" int\n    <*> %s\n"
            lc uc lc uc
            (gToDecoder (Proxy :: Proxy f))
        | otherwise = ""
        where
            uc = conName m
            lc = lowerCase uc
            m = undefined :: t c f a


-- Selector Metadata
instance (GToDecoder f, Selector c) => GToDecoder (M1 S c f) where
    gToDecoder _ = printf "field \"%s\" %s"
        selName' (gToDecoder (Proxy :: Proxy f))
        where
            m = undefined :: t c f a
            selName' = unCamel (selName m)



-- Constructor Paramater (Rec0)
instance (ToDecoder f) => GToDecoder (K1 R f) where
    gToDecoder _ = toDecoder (undefined :: f)


-- Sum branch
instance (GToDecoder a, GToDecoder b) => GToDecoder (a :+: b) where
    gToDecoder _ = gToDecoder (Proxy :: Proxy a) ++ gToDecoder (Proxy :: Proxy b)


-- Product branch
instance (GToDecoder a, GToDecoder b) => GToDecoder (a :*: b) where
    gToDecoder _ = printf "%s\n    <*> %s"
        (gToDecoder (Proxy :: Proxy a))
        (gToDecoder (Proxy :: Proxy b))


-- Void branch
instance GToDecoder U1 where
    gToDecoder _ = " UNIT "


--------------------------------------------------------------------------------
-- HELPER FUNCTIONS


lowerCase :: String -> String
lowerCase []     = []
lowerCase (c:cs) = toLower c : cs


-- camelCase -> case
unCamel :: String -> String
unCamel s = lowerCase (dropWhile isLower s)
