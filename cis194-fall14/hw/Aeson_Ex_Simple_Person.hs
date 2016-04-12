{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Data.Aeson
import Data.Monoid
import GHC.Generics

data Person = Person {  name :: String
                      , age :: Int }
                deriving (Show, Generic)

instance FromJSON Person

p :: Either String Person
p = eitherDecode "{ \"name\" : \"Richard\", \"age\" : 32 }"
