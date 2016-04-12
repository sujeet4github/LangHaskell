{-# START_FILE Aeson_Ex_Pizza.hs #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Aeson_Examples where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- from: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
--

-- | Type of each JSON entry in record syntax.
data Person =
  Person { firstName  :: T.Text
         , lastName   :: T.Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show,Generic)

-- Instances to convert our type to/from JSON.
instance FromJSON Person
instance ToJSON Person

{- from a local file -}
jsonFile :: FilePath
jsonFile = "pizza.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

{- from a URL
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

jsonURL :: String
jsonURL = "http://daniel-diaz.github.io/misc/pizza.json"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL
-}


main :: IO ()
main = do
 -- Get JSON data and decode it
 d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> putStrLn err
  Right ps -> print ps
