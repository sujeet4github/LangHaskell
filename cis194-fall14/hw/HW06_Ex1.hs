{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module HW06_Ex1 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

{-
You will need two packages that are not part of Haskell’s standard
library for this assignment. They are aeson and text. You can install
these with cabal update; cabal install aeson text
-}
