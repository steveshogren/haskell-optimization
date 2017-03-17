module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified CardsTest as CT

main :: IO ()
main = defaultMain (testGroup "new group" [CT.tests2])
