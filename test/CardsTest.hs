module CardsTest where

import Cards
import Types
import Test.Tasty
import Test.Tasty.HUnit
import Control.Lens

testParseCard1 :: IO ()
testParseCard1 =
  let i = "16 Ability penetration"
      card = parseCard i
  in (assertEqual "ab pen" 8 (card^.abilityPen))

testParseCard2 :: IO ()
testParseCard2 =
  let i = "2.5 Lifesteal|4 Basic penetration|4 Crit Chance|16 Ability penetration|150 Health|5 Lifesteal|60 Mana|11 Attack Speed|0.3 Mana Regen|6 Power|7 Basic armor|1.4 Health Regen"
      card = parseCard i
  in (assertEqual "health" 3 (card^.health))
     >> (assertEqual "ab pen" 8 (card^.abilityPen))

tests2 :: TestTree
tests2 = testGroup "Parsing Tests"
  [
    testCase "parse a card" testParseCard1
    , testCase "parse a bigger card" testParseCard2
  ]
