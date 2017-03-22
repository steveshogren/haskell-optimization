module CardsTest where

import Cards
import qualified Optimization as OP
import qualified DamagePen as DP
import Types
import Test.Tasty
import Test.Tasty.HUnit
import Control.Lens

testOptimization :: IO ()
testOptimization = do
  let build = DP.maxDps False True False 0 "sparrow" 0 31.5
  cards <- OP.optimize build
  (assertEqual ("card count: " ++ show cards)  6 (length cards))

testWards :: IO ()
testWards =
  let wardCards = map _name $ filter (\c -> c^.ward == 1) allCards
  in (assertEqual "ward cards"
      ["Scout's Ward","Assassin's Ward","Brawler's Ward","Guardian's Ward","Lord's Ward","Magus' Ward","Sage's Ward","Sorcerer's Ward"]
      wardCards)

testBlink :: IO ()
testBlink =
  let cards = map _name $ filter (\c -> c^.blink == 1) allCards
  in (assertEqual "blink cards"
      ["Blink Charm","Blinkshot","Stab Link","Tele-Blink"]
      cards)

testParseCard :: IO ()
testParseCard =
  let i = "100 Crit Bonus|4 Ability armor|4 Basic penetration|4 Crit Chance|16 Ability penetration|150 Health|5 Lifesteal|60 Mana|11 Attack Speed|0.3 Mana Regen|12 Power|7 Basic armor|1.4 Health Regen"
      card = parseCard i
  in
    (assertEqual "crit bonus" 1 (card^.crit_bonus))
     >> (assertEqual "ability armor" 1 (card^.abilityArmor))
     >> (assertEqual "pen" 1 (card^.pen))
     >> (assertEqual "crit" 1 (card^.crit))
     >> (assertEqual "ab pen" 8 (card^.abilityPen))
     >> (assertEqual "health" 3 (card^.health))
     >> (assertEqual "lifesteal" 2 (card^.lifesteal))
     >> (assertEqual "mana" 2 (card^.mana))
     >> (assertEqual "attack speed" 2 (card^.speed))
     >> (assertEqual "mana regen" 1 (card^.manaRegen))
     >> (assertEqual "power" 2 (card^.power))
     >> (assertEqual "basic armor" 1 (card^.basicArmor))
     >> (assertEqual "health regen" 1 (card^.healthRegen))

testParseCardWithActives :: IO ()
testParseCardWithActives =
  let i = "7 Basic armor|30 Mana|Active:Restore 50 Health.|15s.Charges: 2|Unique Active:Team Shield: Consume 100 Mana to Shield yourself and nearby Allies for 3 seconds,absorbing up to 125 + (25 * Player Level) damage. Any Ally affected by Team Shield cannot activate or benefit from another Team Shield for 15 seconds."
      card = parseCard i
  in
    (assertEqual "mana" 1 (card^.mana))
     >> (assertEqual "basic armor" 1 (card^.basicArmor))

testParseFullyUpgraded :: IO()
testParseFullyUpgraded =
  let i = "11 Attack Speed|Fully Upgraded Bonus:6 Power"
      card = parseCard i
  in
    (assertEqual "power" 1 (card^.power))
     >> (assertEqual "attack speed" 2 (card^.speed))

tests2 :: TestTree
tests2 = testGroup "Parsing Tests"
  [
    testCase "parse a card" testParseCard
    , testCase "parse removing actives" testParseCardWithActives
    , testCase "parse fully upgraded bonus" testParseFullyUpgraded
    , testCase "wards" testWards
    , testCase "blink" testBlink
    , testCase "optimiztion" testOptimization
  ]
