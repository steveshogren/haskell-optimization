{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Optimization (optimize
                    , Build(..)
                    , toBuild
                    , Afinity(..)
                    , HandCard(..)
                    ) where

import Control.Lens
import Prelude hiding ((*), (/), (+), (-))
import qualified Data.Map as Map
import Data.LinearProgram as DLP
import Data.List
import qualified DamagePen as DP
import Types
import qualified Cards as C
import qualified Data.Set as Set

-- Types

mainCards :: Hero -> [Card]
mainCards hero = C.bestCarryCards (hero^.afinities)

-- Logic

upgradeTypes :: (Eq a, Num t, Num a) => a -> (t, [Char])
upgradeTypes 1 = (1, "a")
upgradeTypes 2 = (2, "a")
upgradeTypes 3 = (3, "a")
upgradeTypes 4 = (1, "b")
upgradeTypes 5 = (2, "b")
upgradeTypes _ = (3, "b")

flattenPermutations :: [(Integer, String)] -> (Integer, Integer)
flattenPermutations = foldl (\(ac, bc) (cost, t) -> if (t == "a") then (ac+cost, bc) else (ac,bc+cost)) (0, 0)

oneCardUpgrades :: [Integer]
oneCardUpgrades = [3..9]

twoCardUpgrades :: [(Integer, Integer)]
twoCardUpgrades =
  let types = Set.toList $ Set.fromList $
                map ((map upgradeTypes) . sort) [ [x,y,z]
                          | x <- [1..6],
                            y <- [1..6],
                            z <- [1..6]]
  in map (flattenPermutations) types

convertCardsToOptTuples :: (Card -> t) -> [Card] -> [OptTuple t]
convertCardsToOptTuples fn permutations =
  map (\next -> (fn next, (_name next))) permutations

collectCostAndNameTuples :: Hero -> (Card -> t) -> Bool -> [OptTuple t]
collectCostAndNameTuples hero fn useCheapCrit =
  foldl (\ret card -> ret ++ (convertCardsToOptTuples fn (cardPermutations card useCheapCrit))) [] $ mainCards hero


updateFields :: CardSetter -> Integer -> CardSetter -> Integer -> String -> String -> Card -> Card
updateFields lens add lens2 add2 n1 n2 =
  (lens +~ add) . (lens2 +~ add2) . (firstType .~ n1) . (secondType .~ n2)

cardFields :: Bool -> Bool -> Bool -> Bool -> Bool -> Card -> Integer -> Integer -> Card
cardFields True True False False False card a b = updateFields power a speed b "power" "speed" card
cardFields True False True False False card a b = updateFields power a crit b "power" "crit" card
cardFields True False False True False card a b = updateFields power a pen b "power" "pen" card
cardFields True False False False True card a b = updateFields power a lifesteal b "power" "lifesteal" card
cardFields False True True False False card a b = updateFields speed a crit b "speed" "crit" card
cardFields False True False True False card a b = updateFields speed a pen b "speed" "pen" card
cardFields False True False False True card a b = updateFields speed a lifesteal b "speed" "lifesteal" card
cardFields False False True True False card a b = updateFields crit a pen b "crit" "pen" card
cardFields False False True False True card a b = updateFields crit a lifesteal b "crit" "lifesteal" card
cardFields False False False True True card a b = updateFields pen a lifesteal b "pen" "lifesteal" card
cardFields True False False False False card a b = updateFields power a power b "power" "power" card
cardFields False True False False False card a b = updateFields speed a speed b "speed" "speed" card
cardFields False False True False False card a b = updateFields crit a crit b "crit" "crit" card
cardFields False False False True False card a b = updateFields pen a pen b "pen" "pen" card
cardFields False False False False True card a b = updateFields lifesteal a lifesteal b "lifesteal" "lifesteal" card
cardFields False False False False False card a b = card

formatCardName :: Bool -> Card -> Integer -> Integer -> Integer -> String
formatCardName hasAny card newCost ac bc =
  let costs = if (card^.firstType) == (card^.secondType) then
                " (" ++ (card^.firstType) ++ ":" ++ (show ac) ++ ") " ++ (show $ (card^.cost) + ac)
              else
                " (" ++ (card^.firstType) ++ ":" ++ (show ac) ++ ", " ++ (card^.secondType) ++ ":" ++ (show bc) ++ ") " ++ (show $ (card^.cost) + ac + bc)
  in "" ++ (_name card) ++ if hasAny then costs else "" ++ " - " ++ (show newCost)

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

hasPrimaryType :: Card -> (Bool, Bool, Bool, Bool, Bool)
hasPrimaryType card =
  let hasPower = _power card > 0
      hasSpeed = _speed card > 0
      hasCrit = _crit card > 0
      hasPen = _pen card > 0
      hasLS = _lifesteal card > 0
  in (hasPower, hasSpeed, hasCrit, hasPen, hasLS)

oneTypeCardPermutations :: Card -> Bool -> [Card]
oneTypeCardPermutations card cheapCrit =
  let (hasPower, hasSpeed, hasCrit, hasPen, hasLS) = hasPrimaryType card
      useCheapCrit = (card^.crit_bonus == 1) && cheapCrit
      upgrades = if useCheapCrit then [3] else oneCardUpgrades
  in concatMap (\c -> map (\(type1Cost) ->
                            let nc = cardFields hasPower hasSpeed hasCrit hasPen hasLS card type1Cost 0
                                newCost = (nc^.cost) + type1Cost
                            in nc { _name = formatCardName True nc newCost type1Cost 0,
                                    _cost = newCost})
                 upgrades) [1]

cardPermutations :: Card -> Bool -> [Card]
cardPermutations card useCheapCrit =
  let (hasPower, hasSpeed, hasCrit, hasPen, hasLS) = hasPrimaryType card
      hasCritBonus = _crit_bonus card > 0
      hasAny = hasPower || hasSpeed || hasCrit || hasPen || hasLS
      hasOne  = hasPower `xor` hasSpeed `xor` hasCrit `xor` hasPen `xor` hasLS
  in
    if hasOne || hasCritBonus then oneTypeCardPermutations card useCheapCrit
    else if not hasAny then [card { _name = formatCardName False card (card^.cost) 0 0}]
    else concatMap (\c -> map (\(type1Cost, type2Cost) ->
                                 let nc = cardFields hasPower hasSpeed hasCrit hasPen hasLS card type1Cost type2Cost
                                     newCost = if hasAny then (nc^.cost) + type1Cost + type2Cost else (nc^.cost)
                                 in nc { _name = (show c) ++ formatCardName hasAny nc newCost type1Cost type2Cost,
                                         _cost = newCost})
                          twoCardUpgrades) [1..3]

totalCXP :: Integer
totalCXP = 60
totalCards :: Integer
totalCards = 6

lpCards :: Build -> LP String Integer
lpCards build = execLPM $ do
  let hero = heroFromName $ build^.bhero
  let useCheapCrit = (build^.bcheapCrit)
  equalTo (linCombination (collectCostAndNameTuples hero _cost useCheapCrit)) totalCXP
  equalTo (linCombination (collectCostAndNameTuples hero _power useCheapCrit)) (build^.bpower)
  equalTo (linCombination (collectCostAndNameTuples hero _speed useCheapCrit)) (build^.bspeed)
  equalTo (linCombination (collectCostAndNameTuples hero _crit useCheapCrit)) (build^.bcrit)
  equalTo (linCombination (collectCostAndNameTuples hero _pen useCheapCrit)) (build^.bpen)
  equalTo (linCombination (collectCostAndNameTuples hero _lifesteal useCheapCrit)) (build^.blifesteal)
  equalTo (linCombination (collectCostAndNameTuples hero _crit_bonus useCheapCrit)) (build^.bcrit_bonus)
  equalTo (linCombination (collectCostAndNameTuples hero _ward useCheapCrit)) (build^.bward)
  equalTo (linCombination (collectCostAndNameTuples hero _blink useCheapCrit)) (build^.bblink)
  equalTo (linCombination (map (\(_,name) -> (1, name)) $ collectCostAndNameTuples hero _power useCheapCrit)) totalCards
  mapM (\(_,name) -> varBds name 0 1) $ collectCostAndNameTuples hero _power useCheapCrit
  mapM (\(_,name) -> setVarKind name IntVar) $ collectCostAndNameTuples hero _power useCheapCrit


solverFailed :: IO [HandCard]
solverFailed = return [toHandCard("Combination impossible", 0)]

optimize :: Build -> IO [HandCard]
optimize b = do
  x <- glpSolveVars mipDefaults (lpCards b)
  putStrLn $ "Build" ++ (show b)
  case x of (Success, Just (obj, vars)) ->
              let cards = (map toHandCard) $ filter (\(name, count) -> count /= 0) $ Map.toList vars
              in if null cards then solverFailed
                 else return cards
            (failure, result) -> solverFailed
