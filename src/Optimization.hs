{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Optimization (optimize
                    , Build(..)
                    , HandCard(..)
                    ) where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Control.Lens
import Prelude hiding ((*), (/), (+), (-))
import qualified Data.Map as Map
import Data.LinearProgram as DLP
import Data.List
import qualified Data.Set as Set

-- Types

-- -- Hand Card
data HandCard = HandCard {
  count :: Double,
  info :: String
  } deriving (Show, Generic)

instance ToJSON HandCard
instance FromJSON HandCard

toHandCard :: (String, Double) -> HandCard
toHandCard (n, c) = HandCard { info = n, count = c}

-- -- Build

data Build = Build {
  _bdps :: Double,
  _bpower :: Integer,
  _bspeed :: Integer,
  _bcrit :: Integer,
  _bpen :: Integer,
  _blifesteal :: Integer,
  _bcrit_bonus :: Integer,
  _bward :: Integer,
  _bblink :: Integer
  } deriving (Show, Generic)
makeLenses ''Build
instance ToJSON Build
instance FromJSON Build

-- -- Card

data Card = Card
    { _cost :: Integer
    , _power :: Integer
    , _speed :: Integer
    , _crit :: Integer
    , _pen :: Integer
    , _lifesteal :: Integer
    , _crit_bonus :: Integer
    , _ward :: Integer
    , _blink :: Integer
    , _name :: String
    , _letter :: String
    , _firstType :: String
    , _secondType :: String
    }
makeLenses ''Card

toCard :: (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, String) -> String -> Card
toCard (cost, power, speed, crit, pen, lifesteal, crit_bonus, ward, blink, name) letter =
  Card { _cost = cost
       , _power = power
       , _speed = speed
       , _crit = crit
       , _pen = pen
       , _lifesteal = lifesteal
       , _crit_bonus = crit_bonus
       , _ward = ward
       , _blink = blink
       , _name = name
       , _letter = letter
       , _firstType = ""
       , _secondType = ""
       }

mainCards :: [Card]
mainCards =
  zipWith toCard
    [(2, 2, 1, 0, 0, 0, 0, 0, 0, "madstone gem")
    ,(2, 0, 2, 1, 0, 0, 0, 0, 0, "redeye nitro")
    ,(3, 2, 0, 2, 0, 0, 0, 0, 0, "impact hammer")
    ,(3, 3, 1, 0, 0, 0, 0, 0, 0, "windcarver blade")
    ,(3, 0, 0, 1, 0, 3, 0, 0, 0, "brand ironeater")
    ,(3, 3, 0, 0, 1, 0, 0, 0, 0, "rustbreaker")
    ,(3, 1, 0, 3, 0, 0, 0, 0, 0, "spear rifthunter")
    ,(3, 1, 3, 0, 0, 0, 0, 0, 0, "whirling wand")
    ,(3, 3, 0, 1, 0, 0, 0, 0, 0, "micro-nuke")
    ,(6, 1, 0, 0, 0, 0, 1, 0, 0, "blade of agora")
    ,(6, 0, 0, 0, 0, 1, 1, 0, 0, "hunger maul")
    ,(3, 2, 0, 0, 0, 0, 0, 1, 0, "sages ward")
    ,(5, 0, 0, 0, 0, 0, 0, 0, 1, "blink charm")
    ,(6, 0, 1, 0, 0, 0, 1, 0, 0, "blast harness")
    ]
    (map (\a -> [a]) ['a'..])

type OptTuple t = (t, String)

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

collectAllPermutations :: (Card -> t) -> [OptTuple t]
collectAllPermutations fn =
  foldl (\ret card -> ret ++ (convertCardsToOptTuples fn (cardPermutations card))) [] mainCards

type CardSetter = ASetter Card Card Integer Integer

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
cardFields False False False False True card a b = updateFields lifesteal a lifesteal b "lifesteal" "lifesteal" card
cardFields True False False False False card a b = updateFields power a power b "power" "power" card
cardFields False True False False False card a b = updateFields speed a speed b "speed" "speed" card
cardFields False False False False False card a b = card

formatCardName :: Bool -> Card -> Integer -> Integer -> Integer -> String
formatCardName hasAny card newCost ac bc =
  let costs = if (card^.firstType) == (card^.secondType) then
                " (" ++ (card^.firstType) ++ ":" ++ (show ac) ++ ") " ++ (show $ (card^.cost) + ac)
              else
                " (" ++ (card^.firstType) ++ ":" ++ (show ac) ++ ", " ++ (card^.secondType) ++ ":" ++ (show bc) ++ ") " ++ (show $ (card^.cost) + ac + bc)
  in " " ++ (_name card) ++ if hasAny then costs else "" ++ " - " ++ (show newCost)

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

oneTypeCardPermutations :: Card -> [Card]
oneTypeCardPermutations card =
  let hasPower = _power card > 0
      hasSpeed = _speed card > 0
      hasCrit = _crit card > 0
      hasPen = _pen card > 0
      hasLS = _lifesteal card > 0
  in concatMap (\c -> map (\(type1Cost) ->
                            let nc = cardFields hasPower hasSpeed hasCrit hasPen hasLS card type1Cost 0
                                newCost = (nc^.cost) + type1Cost
                            in nc { _name = formatCardName True nc newCost type1Cost 0,
                                    _cost = newCost}) oneCardUpgrades) [1..1]

cardPermutations :: Card -> [Card]
cardPermutations card =
  let hasPower = _power card > 0
      hasSpeed = _speed card > 0
      hasCrit = _crit card > 0
      hasPen = _pen card > 0
      hasLS = _lifesteal card > 0
      hasAny = hasPower || hasSpeed || hasCrit || hasPen || hasLS
      hasOne  = hasPower `xor` hasSpeed `xor` hasCrit `xor` hasPen `xor` hasLS
  in
    if hasOne then oneTypeCardPermutations card
    else if not hasAny then [card { _name = formatCardName False card (card^.cost) 0 0}]
    else concatMap (\c -> map (\(type1Cost, type2Cost) ->
                                 let nc = cardFields hasPower hasSpeed hasCrit hasPen hasLS card type1Cost type2Cost
                                     newCost = if hasAny then (nc^.cost) + type1Cost + type2Cost else (nc^.cost)
                                 in nc { _name = formatCardName hasAny nc newCost type1Cost type2Cost,
                                         _cost = newCost})
                          twoCardUpgrades) [1..1]

totalCXP :: Integer
totalCXP = 60
totalCards :: Integer
totalCards = 6

lpCards :: Build -> LP String Integer
lpCards build = execLPM $ do
  equalTo (linCombination (collectAllPermutations _cost)) totalCXP
  geqTo (linCombination (collectAllPermutations _power)) (build^.bpower)
  geqTo (linCombination (collectAllPermutations _speed)) (build^.bspeed)
  geqTo (linCombination (collectAllPermutations _crit)) (build^.bcrit)
  geqTo (linCombination (collectAllPermutations _pen)) (build^.bpen)
  geqTo (linCombination (collectAllPermutations _lifesteal)) (build^.blifesteal)
  geqTo (linCombination (collectAllPermutations _crit_bonus)) (build^.bcrit_bonus)
  geqTo (linCombination (collectAllPermutations _ward)) (build^.bward)
  geqTo (linCombination (collectAllPermutations _blink)) (build^.bblink)
  equalTo (linCombination (map (\(_,n) -> (1, n)) $ collectAllPermutations _power)) totalCards
  mapM (\(_,n) -> setVarKind n IntVar) $ collectAllPermutations _power
  mapM (\(_,n) -> varBds n 0 1) $ collectAllPermutations _power

optimize :: Build -> IO [HandCard]
optimize b = do
  x <- glpSolveVars mipDefaults (lpCards b)
  case x of (Success, Just (obj, vars)) ->
              let cards = (map toHandCard) $ filter (\(name, count) -> count > 0) $ Map.toList vars
              in if null cards then
                   return [toHandCard("Combination impossible", 0)]
                 else return cards
            (failure, result) -> return []
