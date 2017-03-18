{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Control.Lens

type OptTuple t = (t, String)

data Afinity = Growth | Order | Fury | Intellect | Corruption | Universal
  deriving (Show, Eq)

-- Hand Card

data HandCard = HandCard {
  count :: Double,
  info :: String
  } deriving (Show, Generic)

instance ToJSON HandCard
instance FromJSON HandCard

toHandCard :: (String, Double) -> HandCard
toHandCard (n, c) = HandCard { info = n, count = c}


-- Build

data Build = Build {
  _bdps :: Double,
  _bpower :: Integer,
  _bspeed :: Integer,
  _bcrit :: Integer,
  _bpen :: Integer,
  _blifesteal :: Integer,
  _bcrit_bonus :: Integer,
  _bward :: Integer,
  _bblink :: Integer,
  _bhero :: String
  } deriving (Show, Generic)
makeLenses ''Build
instance ToJSON Build
instance FromJSON Build

toBuild :: (Double, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, String) -> Build
toBuild (dps,dmg,speed,crit,pen,critbonus, ward, blink, ls, hero) =
  Build { _bpower = dmg
        , _bdps = dps
        , _bspeed = speed
        , _bcrit = crit
        , _bpen = pen
        , _blifesteal = ls
        , _bcrit_bonus = critbonus
        , _bward = ward
        , _bhero = hero
        , _bblink = blink}

-- Card

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
    , _firstType :: String
    , _secondType :: String
    , _afinity :: Afinity
    , _mana :: Integer
    , _manaRegen :: Integer
    , _health :: Integer
    , _healthRegen :: Integer
    , _basicArmor :: Integer
    , _abilityArmor :: Integer
    , _abilityPen :: Integer
    } deriving (Show)
makeLenses ''Card

type CardSetter = ASetter Card Card Integer Integer

defaultCard =
  Card { _cost = 0
       , _power = 0
       , _speed = 0
       , _crit = 0
       , _pen = 0
       , _lifesteal = 0
       , _crit_bonus = 0
       , _ward = 0
       , _blink = 0
       , _name = ""
       , _afinity = Universal
       , _firstType = ""
       , _secondType = ""
       , _mana  = 0
       , _manaRegen  = 0
       , _health  = 0
       , _healthRegen  = 0
       , _basicArmor  = 0
       , _abilityArmor  = 0
       , _abilityPen  = 0
       }

toCard :: (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, String, Afinity) -> Card
toCard (cost, power, speed, crit, pen, lifesteal, crit_bonus, ward, blink, name, afinity) =
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
       , _afinity = afinity
       , _firstType = ""
       , _secondType = ""
       , _mana  = 0
       , _manaRegen  = 0
       , _health  = 0
       , _healthRegen  = 0
       , _basicArmor  = 0
       , _abilityArmor  = 0
       , _abilityPen  = 0
       }

-- Hero

data Hero = Hero {
  _base_damage :: Double,
  _attack_speed :: Double,
  _base_attack_time :: Double,
  _scaling :: Double,
  _lvl :: Double,
  _afinities :: [Afinity]
  } deriving (Show, Eq)
makeLenses ''Hero

toHero :: Double -> Double -> Double -> Double -> Double -> [Afinity] -> Hero
toHero bd att bas sc l afs = Hero { _base_damage = bd, _attack_speed = bas, _scaling = sc, _lvl = l, _afinities = afs, _base_attack_time = att}

-- Hero

heroFromName "murdock" = toHero   86   1.35 116.8 1    15 [Fury, Intellect]
heroFromName "sparrow" = toHero   64   1.2  139.2 1    15 [Growth, Intellect]
heroFromName "grim" = toHero      77.4 1.2  139.2 0.85 15 [Fury, Corruption]
heroFromName "twinblast" = toHero 68.2 1.0  100.0 0.8  15 [Growth, Fury]
heroFromName _ = toHero 0 0 0 0 0 []

-- UISetting

data UISetting = UISetting { hero_name :: String
                           , has_blink :: Bool
                           , has_ward :: Bool
                           , enemy_armor :: Double
                           , desired_lifesteal :: Integer}
             deriving (Show, Generic)
instance ToJSON UISetting
instance FromJSON UISetting
