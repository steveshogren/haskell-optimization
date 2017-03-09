{-# LANGUAGE TemplateHaskell #-}
module DamagePen (maxDps) where

import Control.Lens
import Optimization
import Data.List
import Data.Ord
import Types
import Data.Function (on)


attackSpeed :: Fractional a => a -> a -> a
attackSpeed bat asm =(1/bat)*asm

yesBonusCrit :: Double
yesBonusCrit = 2.5

noBonusCrit :: Double
noBonusCrit = 1.5

armorPointsToNum :: Num a => a -> a
armorPointsToNum ar = ar * 7

dmgReduction2 :: Double -> Double -> Double
dmgReduction2 armorpts penpts =
  let effectiveArmor = 31.5 + (7 * armorpts) - (penpts * 4.0)
      realArmor = if (effectiveArmor < 0) then 0 else effectiveArmor
      reduction = (100/(100 + effectiveArmor))
  in if reduction > 1 then 1 else reduction

dps :: Hero -> Integer -> Integer -> Integer -> Integer -> Double -> Double
dps hero pwr as cr pn crit_damage =
  let power_points = (fromInteger pwr)
      attack_speed_points = (fromInteger as)
      crit_points = (fromInteger cr)
      pen = (fromInteger pn)
      reduction = dmgReduction2 0 pen
      base_dmg = ((hero^.base_damage)+(6*power_points*(hero^.scaling)))
      hits_second = ((1+(0.055*attack_speed_points))/(hero^.base_attack_speed))
      crit_bonus = (1+((0.04*crit_points)*(crit_damage-1)))
  in base_dmg * hits_second * crit_bonus * reduction

hero "murdock" = toHero   86   1.168 1    15 [Fury, Intellect]
hero "sparrow" = toHero   64   1.392 1    15 [Growth, Intellect]
hero "grim" = toHero      77.4 1.392 0.85 15 [Intellect, Fury]
hero "twinblast" = toHero 68.2 1.000 0.8  15 [Growth, Fury]
hero _ = toHero 0 0 0 0 []

rounder f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

calcIfUnder :: String -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Build
calcIfUnder hero_name dmg speed crit pen critbonus max ward blink ls =
  if (dmg + speed + crit + pen + (critbonus * 6)) == max
  then
    let bonus = if critbonus == 1 then yesBonusCrit else noBonusCrit
        dpsNum = dps (hero hero_name) dmg speed crit pen bonus
    in toBuild (rounder dpsNum 0, dmg,speed,crit,pen,critbonus, ward, blink, ls)
  else toBuild (0,0,0,0,0,0,0,0, 0)

-- wards, blink, and crit bonus take up extra because of the missed opportunity cost
-- of the "completed" bonus another card would offer. Wards are 3cxp for 2pwr, so
-- (-2 a full power card and -1 for opportunty)
maxDps w b lifeSteal hero_name =
  let totalPoints = 66 -- counts the bonus +1 of the 6 cards
      ward = if w then 1 else 0
      blink = if b then 1 else 0
      points = totalPoints - lifeSteal - (3 * ward) - (6 * blink)
      totals = [ (calcIfUnder hero_name dmg speed crit pen critbonus points ward blink lifeSteal) |
                 dmg <- [0..30],
                 speed <- [0..30],
                 crit <- [0..30],
                 pen <- [0..30],
                 critbonus <- [0..1]]
      builds = take 3 $ sortBy (flip compare `on` _bdps) totals
  in builds
