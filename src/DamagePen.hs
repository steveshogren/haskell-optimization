{-# LANGUAGE TemplateHaskell #-}
module DamagePen (maxDps) where

import Control.Lens
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
dmgReduction2 en_armor penpts =
  let effectiveArmor = en_armor - (penpts * 4.0)
      realArmor = if (effectiveArmor < 0) then 0 else effectiveArmor
      reduction = (100/(100 + effectiveArmor))
  in if reduction > 1 then 1 else reduction

dps :: Hero -> Integer -> Integer -> Integer -> Integer -> Double -> Double -> Double
dps hero pwr as cr pn crit_damage en_armor = do
  let power_points = (fromInteger pwr)
      attack_speed_points = (fromInteger as)
      crit_points = (fromInteger cr)
      pen = (fromInteger pn)
      reduction = dmgReduction2 en_armor pen
      base_dmg = ((hero^.base_damage)+(6*power_points*(hero^.scaling)))
      hits_second = 1/((hero^.base_attack_time)/(((5.5*attack_speed_points) + (hero^.attack_speed))/100))
      crit_bonus = (1+((0.04*crit_points)*(crit_damage-1)))
  base_dmg * hits_second * crit_bonus  * reduction


rounder f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

calcIfUnder :: String -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Double -> Build
calcIfUnder hero_name dmg speed crit pen critbonus max ward blink ls en_armor =
  if (dmg + speed + crit + pen + (critbonus * 6)) == max
  then
    let bonus = if critbonus == 1 then yesBonusCrit else noBonusCrit
        dpsNum = dps (heroFromName hero_name) dmg speed crit pen bonus en_armor
    in toBuild (rounder dpsNum 0, dmg,speed,crit,pen,critbonus, ward, blink, ls, hero_name)
  else toBuild (0,0,0,0,0,0,0,0,0,hero_name)

-- wards, blink, and crit bonus take up extra because of the missed opportunity cost
-- of the "completed" bonus another card would offer. Wards are 3cxp for 2pwr, so
-- (-2 a full power card and -1 for opportunty)
maxDps :: Bool -> Bool -> Integer -> String -> Integer -> Double -> Build
maxDps w b lifeSteal hero_name reduce_by en_armor =
  let totalPoints = 66 -- counts the bonus +1 of the 6 cards
      ward = if w then 1 else 0
      blink = if b then 1 else 0
      maxPen = if hero_name == "sparrow" then 0 else 30
      points = totalPoints - lifeSteal - (3 * ward) - (6 * blink) - reduce_by
      totals = [ (calcIfUnder hero_name dmg speed crit pen critbonus points ward blink lifeSteal en_armor) |
                 dmg <- [0..30],
                 speed <- [0..30],
                 crit <- [0..30],
                 pen <- [0..maxPen],
                 critbonus <- [0..1]]
  in head $ sortBy (flip compare `on` _bdps) totals
