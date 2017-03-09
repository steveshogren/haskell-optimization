{-# LANGUAGE TemplateHaskell #-}
module DamagePen where --(maxDps) where

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

dps :: Hero -> Integer -> Integer -> Integer -> Integer -> Double -> IO Double
dps hero pwr as cr pn crit_damage = do
  let power_points = (fromInteger pwr)
  let attack_speed_points = (fromInteger as)
  let crit_points = (fromInteger cr)
  let pen = (fromInteger pn)
  let reduction = dmgReduction2 0 pen
  let base_dmg = ((hero^.base_damage)+(6*power_points*(hero^.scaling)))
  putStrLn $ "Base dmg: " ++ (show base_dmg)
  let hits_second = 1/((hero^.base_attack_time)/(((5.5*attack_speed_points) + (hero^.attack_speed))/100))
  putStrLn $ "Hits/second: " ++ (show hits_second)
  let crit_bonus = (1+((0.04*crit_points)*(crit_damage-1)))
  putStrLn $ "Crit: " ++ (show crit_bonus)
  return $ base_dmg * hits_second * crit_bonus  * reduction

hero "murdock" = toHero   86   1.35 116.8 1    15 [Fury, Intellect]
hero "sparrow" = toHero   64   1.2  139.2 1    15 [Growth, Intellect]
hero "grim" = toHero      77.4 1.2  139.2 0.85 15 [Intellect, Fury]
hero "twinblast" = toHero 68.2 1.0  100.0 0.8  15 [Growth, Fury]
hero _ = toHero 0 0 0 0 0 []

rounder f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

-- calcIfUnder :: String -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Build
-- calcIfUnder hero_name dmg speed crit pen critbonus max ward blink ls =
--   if (dmg + speed + crit + pen + (critbonus * 6)) == max
--   then
--     let bonus = if critbonus == 1 then yesBonusCrit else noBonusCrit
--         dpsNum = dps (hero hero_name) dmg speed crit pen bonus
--     in toBuild (rounder dpsNum 0, dmg,speed,crit,pen,critbonus, ward, blink, ls)
--   else toBuild (0,0,0,0,0,0,0,0, 0)

-- -- wards, blink, and crit bonus take up extra because of the missed opportunity cost
-- -- of the "completed" bonus another card would offer. Wards are 3cxp for 2pwr, so
-- -- (-2 a full power card and -1 for opportunty)
-- maxDps w b lifeSteal hero_name =
--   let totalPoints = 66 -- counts the bonus +1 of the 6 cards
--       ward = if w then 1 else 0
--       blink = if b then 1 else 0
--       points = totalPoints - lifeSteal - (3 * ward) - (6 * blink)
--       totals = [ (calcIfUnder hero_name dmg speed crit pen critbonus points ward blink lifeSteal) |
--                  dmg <- [0..30],
--                  speed <- [0..30],
--                  crit <- [0..30],
--                  pen <- [0..30],
--                  critbonus <- [0..1]]
--       builds = take 3 $ sortBy (flip compare `on` _bdps) totals
--   in builds
