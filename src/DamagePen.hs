module DamagePen (maxDps) where

import Optimization
import Data.List
import Data.Ord
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

dps :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
dps power_points attack_speed_points crit_points pen crit_damage base_damage base_attack_speed scaling lvl =
  let reduction = dmgReduction2 0 pen
      base_dmg = (base_damage+(6*power_points*scaling))
      hits_second = ((1+(0.055*attack_speed_points))/base_attack_speed)
      crit_bonus = (1+((0.04*crit_points)*(crit_damage-1)))
  in base_dmg * hits_second * crit_bonus * reduction

murdockDps :: Integer -> Integer -> Integer -> Integer -> Double -> Double
murdockDps pwr speed crit pen bonus = dps (fromInteger pwr) (fromInteger speed) (fromInteger crit) (fromInteger pen) bonus 86 1.16 1 15

toBuild (dps,dmg,speed,crit,pen,critbonus, ward, blink, ls) =
  Build { _bpower = dmg
        , _bdps = dps
        , _bspeed = speed
        , _bcrit = crit
        , _bpen = pen
        , _blifesteal = ls
        , _bcrit_bonus = critbonus
        , _bward = ward
        , _bblink = blink}

rounder f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)


calcIfUnder :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Build
calcIfUnder dmg speed crit pen critbonus max ward blink ls =
  if (dmg + speed + crit + pen + (critbonus * 6)) == max
  then
    let bonus = if critbonus == 1 then yesBonusCrit else noBonusCrit
        dpsNum = murdockDps dmg speed crit pen bonus
    in toBuild (rounder dpsNum 0, dmg,speed,crit,pen,critbonus, ward, blink, ls)
  else toBuild (0,0,0,0,0,0,0,0, 0)

-- wards, blink, and crit bonus take up (+1) because of the missed opportunity cost
-- of the "completed" bonus another card would offer
maxDps w b lifeSteal =
  let totalPoints = 60 -- counts the bonus +1 of the 6 cards
      ward = if w then 1 else 0
      blink = if b then 1 else 0
      points = totalPoints - lifeSteal - (2 * ward) - (6 * blink)
      totals = [ (calcIfUnder dmg speed crit pen critbonus points ward blink lifeSteal) |
                 dmg <- [0..30],
                 speed <- [0..30],
                 crit <- [0..30],
                 pen <- [0..30],
                 critbonus <- [0..1]]
      builds = take 3 $ sortBy (flip compare `on` _bdps) totals
  in builds
