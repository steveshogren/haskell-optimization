module Cards where

import Types
import qualified Data.Char as C
import qualified Data.List as L
import Control.Lens

cardTuples = [("Barrier Token",1,Universal,"4 Ability armor"),
              ("Bump Juice",1,Fury,"Active:Restore 50 Health.|15s.Charges: 2"),
              ("Cast Token",1,Universal,"6 Power"),
              ("Chrono-Tonic",1,Intellect,"Active:For 15 seconds +3.6 Health regeneration and +10 Cooldown reduction.|15s.Charges: 2"),
              ("Fiend Elixir",1,Corruption,"Active:For 15 seconds +2.28 Mana regeneration,and upon receiving damage restore 12 Mana.|15s.Charges: 2"),
              ("Guard Token",1,Universal,"7 Basic armor"),
              ("Health Potion",1,Universal,"Active:For 15 seconds restore 90 Health.|15s.Charges: 2"),
              ("Health Token",1,Universal,"50 Health"),
              ("Hunter's Drink",1,Growth,"Active:For 15 seconds +3.6 Health regeneration and aditiondal 0,6xPlayer level.|15s.Charges: 2"),
              ("Jungle Drink",1,Growth,"Active:For 15 seconds +6 damage and +3.6 Health regeneration.|15s.Charges: 2"),
              ("Mana Potion",1,Universal,"Active:For 15 seconds restore 57 Mana.|15s.Charges: 2"),
              ("Mender's Phial",1,Order,"Active:For 15 seconds +4.37 Health regeneration and for 7 seconds +3.8 Mana regeneration to nearby allies.|15s.Charges: 2"),
              ("Scout's Ward",1,Universal,"Unique Active:Place a Ward to reveal the surrounding area for 1.5 minutes.|60s.Charges: 1"),
              ("Shaman's Drink",1,Growth,"Active:For 15 seconds +2.28 Mana regeneration and aditiondal 0,3xPlayer level. |15s.Charges: 2"),
              ("Shepard's Phial",1,Order,"Active:For 15 seconds +2.85 Mana regeneration and for 7 seconds +6 Health regeneration to nearby allies.|15s.Charges: 2"),
              ("Strike Token",1,Universal,"6 Power"),
              ("Vampiric Elixir",1,Corruption,"Active:For 15 seconds gain +5 Lifesteal and +3.6 Health regeneration.|15s.Charges: 2"),
              ("Zap Juice",1,Fury,"Active:Restore 35 Mana.|15s.Charges: 2"),
              ("Flashfire Piston",2,Fury,"11 Attack Speed|Fully Upgraded Bonus:6 Power"),
              ("Hatespitter",2,Fury,"2 Ability penetration|0.3 Mana Regen|Fully Upgraded Bonus:0.3 Mana Regen"),
              ("Healer Token",2,Universal,"2.8 Health Regen"),
              ("Madstone Gem",2,Fury,"12 Power|Fully Upgraded Bonus:5.5 Attack Speed"),
              ("Rageheart Engine",2,Fury,"4 Basic penetration|0.3 Mana Regen|Fully Upgraded Bonus:0.3 Mana Regen"),
              ("Redeye Nitro",2,Fury,"5.5 Attack Speed|4 Crit Chance|Fully Upgraded Bonus:5.5 Attack Speed"),
              ("Adamant Edge",3,Universal,"6 Power|50 Health|Fully Upgraded Bonus:12 Power"),
              ("Amp Crystal",3,Intellect,"6 Power|30 Mana|Fully Upgraded Bonus:60 Mana"),
              ("Amulet of the Veteran",3,Universal,"100 Health|Fully Upgraded Bonus:6 Power|50 Health"),
              ("Armavore",3,Fury,"2 Ability penetration|Unique Passive:On hit with basic attack,restore 2 Mana."),
              ("Assassin's Ward",3,Corruption,"6 Power|4 Basic penetration|2.5 Lifesteal|Unique Active:Place a Shadow Ward to reveal the surrounding area for 1.5 minutes."),
              ("Backstabber",3,Corruption,"4 Crit Chance|Unique Passive:+24 Critical chance versus heroes not in combat."),
              ("Beastmaster's Scythe",3,Universal,"50 Health|5.5 Attack Speed|Unique Passive:+15 Damage bonus against Jungle minions."),
              ("Berzerker Drive",3,Fury,"6 Power|0.3 Mana Regen|5.5 Attack Speed|Fully Upgraded Bonus:5.5 Attack Speed"),
              ("Bloodsoaked Armor",3,Corruption,"7 Basic armor|5.5 Attack Speed|Unique Active:For 6 seconds,when hit by a basic attack,apply a Bleed stack to attacker (Max 10 stacks)."),
              ("Bone Dice",3,Fury,"4 Crit Chance|Unique Passive:While Critically wounded,gain +20 critical chance."),
              ("Brand of the Ironeater",3,Fury,"4 Crit Chance|5 Lifesteal|Fully Upgraded Bonus:2.5 Lifesteal"),
              ("Brawler's Scythe",3,Universal,"6 Power|30 Mana|Unique Passive:+15 Damage bonus against Jungle minions."),
              ("Brawler's Ward",3,Universal,"12 Power|30 Mana|Unique Active:Place a Shadow Ward to reveal the surrounding area for 1.5 minutes."),
              ("Brightsteel Plate",3,Order,"7 Basic armor|8 Crit Chance|Fully Upgraded Bonus:7 Basic armor"),
              ("Burnblood Powder",3,Fury,"6 Power|0.3 Mana Regen|5.5 Attack Speed|Fully Upgraded Bonus:0.3 Mana Regen"),
              ("Burst-Engine",3,Intellect,"6 Power|0.3 Mana Regen|Fully Upgraded Bonus:12 Power"),
              ("Celestine Diamond",3,Order,"6 Power|0.3 Mana Regen|30 Mana|Fully Upgraded Bonus:30 Mana"),
              ("Circlet of Health",3,Universal,"30 Mana|Unique Passive:+2.8 Health regeneration to nearby allies."),
              ("Circlet of Mana",3,Universal,"50 Health|Unique Passive:+0.6 Mana regeneration to nearby allies."),
              ("Combustion",3,Fury,"0.3 Mana Regen|Unique Passive:While Burning,gain +16.5 Attack speed and +160 Movement speed."),
              ("Crystal Conduit",3,Intellect,"2 Ability penetration|30 Mana|Fully Upgraded Bonus:60 Mana"),
              ("Eldermage Amulet",3,Universal,"6 Power|50 Health|Fully Upgraded Bonus:100 Health"),
              ("Elysian Diamond",3,Order,"6 Power|0.3 Mana Regen|50 Health|Fully Upgraded Bonus:50 Health"),
              ("Executioner's Scythe",3,Corruption,"6 Power|4 Crit Chance|Unique Passive:+15 Damage bonus against Jungle minions."),
              ("Fountain Spike",3,Universal,"6 Power|30 Mana|Fully Upgraded Bonus:6 Power|30 Mana"),
              ("Fracturing Spike",3,Intellect,"4 Basic penetration|0.3 Mana Regen|Fully Upgraded Bonus:12 Power"),
              ("Genesis Torch",3,Order,"4 Basic penetration|4 Ability armor|0.3 Mana Regen|Fully Upgraded Bonus:0.3 Mana Regen"),
              ("Gift of the Rains",3,Growth,"0.3 Mana Regen|1.4 Health Regen|Fully Upgraded Bonus:2.8 Health Regen"),
              ("Golden Veil",3,Order,"4 Ability armor|8 Crit Chance|Fully Upgraded Bonus:4 Ability armor"),
              ("Guardian's Scythe",3,Universal,"6 Power|50 Health|Unique Passive:+15 Damage bonus against Jungle minions."),
              ("Guardian's Ward",3,Universal,"6 Power|50 Health|Unique Active:Place a Shadow Ward to reveal the surrounding area for 1.5 minutes."),
              ("Hemorrhage",3,Fury,"0.3 Mana Regen|Unique Passive:While Bleeding,gain +16.5 Attack speed and +18 damage."),
              ("Hex of the Devastator",3,Corruption,"2 Ability penetration|4 Crit Chance|Fully Upgraded Bonus:8 Crit Chance"),
              ("Hinder-Beast",3,Fury,"50 Health|Unique Passive:While carryung River buff,on hit basic attack,slow target by 5 for 1.5 seconds."),
              ("Impact Hammer",3,Fury,"12 Power|4 Crit Chance|Fully Upgraded Bonus:4 Crit Chance"),
              ("Irradiate",3,Fury,"5.5 Attack Speed|Unique Passive:While Poisoned,gain +7.5 Cooldown reduction and +19.5 damage."),
              ("Killdozer Horn",3,Fury,"30 Mana|Unique Passive:+6 damage to nearby allies minions."),
              ("Lantern of Spring",3,Order,"2 Ability penetration|7 Basic armor|0.3 Mana Regen|Fully Upgraded Bonus:0.3 Mana Regen"),
              ("Lord's Siphon",3,Universal,"30 Mana|50 Health|0.3 Mana Regen|Unique Passive:Basic attacks on Jungle Minions,restore 8 health."),
              ("Lord's Ward",3,Universal,"30 Mana|50 Health|0.3 Mana Regen|Unique Active:Place a Shadow Ward to reveal the surrounding area for 1.5 minutes."),
              ("Magna-Lens",3,Intellect,"2 Ability penetration|0.3 Mana Regen|Fully Upgraded Bonus:12 Power"),
              ("Magus' Siphon",3,Universal,"6 Power|30 Mana|Unique Passive:Basic attacks on Jungle Minions,restore 8 health."),
              ("Magus' Ward",3,Universal,"6 Power|30 Mana|Unique Active:Place a Shadow Ward to reveal the surrounding area for 1.5 minutes."),
              ("Meltdown",3,Fury,"12 Power|2 Ability penetration|Fully Upgraded Bonus:6 Power"),
              ("Merciless",3,Fury,"4 Crit Chance|5.5 Attack Speed|Unique Passive:+12 Critical chance against Bleeding target."),
              ("Micro-Nuke",3,Fury,"12 Power|4 Crit Chance|Fully Upgraded Bonus:6 Power"),
              ("Mind Flow",3,Intellect,"30 Mana|Unique Passive:+10 Cooldown reduction,while over 50Mana."),
              ("Muta-Gem",3,Intellect,"6 Power|30 Mana|Fully Upgraded Bonus:0.6 Mana Regen"),
              ("Nirvana Stone",3,Order,"4 Crit Chance|1.4 Health Regen|0.3 Mana Regen|Fully Upgraded Bonus:4 Crit Chance"),
              ("Oasis Siphon",3,Growth,"1.4 Health Regen|2.5 Lifesteal|Unique Passive:Basic attacks on Jungle Minions,restore 8 health."),
              ("Offensive Maneuvers",3,Order,"8 Crit Chance|Unique Passive:+10 Physical armor and +Energy armor against basic attacks."),
              ("Pendulum of Lords",3,Universal,"30 Mana|50 Health|0.3 Mana Regen|Fully Upgraded Bonus:0.3 Mana Regen"),
              ("Quartzblade",3,Order,"6 Power|0.3 Mana Regen|50 Health|Fully Upgraded Bonus:6 Power"),
              ("Reaper's Siphon",3,Corruption,"6 Power|4 Crit Chance|Unique Passive:Basic attacks on Jungle Minions,restore 8 health."),
              ("Reptilian Claw",3,Growth,"1.4 Health Regen|4 Crit Chance|Fully Upgraded Bonus:2.8 Health Regen"),
              ("Riftmagus Scepter",3,Universal,"6 Power|4 Crit Chance|Fully Upgraded Bonus:12 Power"),
              ("Rust-Breaker",3,Fury,"12 Power|4 Basic penetration|Fully Upgraded Bonus:6 Power"),
              ("Sage's Siphon",3,Universal,"6 Power|50 Health|Unique Passive:Basic attacks on Jungle Minions,restore 8 health."),
              ("Sage's Ward",3,Universal,"12 Power|50 Health|Unique Active:Place a Shadow Ward to reveal the surrounding area for 1.5 minutes."),
              ("Savage Remedy",3,Growth,"30 Mana|1.4 Health Regen|Fully Upgraded Bonus:2.8 Health Regen"),
              ("Scourging Tails",3,Corruption,"6 Power|Active: For 6 seconds,on hit with ability shred 20 Armor from target(s) for 15 seconds."),
              ("Seek and Destroy",3,Corruption,"6 Power|Unique Active:For 6 seconds +40 Movement speed +5.5 Attack speed for each nearby Bleeding unit. (Max - +280 Movement speed and +38.5 Attack speed)"),
              ("Shadow Scroll",3,Corruption,"6 Power|2 Ability penetration|Fully Upgraded Bonus:4 Ability penetration"),
              ("Silverspear",3,Order,"6 Power|1.4 Health Regen|30 Mana|Fully Upgraded Bonus:6 Power"),
              ("Siphon Shard",3,Intellect,"1.4 Health Regen|2.5 Lifesteal|Fully Upgraded Bonus:5 Lifesteal"),
              ("Snakevine Mesh",3,Growth,"4 Ability armor|6 Power|Fully Upgraded Bonus:8 Ability armor"),
              ("Solaris Reactor",3,Intellect,"6 Power|0.3 Mana Regen|Fully Upgraded Bonus:0.6 Mana Regen"),
              ("Sorcerer's Ward",3,Corruption,"6 Power|2 Ability penetration|2.5 Lifesteal|Unique Active:Place a Shadow Ward to reveal the surrounding area for 1.5 minutes."),
              ("Spear of the Rifthunter",3,Universal,"6 Power|4 Crit Chance|Fully Upgraded Bonus:8 Crit Chance"),
              ("Spiked Boneplate",3,Growth,"7 Basic armor|6 Power|Fully Upgraded Bonus:14 Basic armor"),
              ("Staff of Adamant",3,Universal,"6 Power|30 Mana|Fully Upgraded Bonus:12 Power"),
              ("Stalker's Siphon",3,Corruption,"2.5 Lifesteal|5.5 Attack Speed|Unique Passive:Basic attacks on Jungle minions,restore 8 health."),
              ("Steelcutter Gem",3,Intellect,"4 Basic penetration|30 Mana|Fully Upgraded Bonus:60 Mana"),
              ("Tempered Plate",3,Universal,"7 Basic armor|50 Health|Fully Upgraded Bonus:14 Basic armor"),
              ("Thorned Greenweave",3,Growth,"4 Ability armor|6 Power|Fully Upgraded Bonus:12 Power"),
              ("Tuned Barrier",3,Universal,"4 Ability armor|50 Health|Fully Upgraded Bonus:8 Ability armor"),
              ("Venom Shell",3,Growth,"7 Basic armor|6 Power|Fully Upgraded Bonus:12 Power"),
              ("Viper Bolt",3,Corruption,"4 Basic penetration|4 Crit Chance|Fully Upgraded Bonus:8 Crit Chance"),
              ("Vital Tap",3,Intellect,"50 Health|2.5 Lifesteal|Fully Upgraded Bonus:100 Health"),
              ("Voidsteel Dagger",3,Corruption,"6 Power|4 Basic penetration|Fully Upgraded Bonus:12 Power"),
              ("Wellspring Staff",3,Universal,"6 Power|30 Mana|Fully Upgraded Bonus:30 Mana|0.3 Mana Regen"),
              ("Whirling Wand",3,Universal,"6 Power|5.5 Attack Speed|Fully Upgraded Bonus:11 Attack Speed"),
              ("Windcarver Blade",3,Universal,"6 Power|5.5 Attack Speed|Fully Upgraded Bonus:12 Power"),
              ("Baneflesh",4,Corruption,"4 Crit Chance|50 Health|Active:For 4 seconds,when hit by a basic attack,apply a Poison stack to attacker (Max 10 stacks)."),
              ("Blight Bones",4,Growth,"50 Health|Unique Passive:When hit by a critical strike,apply Blight to the attacker (Blight reduces healing by 75)"),
              ("Bud of the Changeling",4,Growth,"50 Health|30 Mana|Unique Passive: While Poisoned,increase all Healing received by 100."),
              ("Crucial Snare",4,Intellect,"0.3 Mana Regen|4 Basic penetration|Unique Active:For 6 seconds,when hit by a basic attack,slow attackers by 250 for 2 seconds."),
              ("Curse of the Leech",4,Corruption,"6 Power|2.5 Lifesteal|Fully Upgraded Bonus:7.5 Lifesteal"),
              ("Drink the Spirits",4,Corruption,"50 Health|30 Mana|Unique Passive:On kill or assist,heal for 5 of killed unit's Health over 10 seconds."),
              ("Empyrean Mask",4,Order,"Unique Toogle:1) +18 damage 2) +10.5 and +6 Armor 3) +0.9 Mana regeneration. "),
              ("Etherweave Mesh",4,Intellect,"4 Ability armor|0.3 Mana Regen|Fully Upgraded Bonus:0.9 Mana Regen"),
              ("Hunter's Guile",4,Growth,"6 Power|50 Health|Unique Active:Removes all debuffs from your hero."),
              ("Injured Rage",4,Fury,"12 Power|Fully Upgraded Bonus:"),
              ("Lazarus Blade",4,Universal,"50 Health|Unique Passive:Basic attacks on Jungle Minions,restore 15 health."),
              ("Marrow-Fangs",4,Corruption,"2.5 Lifesteal|4 Basic penetration|Unique Passive:While Bleeding,on hit with basic attack,steal 9 armor from target for 6 seconds."),
              ("Nanoplasm",4,Intellect,"50 Health|7 Basic armor|Unique Passive:While Bleeding,gain +35 and +20 armor against critical strike."),
              ("Quantum Casing",4,Intellect,"7 Basic armor|0.3 Mana Regen|Fully Upgraded Bonus:0.6 Mana Regen|7 Basic armor"),
              ("Quenching Scales",4,Universal,"50 Health|30 Mana|Unique Passive:Grants 0.6 increased Mana Regen in a radius."),
              ("Reverberate",4,Fury,"12 Power|Unique Active:For 4 seconds,gain 50 Damage bonus against Shield."),
              ("Ring of the Domina",4,Universal,"50 Health|30 Mana|Unique Passive:+10 Damage bonus against minions."),
              ("Silent Indignation",4,Growth,"2.5 Lifesteal|Unique Active:+27.5 Attack speed for 6 seconds."),
              ("Stonetooth Heart",4,Growth,"50 Health|4 Basic penetration|Fully Upgraded Bonus:150 Health"),
              ("Swiftcreek Heart",4,Growth,"50 Health|5.5 Attack Speed|Fully Upgraded Bonus:150 Health"),
              ("Thickblood",4,Universal,"12 Power|30 Mana|Unique Passive:Hitting an enemy with an ability applies Blight for 4 seconds."),
              ("Thirstfang",4,Corruption,"6 Power|5 Lifesteal|Fully Upgraded Bonus:12 Power"),
              ("Victory Torch",4,Fury,"6 Power|0.3 Mana Regen|Fully Upgraded Bonus:"),
              ("Vineheart",4,Growth,"50 Health|2 Ability penetration|Fully Upgraded Bonus:150 Health"),
              ("Battle Mantra",5,Order,"6 Power|0.3 Mana Regen|Unique Passive:+1.5 Mana regeneration,while over 50 Mana."),
              ("Blink Charm",5,Universal,"Unique Active:Teleport Forward.|180s."),
              ("Blinkshot",5,Fury,"6 Power|Unique Active:Teleport Forward"),
              ("Bubble Tap",5,Intellect,"60 Mana|4 Ability armor|Unique Active:Consume 60 Mana,for 3 seconds gain Shield 35 of current Mana."),
              ("Charging Brute",5,Growth,"6 Power|Unique Passive:On unit kill,gain +10 Movement speed for 5 seconds.(Max +140 Movement speed)."),
              ("Chieftain's Crown",5,Growth,"1.4 Health Regen|100 Health|Unique Passive:+4.2 Health regeneration to neary allied minions."),
              ("Chrono-Mancer Disc",5,Universal,"0.3 Mana Regen|6 Power|Fully Upgraded Bonus:10 Cooldown Reduction"),
              ("Chrono-vore",5,Fury,"5.5 Attack Speed|0.3 Mana Regen|Fully Upgraded Bonus:10 Cooldown Reduction"),
              ("Chronospike",5,Universal,"30 Mana|6 Power|Fully Upgraded Bonus:10 Cooldown Reduction"),
              ("Clearheart",5,Growth,"0.3 Mana Regen|Unique Passive:+15 increase all Healing"),
              ("Cup of the Vampire",5,Corruption,"5 Lifesteal|4 Basic penetration|Unique Passive:+12.5 Lifesteal against Bleeding target."),
              ("Dreadfeast Relic",5,Corruption,"5 Lifesteal|4 Crit Chance|Active:For 6 seconds,on hit with ability,apply 12.5Life grant to enemy target for 4 seconds. Attacking an enemy marked with Life Grant gives +12.5 Lifesteal to attacker."),
              ("Firepiercer",5,Fury,"0.3 Mana Regen|Active:For 10 seconds,your next basic attack on an Enemy Hero will shred 45 of their Armor for 4 seconds."),
              ("Font of Rapture",5,Order,"1.4 Health Regen|0.3 Mana Regen|4 Crit Chance|Unique Passive:Lifesteal will overheal,granting Shield for 3 seconds,absorbs 10 damage of max Health"),
              ("Heartshot",5,Fury,"5.5 Attack Speed|Unique Passive:On Player kill,gain +280 Movement speed for 4 seconds."),
              ("Incessant Onslaught",5,Fury,"5.5 Attack Speed|Unique Passive:Basic attacks on Player,grant +10 Cooldown reduction for 4 seconds."),
              ("Jewel of the Apostle",5,Order,"0.3 Mana Regen|2.5 Lifesteal|Unique Active:For 6 seconds gain damage equal to 35 of armor."),
              ("Megavolt Pylon",5,Intellect,"60 Mana|0.3 Mana Regen|Unique Active:Restore 15 Mana for each nearby Unit."),
              ("Mending Bloom",5,Growth,"1.4 Health Regen|Unique Active:+3.8 Mana regeneration to nearby allied for 14 seconds."),
              ("Overflowing Gifts",5,Growth,"0.3 Mana Regen|Unique Active:+6 Health regeneration to nearby allied for 14 seconds."),
              ("Pain-Eater",5,Fury,"50 Health|Unique Passive:While critically wounded,gain 40 Life Steal"),
              ("Plasma Channel",5,Intellect,"60 Mana|Active:Shred 8 Armor from nearby enemues for 4 seconds. Shred value stacks based on number of nearby Enemy Heroes."),
              ("Portal Stone",5,Universal,"Unique Active:Teleport to allied structure|300s."),
              ("Rally the Troops",5,Intellect,"5.5 Attack Speed|0.3 Mana Regen|Unique Active:For 5 second +22 Attack speed to nearby allies."),
              ("Rocket Spike",5,Fury,"5.5 Attack Speed|Active:For 10 seconds,your next basic attack on an Enemy Hero will shred 45 of their Armor for 4 seconds."),
              ("Satori Cloak",5,Order,"5.5 Attack Speed|Unique Passive:On hit with basic attack,shred 50 of enemy's armor for 1.5 seconds."),
              ("Shockwave",5,Intellect,"0.6 Mana Regen|Unique Active:For 2 seconds slow nearby enemies by 250 ."),
              ("Stab Link",5,Corruption,"1.4 Health Regen|Unique Active:Consume 20 of your Base Health to Teleport Forward. Cannot use if Critically Wounded."),
              ("Stone of Salvation",5,Order,"0.3 Mana Regen|2.5 Lifesteal|Unique Active:For 6 second gain damage equal to 35 of armor."),
              ("Tainted Magic",5,Corruption,"1.4 Health Regen|0.3 Mana Regen|Active:For 6 seconds,on hit with ability,apply a stack of Poison to target (6 + 1.5*Player level)."),
              ("Tele-Blink",5,Intellect,"0.3 Mana Regen|Unique Active:Consume 75 Mana to Teleport Forward."),
              ("Thunder Cleaver",5,Universal,"5.5 Attack Speed|Unique Passive:Melee basic attacks gains an additional 50 Damage Bonus to Cleave."),
              ("Wicked Stigma",5,Corruption,"0.3 Mana Regen|50 Health|Unique Active:For 6 seconds,on hit with ability,gain vision of target for 10 seconds."),
              ("Agoran Scepter",6,Universal,"30 Mana|Fully Upgraded Bonus:100 Crit Bonus"),
              ("Ash of the Witch",6,Universal,"12 Power|Unique Passive:Enemies hit by abilities are slowed by 150 move speed for 1 second."),
              ("Blade of Agora",6,Universal,"6 Power|Fully Upgraded Bonus:100 Crit Bonus"),
              ("Blast Harness",6,Fury,"5.5 Attack Speed|Fully Upgraded Bonus:100 Crit Bonus"),
              ("Blood Catalyst",6,Intellect,"30 Mana|50 Health|Unique Active:Consume 10 of Health,for 10 seconds gain 10.5 Mana regeneration."),
              ("Bloodrite Brand",6,Fury,"4 Crit Chance|2.5 Lifesteal|Fully Upgraded Bonus:"),
              ("Bounty Stalker",6,Universal,"6 Power|50 Health|Unique Passive:Basic attacks on Players,grant a stack of +6 Critical chance for 4 seconds (Max 7 stacks)"),
              ("Cast Converter",6,Intellect,"30 Mana|0.3 Mana Regen|Unique Active:Consume 175 Mana,for 10 second gain 47.5 Health regeneration."),
              ("Deathnail",6,Corruption,"4 Basic penetration|Fully Upgraded Bonus:100 Crit Bonus"),
              ("Everglass",6,Order,"0.6 Mana Regen|Unique Active:+40 Speed for 1 second on hit with ability."),
              ("Feral Stone",6,Growth,"5.5 Attack Speed|Fully Upgraded Bonus:100 Crit Bonus"),
              ("Heart of the Apex",6,Growth,"50 Health|Fully Upgraded Bonus:100 Crit Bonus"),
              ("Honor the Pure",6,Order,"50 Health|30 Mana|Unique Active:Team Shield: Consume 100 Mana to Shield yourself and nearby Allies for 3 seconds,absorbing up to 125 + (25 * Player Level) damage. Any Ally affected by Team Shield cannot activate or benefit from another Team Shield for 15 seconds."),
              ("Hunger-Maul",6,Fury,"2.5 Lifesteal|Fully Upgraded Bonus:100 Crit Bonus"),
              ("Madspore Sash",6,Growth,"1.4 Health Regen|7 Basic armor|Unique Passive:Deal 10 damage + Player leve) per second to nearby enemies (disables while sprinting)."),
              ("Nanodrive",6,Intellect,"30 Mana|2 Ability penetration|Unique Active:For 4 seconds +20 Movement speed for each nearby Player. (Max - +320 Movement speed)."),
              ("Nature's Muse",6,Growth,"50 Health|30 Mana|Fully Upgraded Bonus:"),
              ("Necroveil",6,Corruption,"2 Ability penetration|6 Power|Unique Passive:On Player kill,enter the shadowplate for 3 seconds."),
              ("Power Chord",6,Universal,"6 Power|5.5 Attack Speed|Unique Passive:Hitting an enemy with a basic attack applies Blight for 4 seconds."),
              ("Radiant Mantle",6,Order,"50 Health|30 Mana|Unique Passive:Deal damage (10 + Player level) per second to nearby enemies (disables while sprinting)."),
              ("Scarab Claws",6,Corruption,"5.5 Attack Speed|0.3 Mana Regen|Unique Passive:On hit with basic attack,to a target with a shield shreds all of their 20 armor for 6 seconds"),
              ("Scorpion Plate",6,Growth,"50 Health|7 Basic armor|Active:For 6 seconds,on hit with basic attack,shred 2 Armor from target(s) for 4 seconds. Consecutive hits shred an additional 2 Armor and refresh duration."),
              ("Seeds of the Flood",6,Growth,"1.4 Health Regen|Fully Upgraded Bonus:100 Crit Bonus"),
              ("Sirensong",6,Corruption,"5 Lifesteal|Unique Active:For 6 seconds,on hit with basic attack,slow by 40 from target for 4 seconds."),
              ("Splinterbark Vest",6,Growth,"50 Health|4 Ability armor|Active:For 6 seconds,on hit with basic attack,shred 4 Armor from target(s) for 4 seconds. Consecutive hits shred an additional 4 Armor and refresh duration."),
              ("Stinger Boost",6,Universal,"60 Mana|11 Attack Speed|Unique Passive:After hitting an enemy hero with an ability,your next basic attack deals 50 + (20 of Power) bonus damage. Can only occur once every 1.5 seconds."),
              ("Sword of the Altar",6,Order,"2.5 Lifesteal|Fully Upgraded Bonus:100 Crit Bonus"),
              ("Tempus Pearl",6,Order,"6 Power|4 Ability armor|Unique Active:+100 Movement speed to nearby allies for 3 seconds."),
              ("Toxi-gel",6,Universal,"100 Health|Unique Passive:Taking 400 damage activates a self cleanse. Being out of combat for 5 seconds resets the counter."),
              ("Wraith Whisper",6,Corruption,"2 Ability penetration|Fully Upgraded Bonus:100 Crit Bonus"),
              ("Blackblood Virus",7,Corruption,"5 Lifesteal|Fully Upgraded Bonus:100 Crit Bonus"),
              ("Infinity Stream",7,Intellect,"0.3 Mana Regen|2 Ability penetration|Unique Passive:+2.4 Mana regeneration (scales with current Mana percent)"),
              ("Stasis Gem",7,Order,"6 Power|Unique Active:Become immune to damage for 2.5 seconds. During this time,you are unable to move,attack or use abilities."),
              ("Traitor's Touch",7,Corruption,"2.5 Lifesteal|0.3 Mana Regen|Unique Passive:Basic attacks on Player deal damage equal by 3 of their current Health."),
              ("Barrier of Will",8,Intellect,"7 Basic armor|30 Mana|Unique Active:Team Shield: Consume 100 Mana to Shield yourself and nearby Allies for 3 seconds,absorbing up to 125 + (25 * Player Level) damage. Any Ally affected by Team Shield cannot activate or benefit from another Team Shield for 15 seconds."),
              ("Divine Shield",8,Universal,"4 Ability armor|60 Mana|Unique Passive:Block an enemy ability."),
              ("Hydroverser",8,Intellect,"0.3 Mana Regen|50 Health|Unique Passive:Gain power equal to 7 of current Mana"),
              ("Purity Censer",8,Order,"30 Mana|50 Health|Unique Active:Removes all debuffs from nearby allies."),
              ("Thermo-bond",8,Universal,"100 Health|4 Ability armor|Unique Passive:Being hit by stuns,roots,and knockups grants a stack of health regen equal to 12 of your max health over 4 seconds."),
              ("Fist of Kings",10,Universal,"32 Basic penetration|150 Health"),
              ("Hand of Prophets",10,Universal,"16 Ability penetration|150 Health")]


parseBenefit :: (Fractional t) => String -> Maybe (CardSetter, t)
parseBenefit "power" = Just (power, 6)
parseBenefit "attackspeed" = Just (speed, 5.5)
parseBenefit "critchance" = Just (crit, 4)
parseBenefit "basicpenetration" = Just (pen, 4)
parseBenefit "lifesteal" = Just (lifesteal, 2.5)
parseBenefit "mana" = Just (mana, 30)
parseBenefit "manaregen" = Just (manaRegen, 0.3)
parseBenefit "health" = Just (health, 50)
parseBenefit "healthregen" = Just (healthRegen, 1.4)
parseBenefit "basicarmor" = Just (basicArmor, 7)
parseBenefit "abilityarmor" = Just (abilityArmor, 4)
parseBenefit "abilitypenetration" = Just (abilityPen, 2)
parseBenefit "critbonus" = Just (crit_bonus, 100)
parseBenefit _ = Nothing

splitBy :: (Foldable t, Eq t1) => t1 -> t t1 -> [[t1]]
splitBy delimiter = foldr f [[]]
            where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs

addPointsToCard :: Card -> String -> Maybe (CardSetter, Float) -> Card
addPointsToCard card val (Just (field, ratio)) =
  let points = truncate $ (read val :: Float) / ratio
  in field +~ points $ card
addPointsToCard card _ Nothing = card

addBenefitToCard :: String -> Card -> Card
addBenefitToCard benefitStr card =
  let (val:benefit) = splitBy ' ' benefitStr
  in
    if val == "Fully" then
      let [_,b] = splitBy ':' $ benefitStr
      in addBenefitToCard b card
    else addPointsToCard card val $ parseBenefit $ map C.toLower $ concat benefit

parseCard :: String -> Card
parseCard str =
  let d = defaultCard
      benefits = splitBy '|' str
  in foldr addBenefitToCard d benefits

allCards :: [Card]
allCards =
  map (\(n, c, a, benefitStr) ->
         let card = parseCard benefitStr
             isWard = L.isInfixOf "ward" $ map C.toLower n
             isBlink = L.isInfixOf "link" $ map C.toLower n
         in (name .~ n) .
            (cost .~ c) .
            (ward .~ if isWard then 1 else 0) .
            (blink .~ if isBlink then 1 else 0) .
            (afinity .~ a) $ card) cardTuples

bestCarryCards :: [Afinity] -> [Card]
bestCarryCards as =
  filter (\c -> (elem (c^.afinity) as || c^.afinity == Universal)
           && (c^.blink == 1
               || (c^.ward == 1 && c^.power > 1)
               || c^.crit_bonus == 1
               || (c^.cost == 3 && c^.power + c^.speed + c^.crit + c^.pen + c^.lifesteal == 4)
               || (c^.cost == 2 && c^.power + c^.speed + c^.crit + c^.pen + c^.lifesteal == 3))
         ) allCards
