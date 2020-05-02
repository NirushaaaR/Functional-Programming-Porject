module Pokemon.PokemonInfo where

import Util (mulceling)

import Pokemon.PokemonStat (PokemonStats (..), StatsModifer, currentHp, changeHpStat, modifyStats)
import Pokemon.PokemonType (Type)
import Pokemon.PokemonMove (Move)

data PokemonInfo = PokemonInfo {
    name::String,
    stats::PokemonStats,
    pokemonType::Type,
    moves :: [Move]
} deriving (Show, Read)

isDead :: PokemonInfo -> Bool
isDead p = (currentHp (stats p)) <= 0

takeDamage :: PokemonInfo -> Int -> PokemonInfo
takeDamage (PokemonInfo n stat pt pm) damage =
    let newStat = changeHpStat stat (currentHp stat - damage)
    in PokemonInfo n newStat pt pm

changeStat :: PokemonInfo -> StatsModifer -> PokemonInfo
changeStat (PokemonInfo n stat pt pm) modifier =
    let newStat = modifyStats stat modifier
    in PokemonInfo n newStat pt pm


calculateMoveDamage :: Int -> PokemonInfo -> PokemonInfo -> Int
calculateMoveDamage movePower attacker defender = 
    let attackerAtk = atk $ stats attacker
        defenderDef = def $ stats defender
    in movePower + ((attackerAtk - defenderDef) * 2)