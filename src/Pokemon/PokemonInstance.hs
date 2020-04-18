module Pokemon.PokemonInstance (
    charizard,
    blastoise
) where

import Pokemon.PokemonInfo (PokemonInfo (PokemonInfo))
import Pokemon.PokemonMove (Move (Move), MoveEffect (..), MoveTarget (..))
import Pokemon.PokemonType (Type (..))
import Pokemon.PokemonStat (PokemonStats (PokemonStats), StatsModifer (StatsModifer))
import Pokemon.Status (Status (..))

charizard = PokemonInfo "Charizard" (PokemonStats 50 50 12 9 15) Fire [flameThrowerMove, swordDanceMove, flyMove]
flameThrowerMove = Move 
    "FlameThrower" 
    [(DealDamage 25 0.0), (AttachStatus Burn (1,3) Opponent)] 
    100 
    "A powerfull fire move has a 1/3 chance to attach burn to op"

swordDanceMove = Move 
    "SwordDance" 
    [(ChangeStats (StatsModifer 0 0.5 0 0) 10 Self)] 
    100 
    "A move that increase user attack by 1.5 percent"

flyMove = Move 
    "Fly" 
    [(AttachStatus Flying (1,1) Self)] 
    100
    "A very powerful move that also hurt attacker by 50% of damage deal to opponent"


blastoise = PokemonInfo "Blastoise" (PokemonStats 61 61 10 15 8) Water [hydroPumpMove, shellSmashMove, aquaTailMove]
hydroPumpMove = Move
    "HydroPump"
    [(DealDamage 35 0.0)]
    50
    "The target is blasted by a huge volume of water launched under great pressure"
shellSmashMove = Move
    "Shell Smash"
    [(ChangeStats (StatsModifer 0 0.5 (-0.5) 0.5) 10 Self)]
    100
    "The user breaks its shell, which lowers Defense stat but raises its Attack and Speed stats"
aquaTailMove = Move
    "Aqua Tail"
    [(DealDamage 25 0.0)]
    100
    "The user attacks by swinging its tail as if it were a vicious wave in a raging storm"