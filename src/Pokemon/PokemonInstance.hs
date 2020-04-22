module Pokemon.PokemonInstance (
    charizard,
    blastoise,
    pikachu
) where

import Pokemon.PokemonInfo (PokemonInfo (..))
import Pokemon.PokemonMove (Move (..), MoveEffect (..), MoveTarget (..))
import Pokemon.PokemonType (Type (..))
import Pokemon.PokemonStat (PokemonStats (PokemonStats), StatsModifer (StatsModifer))
import Pokemon.Status (Status (..))

-- Charizard --
charizard = PokemonInfo {
    name="Charizard",
    stats=(PokemonStats 50 50 12 9 15),
    pokemonType=Fire,
    moves=[flameThrowerMove, swordDanceMove, flyMove]
}
flameThrowerMove = Move {
    moveName="FlameThrower",
    moveEffect=[(DealDamage 25 0.0), (AttachStatus Burn (1,3) Opponent)],
    accuracy=100, 
    description="A powerfull fire move has a 1/3 chance to attach burn to op"
}
swordDanceMove = Move { 
    moveName="SwordDance", 
    moveEffect=[(ChangeStats (StatsModifer 0 0.5 0 0) 10 Self)],
    accuracy=100, 
    description="A move that increase user attack by 1.5 percent"
}
flyMove = Move {
    moveName="Fly",
    moveEffect=[(AttachStatus Flying (1,1) Self)], 
    accuracy=100,
    description="A very powerful move that also hurt attacker by 50% of damage deal to opponent"
}
-- Charizard --

-- blastoise --
blastoise = PokemonInfo {
    name="Blastoise",
    stats=(PokemonStats 61 61 10 15 8),
    pokemonType=Water,
    moves=[hydroPumpMove, shellSmashMove, aquaTailMove]
}
hydroPumpMove = Move {
    moveName="HydroPump",
    moveEffect=[(DealDamage 35 0.0)],
    accuracy=50,
    description="The target is blasted by a huge volume of water launched under great pressure"
}
shellSmashMove = Move {
    moveName="Shell Smash",
    moveEffect=[(ChangeStats (StatsModifer 0 0.5 (-0.5) 0.5) 10 Self)],
    accuracy=100,
    description="The user breaks its shell, which lowers Defense stat but raises its Attack and Speed stats"
}
aquaTailMove = Move {
    moveName="Aqua Tail",
    moveEffect=[(DealDamage 25 0.0)],
    accuracy=100,
    description="The user attacks by swinging its tail as if it were a vicious wave in a raging storm"
}
-- blastoise --

-- pikachu--
pikachu = PokemonInfo {
    name="Pikachu",
    stats=(PokemonStats 56 56 14 10 16),
    pokemonType=Electric,
    moves=[electroBall, voltTackle, thunderWave]
}
electroBall = Move {
    moveName="Electro Ball",
    moveEffect=[(DealDamage 30 0.0)],
    accuracy=100,
    description="The user hurls an electric orb at the target. The faster the user is than the target, the greater the damage."
}
voltTackle = Move {
    moveName="volt Tackle",
    moveEffect=[(DealDamage 40 0.5)],
    accuracy=70,
    description="The user throws an electrified tackle. It hurts the user a little."
}
thunderWave = Move {
    moveName="Thuner Wave",
    moveEffect=[(AttachStatus Paralyzed (1,3)) Opponent],
    accuracy=100,
    description="A weak electric shock that is sure to cause paralysis if it hits."
}
-- pikachu--





