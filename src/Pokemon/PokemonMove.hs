module Pokemon.PokemonMove where

import Pokemon.PokemonStat 
import Pokemon.Status (Status)

data MoveTarget = Self | Opponent deriving (Show, Read, Eq)

data MoveEffect = 
    DealDamage { power::Int, drawbackPercent:: Float }
    | ChangeStats { modifier::StatsModifer, turn::Int, moveTarget::MoveTarget } 
    | AttachStatus { attachedStatus::Status, successRate::(Int,Int), moveTarget::MoveTarget }
    deriving (Show, Read)

data Move = Move {
    moveName::String,
    moveEffect::[MoveEffect],
    accuracy::Int,
    description:: String
} deriving (Show, Read)
