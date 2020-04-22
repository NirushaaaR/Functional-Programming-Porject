module Pokemon.PokemonMove where

import Data.List (partition)

import Pokemon.PokemonStat 
import Pokemon.Status (Status)
import Util (AttackTurn (..))

data MoveTarget = Self | Opponent deriving (Show, Read, Eq)

data MoveEffect = 
    DealDamage { power::Int, drawbackPercent:: Float }
    | ChangeStats { modifier::StatsModifer, moveTarget::MoveTarget } 
    | AttachStatus { attachedStatus::Status, successRate::(Int,Int), moveTarget::MoveTarget }
    deriving (Show, Read)

data Move = Move {
    moveName::String,
    moveEffect::[MoveEffect],
    accuracy::Int,
    description:: String
} deriving (Show, Read)

-- use for writer to log data
data Log = 
    DamageLog { damageTaken::Int, targetLog::MoveTarget }
    | StatsLog { change::String, targetLog::MoveTarget }
    | StatusLog { statusAffect::Status, desc::String, targetLog::MoveTarget }
    | NormalLog String
    deriving (Show)


splitLogs :: [Log] -> AttackTurn -> ([Log], [Log])
splitLogs logs turn = partition isPlayerLog logs
    where
    isPlayerLog log =
        if turn == Player then
            case log of DamageLog _ target -> target == Self
                        StatsLog _ target -> target == Self
                        StatusLog _ _ target -> target == Self
                        NormalLog _ -> True
        else 
            case log of DamageLog _ target -> target == Opponent
                        StatsLog _ target -> target == Opponent
                        StatusLog _ _ target -> target == Opponent
                        NormalLog _ -> False

logToStr :: Log -> String
logToStr (DamageLog damage _) = 
    if damage < 0 then "Heal "++(show damage)++" Hp"
    else "Take "++(show damage)++" damage"
logToStr (StatsLog change' target) = (show target) ++ ": "++change'
logToStr (StatusLog statusAffect desc' _) = desc'
logToStr (NormalLog l) = l