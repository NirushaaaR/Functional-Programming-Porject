module Pokemon.BattleState where

import System.Random
import Control.Monad (foldM)
import Control.Monad.Writer

import Pokemon.PokemonInfo (PokemonInfo (..), takeDamage, changeStat, calculateMoveDamage)
import Pokemon.PokemonStat (PokemonStats (..), StatsModifer (StatsModifer), modifierChangeDescription)
import Pokemon.PokemonMove (Move (..), MoveEffect (..), MoveTarget (Self, Opponent))
import Pokemon.Status
import Pokemon.PokemonType
import Util (mulceling, randomTrigger)

type PokemonState = (PokemonInfo, [Status])

data BattleState = BattleState {
    attacker :: PokemonState,
    defender :: PokemonState,
    gen :: StdGen
} deriving (Show, Read)

printBattleState :: BattleState -> IO ()
printBattleState (BattleState (at,atst) (df,dfst) _) = do
    putStrLn $ (name at)++" Hp "++ (show $ currentHp $ stats at)
    print atst
    putStrLn $ (name df)++" Hp "++ (show $ currentHp $ stats df)
    print dfst

attackerUseMove :: BattleState -> Int -> Writer [String] BattleState
attackerUseMove bs@(BattleState at _ _) moveIndex = do
    let move = (moves $ fst at) !! (moveIndex-1)
    (bs', move') <- checkBeforeAttack bs move
    case move' of   
        Nothing -> do
            tell [(name $ fst at)++" failed to use move"]
            return bs'
        Just m -> do
            let (Move mn effects macc _) = m
                (BattleState at' df' g') = bs'
            tell [(name $ fst at') ++ " use "++ mn]
            let (trigger, newGen) = randomTrigger g' (macc, 100)
                newBs = (BattleState at' df' newGen)
            if trigger then foldM (\bstate f -> f bstate) newBs (map (flip actionByMoveEffect) effects)
            else do
                tell ["But it missed..."]
                return newBs


afterAllAttack :: BattleState -> Writer [String] BattleState
afterAllAttack (BattleState (atker,atkerSt) (dfder, dfderSt) g) = do
    (atker') <- foldM (\at f -> f at) (atker) (map takeStatusEffect atkerSt)
    (dfder') <- foldM (\at f -> f at) (dfder) (map takeStatusEffect dfderSt)
    return (BattleState (atker',atkerSt) (dfder', dfderSt) g)



takeStatusEffect :: Status -> PokemonInfo -> Writer [String] PokemonInfo
takeStatusEffect st pkInfo = 
    if st == Poison || st == Burn then do
        let damage = mulceling (maxHp $ stats pkInfo) (1/8)
        tell [(name pkInfo) ++ " take " ++ (show damage)++ " " ++(show st) ++ " damage"]
        return $ (takeDamage pkInfo damage)
    else return (pkInfo)



actionByMoveEffect :: BattleState -> MoveEffect ->  Writer [String] BattleState
actionByMoveEffect bs (DealDamage p drawback) = do
    if Flying `elem` (snd $ defender bs) then do
        tell ["Can't Deal Damage, Oppennet is Flying"]
        return bs
    else do
            let damage = calculateMoveDamage p (fst $ attacker bs) (fst $ defender bs)
            bs2 <- attackerAttack bs damage Opponent
            if drawback /= 0.0 then do
                let drawbackDamage = mulceling damage drawback
                attackerAttack bs drawbackDamage Self
            else return bs2

actionByMoveEffect bs (ChangeStats mod t target) = attackerChangeStats bs mod target

actionByMoveEffect bs@(BattleState atker dfder g) (AttachStatus st rate target) = do
    let (trigger, newGen) = randomTrigger g rate
    if trigger then attackerSetStatus bs st target
    else return (BattleState atker dfder newGen)
        

attackerAttack :: BattleState -> Int -> MoveTarget -> Writer [String] BattleState
attackerAttack (BattleState (atker, atkerSt) (dfder, dfderSt) g) damage target = do
    if damage >= 0 then tell [(name atker) ++ " deal "++ (show damage) ++ " damage to "++ show target] 
    else tell [(name atker) ++ " heal "++ (show damage) ++ " hp to "++ show target]
    case target of  
        Self -> do
            let newAtker = takeDamage atker damage
            return (BattleState (newAtker, atkerSt) (dfder, dfderSt) g)
        Opponent -> do 
            let newDfder = takeDamage dfder damage
            return (BattleState (atker, atkerSt) (newDfder, dfderSt) g)


attackerChangeStats :: BattleState -> StatsModifer -> MoveTarget -> Writer [String] BattleState
attackerChangeStats (BattleState (atker, atkerSt) (dfder, dfderSt) g) modifier target = do
    tell [(name atker) ++ " change "++(show target)++" status"]
    tell (modifierChangeDescription modifier)
    case target of  
        Self -> do
            let newAtker = changeStat atker modifier
            return (BattleState (newAtker, atkerSt) (dfder, dfderSt) g)
        Opponent -> do
            let newDfder = changeStat dfder modifier
            return (BattleState (atker, atkerSt) (newDfder, dfderSt) g)



attackerSetStatus :: BattleState -> Status -> MoveTarget -> Writer [String] BattleState
attackerSetStatus (BattleState (atker, atkerSt) (dfder, dfderSt) g) st target =
    case target of  
        Self -> do
            let (newAtkerSt, isSet) = setStatus atkerSt st
            if isSet then tell [(name atker) ++ " is " ++ (show st)]
            else tell ["set "++(show st)++" to " ++ (show target)++" Failed"]
            return (BattleState (atker, newAtkerSt) (dfder, dfderSt) g)
        Opponent -> do
            let (newDfderSt, isSet) = setStatus dfderSt st
            if isSet then tell [(name dfder) ++ " is " ++ (show st)]
            else tell ["set "++(show st)++ " to "++(show target)++" Failed"]
            return (BattleState (atker, atkerSt) (dfder, newDfderSt) g)


canChooseMove :: PokemonState -> Bool
canChooseMove (pkInfo, pkSt) = 
    if Flying `elem` pkSt then False
    else True

checkBeforeAttack :: BattleState -> Move -> Writer [String] (BattleState, Maybe Move)
checkBeforeAttack bs@(BattleState atker dfder g) move = do
    if Paralyzed `elem` (snd atker) then do
        let (trigger, newGen) = randomTrigger g (25, 100)
        if trigger then tell [(name $ fst atker)++" is Paralyzed and can't move"]
        else tell [(name $ fst atker)++" is Paralyzed but can still move"]
        return (BattleState atker dfder newGen, Nothing)
    else if Flying `elem` (snd atker) then do
        let flyAttackMove = Move "Fly Attack" [DealDamage 20 0.0] 100 "Attack From the Sky"
            filterOutFlyStatus = filter (\s -> s /= Flying) (snd atker)
        tell [(name $ fst atker)++" is attacking From The Sky"]
        return ((BattleState (fst atker, filterOutFlyStatus) dfder g), Just flyAttackMove)
    else return (bs, Just move)


playerAttackFirst :: PokemonState -> PokemonState -> StdGen -> (Bool, StdGen)
playerAttackFirst p1@(pk1, _) p2@(pk2, _) g =
    if (speed $ stats pk1) == (speed $ stats pk2) then
        let (trigger, newGen) = randomTrigger g (50,100) in (trigger, newGen)
    else ((speed $ stats pk1) > (speed $ stats pk2), g)



-- bs = BattleState (blastoise, []) (charizard, []) (mkStdGen 100)
-- runWriter $ attackerUseMove bs 1