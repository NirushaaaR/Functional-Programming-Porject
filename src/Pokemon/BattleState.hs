module Pokemon.BattleState where

import System.Random
import Control.Monad (foldM)
import Control.Monad.Writer

import Pokemon.PokemonInfo (PokemonInfo (..), takeDamage, changeStat, calculateMoveDamage)
import Pokemon.PokemonStat (PokemonStats (..), StatsModifer (StatsModifer), modifierChangeDescription)
import Pokemon.PokemonMove (Move (..), MoveEffect (..), MoveTarget (Self, Opponent), Log (..))
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

attackerUseMove :: BattleState -> Int -> Writer [Log] BattleState
attackerUseMove bs@(BattleState at _ _) moveIndex = do
    let move = (moves $ fst at) !! (moveIndex-1)
    (bs', move') <- checkBeforeUseMove bs move
    case move' of   
        Nothing -> return bs'
        Just m -> do
            let (Move mn effects macc _) = m
                (BattleState at' df' g') = bs'
            tell [NormalLog $ (name $ fst at') ++ " use "++ mn]
            let (trigger, newGen) = randomTrigger g' (macc, 100)
                newBs = (BattleState at' df' newGen)
            if trigger then foldM (\bstate f -> f bstate) newBs (map (flip actionByMoveEffect) effects)
            else do
                tell [NormalLog $ "But it missed..."]
                return newBs


takeStatusEffect :: Status -> PokemonInfo -> Writer [Log] PokemonInfo
takeStatusEffect st pkInfo = 
    if st == Poison || st == Burn then do
        let damage = mulceling (maxHp $ stats pkInfo) (1/8)
        tell [StatusLog st ((name pkInfo)++" is hurt by "++(show st)) Self]
        tell [DamageLog damage Self]
        return $ (takeDamage pkInfo damage)
    else return (pkInfo)


-- Attacker use move that Dealdamage
actionByMoveEffect :: BattleState -> MoveEffect ->  Writer [Log] BattleState
actionByMoveEffect bs (DealDamage p drawback) = do
    if Flying `elem` (snd $ defender bs) then do
        tell [NormalLog $ "Can't Deal Damage, "++(show $ name $ fst $ defender bs)++" is Flying"]
        return bs
    else do
            let damage = calculateMoveDamage p (fst $ attacker bs) (fst $ defender bs)
            bs2 <- attackerAttack bs damage Opponent
            if drawback /= 0.0 then do
                let drawbackDamage = mulceling damage drawback
                attackerAttack bs drawbackDamage Self
            else return bs2
            where
                attackerAttack :: BattleState -> Int -> MoveTarget -> Writer [Log] BattleState
                attackerAttack (BattleState (atker, atkerSt) (dfder, dfderSt) g) damage target = do
                    tell [DamageLog damage target] 
                    case target of  
                        Self -> let newAtker = takeDamage atker damage
                            in return (BattleState (newAtker, atkerSt) (dfder, dfderSt) g)
                        Opponent -> let newDfder = takeDamage dfder damage
                            in return (BattleState (atker, atkerSt) (newDfder, dfderSt) g)

-- Attacker use move that ChangeStats
actionByMoveEffect bs (ChangeStats mod target) = attackerChangeStats bs mod target
    where
        attackerChangeStats :: BattleState -> StatsModifer -> MoveTarget -> Writer [Log] BattleState
        attackerChangeStats (BattleState (atker, atkerSt) (dfder, dfderSt) g) modifier target = do
            tell $ map (\s -> StatsLog s target) (modifierChangeDescription modifier)
            case target of  
                Self -> do
                    let newAtker = changeStat atker modifier
                    return (BattleState (newAtker, atkerSt) (dfder, dfderSt) g)
                Opponent -> do
                    let newDfder = changeStat dfder modifier
                    return (BattleState (atker, atkerSt) (newDfder, dfderSt) g)

-- Attacker use move that AttachStatus
actionByMoveEffect bs@(BattleState atker dfder g) (AttachStatus st rate target) = do
    let (trigger, newGen) = randomTrigger g rate
    if trigger then attackerSetStatus bs st target
    else do
        tell [(StatusLog st ("Failed to attach "++(show st)) target)]
        return (BattleState atker dfder newGen)
        where
            attackerSetStatus :: BattleState -> Status -> MoveTarget -> Writer [Log] BattleState
            attackerSetStatus (BattleState (atker, atkerSt) (dfder, dfderSt) g) st target = do
                (newBs, isSet', pkName) <- return $ case target of  
                    Self -> let (newAtkerSt, isSet) = setStatus atkerSt st
                        in (BattleState (atker, newAtkerSt) (dfder, dfderSt) g, isSet, name atker)
                    Opponent -> let (newDfderSt, isSet) = setStatus dfderSt st
                        in (BattleState (atker, atkerSt) (dfder, newDfderSt) g, isSet, name dfder)
                if (isSet') then tell [StatusLog st (pkName++(statusAttachedDescription st)) target]
                else tell [StatusLog st (pkName++" is already "++(show st)) target]
                return newBs


canChooseMove :: PokemonState -> Bool
canChooseMove (_, pkSt) = 
    if Flying `elem` pkSt then False
    else True

-- check if the attacker can actually use move
checkBeforeUseMove :: BattleState -> Move -> Writer [Log] (BattleState, Maybe Move)
checkBeforeUseMove bs@(BattleState atker dfder g) move = do
    -- when paralyzed have 25% that can't to use move
    if Paralyzed `elem` (snd atker) then do
        let (trigger, newGen) = randomTrigger g (25, 100)
        if trigger then do
            (tell [NormalLog $ (name $ fst atker)++" is Paralyzed and can't move"])
            return (BattleState atker dfder newGen, Nothing)
        else return (bs, Just move)
    -- when flying this turn will use fly attack move instead and remove Flying status
    else if Flying `elem` (snd atker) then do
        let flyAttackMove = (Move "Fly Attack" [DealDamage 20 0.0] 100 "Attack From the Sky")
            filterOutFlyStatus = filter (\s -> s /= Flying) (snd atker)
        tell [NormalLog $ (name $ fst atker)++" is hovering above the sky"]
        return ((BattleState (fst atker, filterOutFlyStatus) dfder g), Just flyAttackMove)
    else return (bs, Just move)
