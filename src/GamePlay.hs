module GamePlay where

import System.Random
import Text.Read
import Control.Monad.Writer

import Pokemon.PokemonInfo (PokemonInfo (..), isDead)
import Pokemon.PokemonMove (Move (..), Log (..), MoveTarget (..))
import Pokemon.PokemonStat (speed)
import Pokemon.BattleState
import Pokemon.PokemonInstance 
import Util (AttackTurn (..), randomTrigger)
import DrawScreen (drawGameState)


type PlayerMoveAndEnemyMove = (Int, Int)

data GamePlayState = GamePlayState {
    playerState :: PokemonState,
    enemyState :: PokemonState,
    gamePlayGen :: StdGen
} deriving (Show, Read)

isPlayerWin :: GamePlayState -> Maybe Bool
isPlayerWin (GamePlayState p e g) =
    if not ((isDead $ fst p ) || (isDead $ fst e)) then Nothing
    else Just (isDead $ fst e)


-- playerWinOrGameNotEnd :: GamePlayState -> Either GamePlayState Bool
-- isPlayerWin (GamePlayState p e g) =
--     if not ((isDead $ fst p ) || (isDead $ fst e)) then Nothing
--     else Just (isDead $ fst e)


mainGamePlay :: IO ()
mainGamePlay = do
    stdGen <- getStdGen
    let gamePlayState = GamePlayState {
        playerState = (pikachu, []),
        enemyState = (blastoise, []),
        gamePlayGen = stdGen
    }
    putStrLn $ "Battle Between" ++ (name $ fst $ playerState gamePlayState) ++ " and " ++ (name $ fst $ enemyState gamePlayState)
    drawGameState (playerState gamePlayState) (enemyState gamePlayState) [] Player
    win <- continueGamePlay gamePlayState
    if win then putStrLn "Player Win!!"
    else putStrLn "Player Lose!!"
    return ()

continueGamePlay :: GamePlayState -> IO Bool
continueGamePlay (GamePlayState player enemy g) = do
    -- Choose Move for player and Enemy
    (chosenMove, g') <- chooseMove player enemy g

    -- determine who go first
    (playerFst, g'') <- return $ checkAttackFirst player enemy g'
    
    -- play battle
    eitherGameEnd <- playBattle (GamePlayState player enemy g'') chosenMove (if playerFst then Player else Enemy)

    -- game End ? return if Player Won : game not end ? continue playing with new BattleState
    case eitherGameEnd of 
        Right m -> return m
        Left newGameState -> continueGamePlay newGameState


playBattle :: GamePlayState -> PlayerMoveAndEnemyMove -> AttackTurn -> IO (Either GamePlayState Bool)
playBattle (GamePlayState player enemy g) (playerMv, enemyMv) firstTurn = do
    -- play first turn
    gs@(GamePlayState player' enemy' g') <- if firstTurn == Player then playerAction (BattleState player enemy g, playerMv)
                                            else enemyAction (BattleState enemy player g, enemyMv)
    case isPlayerWin gs of 
        Just m -> return $ Right m
        -- if not end yet play seccond turn
        Nothing -> do
            gs' <-  if firstTurn == Player then enemyAction (BattleState enemy' player' g', enemyMv)
                    else playerAction (BattleState player' enemy' g', playerMv)
            -- when everyone already end turn
            case isPlayerWin gs' of 
                Just m -> return $ Right m
                Nothing -> do
                    gs'' <- endTurnAction gs'
                    case isPlayerWin gs'' of
                        Just m -> return $ Right m
                        Nothing -> return $ Left gs''


playerAction :: (BattleState, Int) -> IO GamePlayState
playerAction = turnAction Player 

enemyAction :: (BattleState, Int) -> IO GamePlayState
enemyAction = turnAction Enemy      

turnAction :: AttackTurn -> (BattleState, Int) -> IO GamePlayState
turnAction turn (bs, mIdx) = do
    (bs', logs) <- return $ runWriter $ attackerUseMove bs mIdx
    let newGs = GamePlayState player enemy (gen bs')
        (player, enemy) = if turn == Player then (attacker bs', defender bs') else (defender bs', attacker bs')
    drawGameState' newGs logs turn
    return newGs


endTurnAction :: GamePlayState -> IO GamePlayState
endTurnAction (GamePlayState (player, playerSt) (enemy,enemySt) g) = do
    (player', logs1) <- return $ runWriter $ foldM (\at f -> f at) (player) (map takeStatusEffect playerSt)
    when (not $ null logs1) $ drawGameState' (GamePlayState (player', playerSt) (enemy,enemySt) g) logs1 Player
    (enemy', logs2) <- return $ runWriter $ foldM (\at f -> f at) (enemy) (map takeStatusEffect enemySt)
    let newGs = (GamePlayState (player', playerSt) (enemy', enemySt) g)
    when (not $ null logs2) $ drawGameState' newGs logs2 Enemy
    return newGs



chooseMove :: PokemonState -> PokemonState -> StdGen -> IO (PlayerMoveAndEnemyMove, StdGen)
chooseMove playerPk enemyPk stdGen = do
    -- Choose Move for player
    playerChooseMove <- return $ canChooseMove playerPk
    playerMove <- if playerChooseMove then do
                    putStr "Choose Your Move: "
                    showPokemonMove (moves $ fst playerPk)
                    choosePlayerMove (1,length $ moves $ fst playerPk)
                  else return 0
    -- choose Move for Enemy
    let (enemyMove, stdGen') = randomR (1, (length $ moves $ fst $ enemyPk)) stdGen 
    return ((playerMove, enemyMove), stdGen')


showPokemonMove :: [Move] -> IO ()
showPokemonMove moveList = do
    putStrLn $ concat $ map (\(m, index) -> "  ["++(show index)++"] "++ moveName m ) (zip moveList [1..])


choosePlayerMove :: (Int,Int) -> IO Int
choosePlayerMove (minRange, maxRange) = do
    line <- getLine
    case readMaybe line :: Maybe Int of
        Nothing -> choosePlayerMove (minRange, maxRange)
        Just n -> if n < minRange || n > maxRange then choosePlayerMove (minRange, maxRange) else return n

drawGameState' :: GamePlayState -> [Log] -> AttackTurn -> IO ()
drawGameState' (GamePlayState player enemy _) logs turn = do
    drawGameState player enemy logs turn
    putStrLn "Press Enter to continue"
    _ <- getLine
    return ()

-- see if player have a chance to attack first
checkAttackFirst :: PokemonState -> PokemonState -> StdGen -> (Bool, StdGen)
checkAttackFirst p1@(pk1, _) p2@(pk2, _) g =
    -- if speed equal random 50 : 50 
    if (speed $ stats pk1) == (speed $ stats pk2) then
        let (trigger, newGen) = randomTrigger g (50,100) in (trigger, newGen)
    else ((speed $ stats pk1) > (speed $ stats pk2), g)