module GamePlay where

import System.Random
import Text.Read
import Control.Monad.Writer

import Pokemon.PokemonInfo (PokemonInfo (..), isDead)
import Pokemon.PokemonMove (Move (..), MoveLogs (..), MoveTarget (..))
import Pokemon.BattleState
import Pokemon.PokemonInstance 
import Util (AttackTurn (..))
import DrawScreen (drawGameState)


type PlayerMoveAndEnemyMove = (Int, Int)

data GamePlayState = GamePlayState {
    playerState :: PokemonState,
    enemyState :: PokemonState,
    gamePlayGen :: StdGen
} deriving (Show, Read)

playerWin :: GamePlayState -> Maybe Bool
playerWin (GamePlayState p e g) =
    if not ((isDead $ fst p ) || (isDead $ fst e)) then Nothing
    else Just (isDead $ fst e)


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
    (playerFst, g'') <- return $ playerAttackFirst player enemy g'
    
    -- play battle
    eitherGameEnd <- playBattle (GamePlayState player enemy g'') chosenMove (if playerFst then Player else Enemy)

    -- game End ? return if Player Won : game not end ? continue playing with new BattleState
    case eitherGameEnd of 
        Right m -> return m
        Left newGameState -> continueGamePlay newGameState


playBattle :: GamePlayState -> PlayerMoveAndEnemyMove -> AttackTurn -> IO (Either GamePlayState Bool)
playBattle (GamePlayState player enemy g) (playerMv, enemyMv) firstTurn = do
    -- play first turn
    let fstBs = if firstTurn == Player then (BattleState player enemy g, playerMv)
             else (BattleState enemy player g, enemyMv)
    gs@(GamePlayState player' enemy' g') <- turnAction firstTurn fstBs
    case playerWin gs of 
        Just m -> return $ Right m
        -- if not end yet play seccond turn
        Nothing -> do
            let (sndBs, sndTurn) = if firstTurn == Player then ((BattleState enemy' player' g', enemyMv), Enemy)
                                    else ((BattleState player' enemy' g', playerMv), Player)
            gs' <- turnAction sndTurn sndBs
            -- when everyone already end turn
            case playerWin gs' of 
                Just m -> return $ Right m
                Nothing -> do
                    gs'' <- endTurnAction gs'
                    case playerWin gs'' of
                        Just m -> return $ Right m
                        Nothing -> return $ Left gs''


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
                    putStrLn $ showPokemonMove (moves $ fst playerPk)
                    getPlayerChoosenMove (1,length $ moves $ fst playerPk)
                  else return 0
    -- choose Move for Enemy
    let (enemyMove, stdGen') = randomR (1, (length $ moves $ fst $ enemyPk)) stdGen 
    return ((playerMove, enemyMove), stdGen')


showPokemonMove :: [Move] -> String
showPokemonMove moveList = concat $ map (\(m, index) -> "  ["++(show index)++"] "++ moveName m ) (zip moveList [1..])

getPlayerChoosenMove :: (Int,Int) -> IO Int
getPlayerChoosenMove (minRange, maxRange) = do
    line <- getLine
    case strToInt line of
        Nothing -> getPlayerChoosenMove (minRange, maxRange)
        Just n -> if n < minRange || n > maxRange then getPlayerChoosenMove (minRange, maxRange) else return n


strToInt :: String -> Maybe Int
strToInt = readMaybe

drawGameState' :: GamePlayState -> [MoveLogs] -> AttackTurn -> IO ()
drawGameState' gs mlogs turn = do
    drawGameState (playerState gs) (enemyState gs) mlogs turn
    putStrLn "Press Enter to continue"
    _ <- getLine
    return ()

-- implement while loop
-- if Nothing Will Continue Doing, if Just a will stop with results a
-- battleCheck :: Monad m => GamePlayState -> m (Maybe Bool) -> m Bool
-- battleCheck gs action = do
--     condition <- action gs
--     case condition of Nothing -> (loopMaybe action)
--                       Just m -> return m