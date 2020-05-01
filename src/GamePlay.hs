module GamePlay where

import System.Random
import System.IO
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)
import Data.Char (toLower)
import Control.Monad.Writer

import Pokemon.PokemonInfo (PokemonInfo (..), isDead)
import Pokemon.PokemonMove (Move (..), Log (..), MoveTarget (..))
import Pokemon.PokemonStat (speed)
import Pokemon.BattleState
import Pokemon.PokemonInstance 
import Util (AttackTurn (..), randomTrigger, fromUnicode, toUnicode)
import DrawScreen (drawGameState)


type PlayerMoveAndEnemyMove = (Int, Int)

data GamePlayState = GamePlayState {
    playerState :: PokemonState,
    enemyState :: PokemonState,
    gamePlayGen :: StdGen
} deriving (Show, Read)

playerWinOrGameNotEnd :: GamePlayState -> Either GamePlayState Bool
playerWinOrGameNotEnd gs@(GamePlayState p e g) =
    if not ((isDead $ fst p ) || (isDead $ fst e)) then Left gs
    else Right (isDead $ fst e)


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
continueGamePlay gs = do
    -- savegame or loadgame
    gs' <- saveOrLoad gs
    let (GamePlayState player enemy g) = gs'

    drawGameState player enemy [] Player

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
playBattle gs (playerMv, enemyMv) firstTurn = 
    -- play first turn
    if firstTurn == Player 
        then foldM (\currentGs f-> f currentGs) (Left gs) [(playerAction playerMv), (enemyAction enemyMv), endTurnAction]
    else 
        foldM (\currentGs f -> f currentGs) (Left gs) [(enemyAction enemyMv), (playerAction playerMv), endTurnAction]


playerAction :: Int -> Either GamePlayState Bool -> IO (Either GamePlayState Bool)
playerAction _ (Right w) = return $ Right w 
playerAction moveIndex (Left gs) = 
    turnAction Player moveIndex (BattleState (playerState gs) (enemyState gs) (gamePlayGen gs))

enemyAction :: Int -> Either GamePlayState Bool -> IO (Either GamePlayState Bool)
enemyAction _ (Right w) = return $ Right w      
enemyAction moveIndex (Left gs) = 
    turnAction Enemy moveIndex (BattleState (enemyState gs) (playerState gs) (gamePlayGen gs))

turnAction :: AttackTurn -> Int -> BattleState -> IO (Either GamePlayState Bool)
turnAction turn moveIndex bs = do
    (bs', logs) <- return $ runWriter $ attackerUseMove bs moveIndex
    let newGs = GamePlayState player enemy (gen bs')
        (player, enemy) = if turn == Player then (attacker bs', defender bs') else (defender bs', attacker bs')
    drawGameState' newGs logs turn
    return $ playerWinOrGameNotEnd newGs

endTurnAction :: Either GamePlayState Bool -> IO (Either GamePlayState Bool)
endTurnAction (Right w) = return $ Right w
endTurnAction (Left (GamePlayState (player, playerSt) (enemy,enemySt) g)) = do
    (player', logs1) <- return $ runWriter $ foldM (\at f -> f at) (player) (map takeStatusEffect playerSt)
    when (not $ null logs1) $ drawGameState' (GamePlayState (player', playerSt) (enemy,enemySt) g) logs1 Player
    (enemy', logs2) <- return $ runWriter $ foldM (\at f -> f at) (enemy) (map takeStatusEffect enemySt)
    let newGs = (GamePlayState (player', playerSt) (enemy', enemySt) g)
    when (not $ null logs2) $ drawGameState' newGs logs2 Enemy
    return $ playerWinOrGameNotEnd newGs


saveOrLoad :: GamePlayState -> IO GamePlayState
saveOrLoad gs = do
    putStrLn "type (save) or type (load) game here, otherwise continue:"
    command <- fmap (map toLower) getLine
    if command == "save" then do
        save "save.dat" gs >>= putStrLn
        saveOrLoad gs
    else if command == "load" then do
        result <- load "save.dat"
        case result of 
            Right loaded -> do
                putStrLn "load file successfully"
                newGen <- newStdGen
                let gs' = (GamePlayState (playerState loaded) (enemyState loaded) newGen)
                saveOrLoad gs'
            Left err -> do
                putStrLn err
                saveOrLoad gs
    else return gs


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
    command <- fmap (map toLower) getLine
    case readMaybe command :: Maybe Int of
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


----------- Save & Load -------------------
save :: String -> GamePlayState -> IO String
save filePath gs = do
    catchIOError (
        do
            let unicode = toUnicode (show gs)
            writeFile filePath (show unicode)
            return $ "Save file successfully to "++(filePath)
        ) 
        (\e -> return $ "Couldn't save file "++(show e) )

load :: String -> IO (Either String GamePlayState)
load filePath = do
    catchIOError (
        do
            readData <- readFile filePath
            return $ case readMaybe readData :: Maybe [Int] of
                Nothing -> Left "Save file corrupted from unicode"
                Just unicode -> case readMaybe (fromUnicode unicode) :: Maybe GamePlayState of
                        Nothing -> Left "Save file corrupted"
                        Just gs -> Right gs
        )
        (\e -> return $ Left $ "Couldn't load file "++(show e)) 