module DrawScreen where

import System.IO
import System.Console.ANSI

import Pokemon.PokemonInfo (PokemonInfo (..))
import Pokemon.PokemonMove (MoveLogs (..), moveLogToStr, splitLogs)
import Util (AttackTurn (..))
import Pokemon.PokemonStat (currentHp, maxHp)
import Pokemon.BattleState (PokemonState)
import Pokemon.Status (statusToColor)
import Pokemon.PokemonInstance (getSprite)


drawLargeText :: (Int, Int) -> [String] -> IO Int
drawLargeText (row, col) sprite = do
    let zipper = (zip sprite [0..])
    mapM_ (\(s, r) -> drawAt (row+r, col) s) zipper
    return $ snd $ last zipper
    where
        drawAt :: (Int,Int) -> String -> IO ()
        drawAt (row, col) draw = do
            setCursorPosition row col
            putStrLn draw

drawLargeTextWithColor :: Color -> (Int, Int) -> [String] -> IO Int
drawLargeTextWithColor color pos sprite  = do
    setSGR [SetColor Foreground Vivid color]
    size <- drawLargeText pos sprite
    setSGR [Reset]
    return size

drawGameState :: PokemonState -> PokemonState -> [MoveLogs] -> AttackTurn -> IO ()
drawGameState player enemy logs turn = do
    resetScreen
    let (playerLogs, enemyLogs) = splitLogs logs turn
    putStrLn (take 150 $ cycle "=")
    size <- drawEnemyPokemon enemy enemyLogs
    size' <- drawPlayerPokemon size player playerLogs
    putStrLn (take 150 $ cycle "=")
    return ()


drawPlayerPokemon :: Int -> PokemonState -> [MoveLogs] -> IO Int
drawPlayerPokemon startAt = drawInfoPokemon (startAt,0) (startAt+25, 75)


drawEnemyPokemon :: PokemonState -> [MoveLogs] -> IO Int
drawEnemyPokemon = drawInfoPokemon (0,90) (10,40)


drawInfoPokemon :: (Int,Int) -> (Int,Int) -> PokemonState -> [MoveLogs] -> IO Int
drawInfoPokemon spritePos descPos (pkInfo, pkStatus) logs = do
    let desc = (name pkInfo)++" Hp: "++(show $ currentHp $ stats pkInfo)++"/"++(show $ maxHp $ stats pkInfo)++" "++(show pkStatus)
        logsString = map moveLogToStr logs
    drawLargeText descPos ( desc:" ":logsString )
    size <- drawLargeText spritePos ( (getSprite $ name pkInfo) )
    return size


resetScreen ::  IO ()
resetScreen = do
    clearFromCursorToScreenBeginning
    setCursorPosition 0 0
    setSGR [Reset]


