module Main where

import GamePlay
import System.IO

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    mainGamePlay
    putStrLn "Game End..."
    _ <- getLine
    return ()
    -- getCurrentTime >>= print
    -- threadDelay (1000000 * 5)
    -- getCurrentTime >>= print

     
