module Main where

import GamePlay
import System.IO

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    mainGamePlay
    -- getCurrentTime >>= print
    -- threadDelay (1000000 * 5)
    -- getCurrentTime >>= print

     
