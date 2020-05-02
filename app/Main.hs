module Main where

import GamePlay
import System.Random (getStdGen)


main :: IO ()
main = getStdGen >>= menu
    -- resetScreen
    -- drawLargeSprite (0, 0) (getSprite "Charizard")
    -- drawLargeSprite (0,90) (getSprite "Blastoise")
    -- _ <- getLine
    -- return ()


     
        