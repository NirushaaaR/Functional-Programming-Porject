module Main where

import Lib

import Data.Time
import Control.Concurrent

main :: IO ()
main = do
    getCurrentTime >>= print
    threadDelay (1000000 * 5)
    getCurrentTime >>= print

     
