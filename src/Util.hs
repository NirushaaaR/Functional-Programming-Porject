module Util where

import System.Random
-- import Control.Monad
import Control.Monad.Writer

multiplyIntFloat :: Int -> Float -> Float
multiplyIntFloat = (*).fromIntegral

mulfloor :: Int -> Float -> Int
mulfloor i f = floor $ multiplyIntFloat i f

mulceling :: Int -> Float -> Int
mulceling i f = ceiling $ multiplyIntFloat i f


randomTrigger :: StdGen -> (Int, Int) -> (Bool, StdGen)
randomTrigger stdGen (chance, inChance) =
    let (randNum, newGen) = random stdGen :: (Int, StdGen)
    in  if chance > inChance then error "InChance Need to be bigger than chance"
        else ((randNum `mod` (inChance+1)) <= chance, newGen)


randomNumInRange :: StdGen -> (Int, Int) -> (Int, StdGen)
randomNumInRange stdGen (minNum, maxNum) =
    let (randNum, newGen) = random stdGen :: (Int, StdGen)
    in ( ( (randNum `mod` ((maxNum+1) - minNum)) + minNum) , newGen )



    
    