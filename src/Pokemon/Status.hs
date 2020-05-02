module Pokemon.Status where

import System.Console.ANSI (Color(..))

data Status = Flying | Poison | Paralyzed | Burn | Confuse  deriving (Eq, Show, Read)

setStatus :: [Status] -> Status -> ([Status], Bool)
setStatus currentSt newSt = 
    if newSt `elem` currentSt then (currentSt, False)
    else (newSt:currentSt, True)

statusAttachedDescription :: Status -> String
statusAttachedDescription Flying = " is Flying up to the sky"
statusAttachedDescription Poison = " is affected by Poison"
statusAttachedDescription Paralyzed = " is now Paralyzed and might not move"
statusAttachedDescription Burn = " is burnt by the fire"
statusAttachedDescription Confuse = " is confused"


filterOutStatus :: Status -> [Status]  -> [Status]
filterOutStatus filterOut = filter (\s -> s /= filterOut) 