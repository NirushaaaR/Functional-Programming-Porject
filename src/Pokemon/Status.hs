module Pokemon.Status where

import System.Console.ANSI (Color(..))

data Status = Flying | Poison | Paralyzed | Burn  deriving (Eq, Show, Read)

setStatus :: [Status] -> Status -> ([Status], Bool)
setStatus currentSt newSt = 
    if newSt `elem` currentSt then (currentSt, False)
    else (newSt:currentSt, True)

statusAttachedDescription :: Status -> String
statusAttachedDescription Flying = " is Flying up to the sky"
statusAttachedDescription Poison = " is affected by Poison"
statusAttachedDescription Paralyzed = " is now Paralyzed and might not move"
statusAttachedDescription Burn = " is burnt by the fire"


statusToColor :: Status -> Color
statusToColor Flying = Cyan
statusToColor Poison = Magenta
statusToColor Paralyzed = Yellow
statusToColor Burn = Red