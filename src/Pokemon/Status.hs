module Pokemon.Status where

data Status = Flying | Poison | Paralyzed | Burn  deriving (Eq, Show, Read)

setStatus :: [Status] -> Status -> ([Status], Bool)
setStatus currentSt newSt = 
    if newSt `elem` currentSt then (currentSt, False)
    else (newSt:currentSt, True)

statusAttachedDescription :: Status -> String
statusAttachedDescription Flying = " is Flying up to the sky"
statusAttachedDescription Poison = " is affected by Poison"
statusAttachedDescription Paralyzed = " is new Paralyzed and might not move"
statusAttachedDescription Burn = " is burnt by the fire"