module Pokemon.Status where

data Status = Flying | Poison | Paralyzed | Burn  deriving (Eq, Show, Read)

setStatus :: [Status] -> Status -> ([Status], Bool)
setStatus currentSt newSt = 
    if newSt `elem` currentSt then (currentSt, False)
    else (newSt:currentSt, True)