module Pokemon.PokemonStat where

import Util (mulfloor)

data PokemonStats = PokemonStats {
    maxHp::Int,
    currentHp::Int,
    atk::Int,
    def::Int,
    speed::Int
} deriving (Show, Read)

data StatsModifer = StatsModifer {
    modHp::Float,
    modAtk::Float,
    modDef::Float,
    modSpeed::Float
} deriving (Show, Read)


changeHpStat :: PokemonStats -> Int -> PokemonStats
changeHpStat (PokemonStats mh h a d s) newHp = PokemonStats mh (max 0 (min mh newHp)) a d s

modifyStats :: PokemonStats -> StatsModifer -> PokemonStats
modifyStats (PokemonStats mh h a d s) (StatsModifer modH modA modD modS) =
    let newAtk = mulfloor a (1.0+modA)
        newHp  = min mh (mulfloor h (1.0+modH))
        newDef = mulfloor d (1.0+modD)
        newSpd = mulfloor s (1.0+modS)
    in PokemonStats mh newHp newAtk newDef newSpd

modifierChangeDescription :: StatsModifer -> [String]
modifierChangeDescription (StatsModifer modH modA modD modS) =
    let descPair = zip [modH, modA, modD, modS] ["HP","Attack","Defense","Speed"]
    in [s | Just s <- map changeDescription descPair]

changeDescription :: (Float, String) -> Maybe String
changeDescription (modify, name) =
    if modify == 0 then Nothing
    else if modify < 0 then Just ("Decrease "++name++" "++ (show (round (modify * (-100))) ++ "%"))
    else Just ("Increase "++name++" "++ (show $ round (modify * 100)) ++ "%")