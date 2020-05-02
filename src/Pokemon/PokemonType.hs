module Pokemon.PokemonType where


data Type = Normal | Fire | Water | Grass | Electric | Dark deriving (Eq, Show, Read)

-- weakAgainst :: Type -> Type -> Bool
-- t1 `weakAgainst` t2 = t2 `elem` (weakness t1)


