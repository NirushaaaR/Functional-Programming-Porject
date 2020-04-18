module Pokemon.PokemonType where


data Type = Fire | Water | Grass deriving (Eq, Show, Read)

-- weakness :: Type -> [Type]
-- weakness Fire = [Water]
-- weakness Water = [Grass]
-- weakness Grass = [Fire]


-- weakAgainst :: Type -> Type -> Bool
-- t1 `weakAgainst` t2 = t2 `elem` (weakness t1)


