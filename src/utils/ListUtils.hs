module Utils.ListUtils where
  
getFirstElem :: (Eq b) => (a -> b) -> b -> [a] -> Maybe a
getFirstElem f b [] = Nothing
getFirstElem f b (a:as) = if f a == b then Just a else getFirstElem f b as

replaceElem :: (Eq b) => (a -> b) -> a -> [a] -> [a]
replaceElem f x = map replaceIf
  where t = f x
        replaceIf x' = let t' = f x' in if t == t' then x else x'
