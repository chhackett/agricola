module Utils.ListUtils where
  
getFirstElem :: (Eq b) => (a -> b) -> b -> [a] -> Maybe a
getFirstElem f b [] = Nothing
getFirstElem f b (a:as) = if f a == b then Just a else getFirstElem f b as

replaceElem :: (Eq b) => (a -> b) -> a -> [a] -> [a]
replaceElem f x = map replaceIf
  where t = f x
        replaceIf x' = let t' = f x' in if t == t' then x else x'

combineThings :: (Eq t) => (t, Int) -> [(t, Int)] -> [(t, Int)]
combineThings (t, n) things = if hasType t things then map addThings things else (t,n):things
  where addThings (t', n') = if t == t' then (t', n + n') else (t', n')

hasType :: (Eq t) => t -> [(t, Int)] -> Bool
hasType t = foldl hasType' False
  where hasType' b (t',_) = b || (t' == t)

hasThings :: [(t, Int)] -> Bool
hasThings = foldl hasThing False
  where hasThing b a = b || snd a > 0

getAmount :: (Eq a) => a -> [(a, Int)] -> Int
getAmount rt = foldl get 0
  where get acc (rt', n) = if rt == rt' then n + acc else acc

-- Return all elements in the second list that are not in the first
difference :: Eq a => [a] -> [a] -> [a]
difference as = filter (`notElem` as)