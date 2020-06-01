module Utils.Selection
( getNextSelection,
  Option,
  Options ) where

import qualified Data.Map as M

type Description = String

type Option a = (Description, a)
type Options a = [Option a]

type OptionMap a = M.Map Char (Option a)

getNextSelection :: Options a -> IO a
getNextSelection os = do
  let dos = displayOptions os
  getSelect dos

getSelect :: OptionMap a -> IO a
getSelect mos = do
        putStr $ "Select from the following options:\n" ++ showOptions mos ++ "\n\nEnter choice: "
        selection <- getLine
        if length selection /= 1
        then do putStrLn "Invalid selection. Please select an option and press <return>"
                getSelect mos
        else case M.lookup (head selection) mos of
                        Nothing -> do putStrLn "Invalid selection."
                                      getSelect mos
                        Just option -> return (snd option)

showOptions :: OptionMap a -> String
showOptions m = foldl builder "" (M.assocs m)
  where builder result (c, o) = result ++ "\n\t(" ++ [c] ++ ") " ++ show (fst o)

displayOptions :: Options a ->  OptionMap a
displayOptions [] = M.empty
displayOptions os = fst $ foldl next (M.empty, 'a') os
  where next (result, c) o = let c' = if c == 'z' then 'A' else succ c in
         (M.insert c o result, c')
