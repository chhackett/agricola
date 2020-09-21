module Utils.Selection
( getNextSelection,
  yesNoOptions,
  oneOrBothOptions,
  getNumberOptions,
  Option,
  Options,
  EitherOrBoth (..)) where

import qualified Data.Map as M

type Description = String

data EitherOrBoth = First | Second | Both

type Option a = (Description, a)
type Options a = [Option a]

type OptionMap a = M.Map Char (Option a)

getNextSelection :: Options a -> IO a
getNextSelection os = do
  let dos = displayOptions os
  getSelect dos

yesNoOptions :: Options Bool
yesNoOptions = [("Yes", True), ("No", False)]

oneOrBothOptions :: Description -> Description -> Options EitherOrBoth
oneOrBothOptions a b = [(a, First), (b, Second), ("Both", Both)]

getNumberOptions :: Int -> Options Int
getNumberOptions n = map (\i -> (show i, i)) [1 .. n]

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
showOptions m = 
  if M.null m
  then error "No options available"
  else foldl builder "" (M.assocs m)
  where builder result (c, o) = result ++ "\n\t(" ++ [c] ++ ") " ++ show (fst o)

displayOptions :: Options a ->  OptionMap a
displayOptions [] = M.empty
displayOptions os = fst $ foldl next (M.empty, 'a') os
  where next (result, c) o = let c' = if c == 'z' then 'A' else succ c in
         (M.insert c o result, c')
