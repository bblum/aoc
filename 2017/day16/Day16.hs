import Data.List
import Data.List.Split
import Data.Maybe

swap l [i,j] = take i l ++ [l !! j] ++ drop (i+1) (take j l) ++ [l !! i] ++ drop (j+1) l

step l ('s':rest) = drop i l ++ take i l where i = length l - read rest
step l ('x':rest) = swap l $ sort $ map read $ splitOn "/" rest
step l ('p':rest) = swap l $ sort $ map (fromJust . flip elemIndex l . head) $ splitOn "/" rest

dance = flip iterate ['a'..'p'] . flip (foldl step)

period (x:rest) = 1 + (length $ takeWhile (/= x) rest)

main = do input <- splitOn "," <$> readFile "input.txt"
          print $ dance input !! 1
          print $ dance input !! mod 1000000000 (period $ dance input)
