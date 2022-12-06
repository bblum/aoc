import qualified Data.Set as S
import Data.List.Split

priority c = if x > 97 then x - 96 else x - 64 + 26 where x = fromEnum c

halve sack = [take n sack, drop n sack] where n = length sack `div` 2

solve = priority . head . S.toList . foldl1 S.intersection . map S.fromList

main = do input <- lines <$> readFile "input.txt"
          print $ sum $ map solve $ map halve input
          print $ sum $ map solve $ chunksOf 3 input
