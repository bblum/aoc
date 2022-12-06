import qualified Data.Set as S
import Data.List
import Data.List.Split

priority c = if x > 97 then x - 96 else x - 64 + 26 where x = fromEnum c

-- TODO write with splitAt
split sack = (take n sack, drop n sack) where n = length sack `div` 2

part1 (l,r) = priority x where [x] = S.toList $ S.intersection (S.fromList l) (S.fromList r)

part2 sacks = priority x where [x] = S.toList $ foldl1 S.intersection $ map S.fromList sacks

main = do input <- lines <$> readFile "input.txt"
          print $ sum $ map (part1 . split) input
          print $ sum $ map part2 $ chunksOf 3 input
