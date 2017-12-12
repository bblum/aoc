import qualified Data.Set as S
import Data.List
import Data.Maybe

part1 input = snd $ until fst bfs (False, (S.singleton $ fst $ head input))
    where bfs (_,g0) = let g = S.foldr nobe g0 g0 in (g0 == g, g)
          nobe n g = foldr S.insert g $ fromJust $ lookup n input

part2 input = S.foldr (filter . (. fst) . (/=)) input $ part1 input

parse (n:_:rest) = (read n :: Int, map (read . filter (/= ',')) rest)

main = do input <- map (parse . words) <$> lines <$> readFile "input.txt"
          print $ length $ part1 input
          print $ length $ takeWhile (not . null) $ iterate part2 input
