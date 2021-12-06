import qualified Data.Set as S
import Data.List
import Data.List.Split

part2 input = map candidates $ nub $ concatMap last input
    where candidates a = (a, foldl1 S.intersection $ map (S.fromList . head) $ filter (elem a . last) input)

part1 input = length $ filter (not . flip S.member allergens) $ concatMap head input
    where allergens = foldl1 S.union $ map snd $ part2 input

main = do input <- map (splitOn ["contains"] . words) <$> lines <$> readFile "input.txt"
          print $ part1 input
          mapM print $ sortOn fst $ part2 input
