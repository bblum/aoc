{-# LANGUAGE TupleSections #-}
import qualified Data.Set as S
import Data.List
import Data.Ord

fnbrs4 (y,x) = [(y-1,x), (y+1,x), (y,x-1), (y,x+1)]

part1 input = sum $ map risklow coords
    where coords = concat $ zipWith (\y row -> map (y,) row) [0..maxy-1] $ repeat [0..maxx-1]
          maxy = length input
          maxx = length (head input)
          val :: Int -> Int -> Int
          val y x = read [input !! y !! x]
          risklow (y,x) | islow (y,x) = val y x + 1
          risklow (y,x) = 0
          inbounds (y,x) | elem y [-1,maxy] || elem x [-1,maxx] = False
          inbounds _ = True
          islow (y,x) = all (\(y2,x2) -> val y x < val y2 x2) $ filter inbounds $ fnbrs4 (y,x)

part2 input = product $ map S.size $ take 3 $ reverse $ sortBy (comparing S.size) all_basins
    where coords = concat $ zipWith (\y row -> map (y,) row) [0..maxy-1] $ repeat [0..maxx-1]
          maxy = length input
          maxx = length (head input)
          val :: Int -> Int -> Int
          val y x = read [input !! y !! x]
          inbounds (y,x) | elem y [-1,maxy] || elem x [-1,maxx] = False
          inbounds _ = True
          islow (y,x) = all (\(y2,x2) -> val y x < val y2 x2) $ filter inbounds $ fnbrs4 (y,x)
          basin existing_basins (y,x) | islow (y,x) = S.insert result existing_basins
              where result = flood S.empty [(y,x)]
                    flood output [] = output
                    flood output (yx@(y,x):rest) = flood (S.insert yx output) (rest ++ nbrs)
                        where nbrs = filter ok $ filter inbounds $ fnbrs4 (y,x)
                              ok (y2,x2) | S.member (y2,x2) output = False
                              ok (y2,x2) | val y2 x2 == 9 = False
                              ok (y2,x2) = val y x < val y2 x2
          basin existing_basins _ = existing_basins
          all_basins = S.toList $ foldl basin S.empty coords

main = do input <- lines <$> readFile "input.txt"
          print $ part1 input
          print $ part2 input
