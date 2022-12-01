import Data.List
import Data.List.Split
import Data.Ord

part1 [_,outs] = length $ filter (flip elem [2,3,4,7] . length) outs

part2 [ins,outs] = foldl (\n i -> 10*n+i) 0 $ map solve outs
    where [n1,_,n4,_,_,_,n0,n6,n9,_] = sortBy (comparing length) ins
          bd = n4 \\ n1
          cde = map head $ filter ((== 2) . length) $ group $ sort $ n0 ++ n6 ++ n9
          [b] = bd \\ cde
          [d] = bd \\ [b]
          [e] = cde \\ (d:n1)
          solve word = case length word of
              2 -> 1
              3 -> 7
              4 -> 4
              5 -> if elem e word then 2 else if elem b word then 5 else 3
              6 -> if elem d word then if elem e word then 6 else 9 else 0
              7 -> 8

main = do input <- map (splitOn ["|"] . words) <$> lines <$> readFile "input.txt"
          print $ sum $ map part1 input
          print $ sum $ map part2 input
