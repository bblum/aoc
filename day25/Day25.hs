import Data.List

nbr x y = (sum $ map abs $ zipWith (-) x y) < 4

rm input [] = input
rm input (s:stars) = rm (input \\ nbrs) (stars ++ nbrs) where nbrs = filter (nbr s) input

main = do input <- map (map read . words) <$> lines <$> readFile "input.txt"
          print $ length $ takeWhile (not . null) $ iterate (\(s:stars) -> rm stars [s]) input
