import Data.List

nbr [x,y,z,w] [a,b,c,d] = (abs (x-a) + abs(y-b) + abs (z-c) + abs(w-d)) <= 3

rm input [] = input
rm input (s:stars) = rm (input \\ nbrs) (stars ++ nbrs) where nbrs = filter (nbr s) input

main = do input <- map (map read . words) <$> lines <$> readFile "input.txt"
          print $ length $ takeWhile (not . null) $ iterate (\(s:stars) -> rm stars [s]) input
