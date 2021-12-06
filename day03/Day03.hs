import Data.List
import Control.Arrow

parse z = foldl f 0 where f a b = 2*a + if b == z then 0 else 1

median input = (transpose $ map sort $ transpose input) !! div (length input) 2

part1 input = parse '0' m * parse '1' m where m = median input

p2 _ _ [w] = w
p2 f i input = p2 f (i+1) $ filter (f (median input !! i) . (!! i)) input

part2 input = parse '0' (p2 (==) 0 input) * parse '0' (p2 (/=) 0 input)

main = print =<< (part1 &&& part2) <$> lines <$> readFile "input.txt"
