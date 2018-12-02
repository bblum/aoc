{-# LANGUAGE MonadComprehensions #-}
import Data.List
import Data.Maybe
import Control.Arrow

count = (length .) . filter
anyn n str = any (\l -> count (== l) str == n) ['a'..'z']

part1 input = count (anyn 2) input * count (anyn 3) input
part2 input = [ z | x <- input, y <- input, let z = catMaybes $ zipWith (\x y -> [x|x==y]) x y, length (x \\ z) == 1]

main = interact $ (++"\n") . show . (part1 &&& part2) . lines
