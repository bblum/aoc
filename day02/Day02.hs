import Data.List
import Data.Maybe

count = (length .) . filter
anyn n str = any (\l -> count (== l) str == n) ['a'..'z']

common = (catMaybes .) . zipWith (\x y -> if x == y then Just x else Nothing)

main = do input <- lines <$> readFile "input.txt"
          print (count (anyn 2) input * count (anyn 3) input)
          print [ z | x <- input, y <- input, let z = common x y, length (x \\ z) == 1]
