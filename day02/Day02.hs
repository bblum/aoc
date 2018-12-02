{-# LANGUAGE MonadComprehensions #-}
import Data.List
import Data.Maybe

count = (length .) . filter
anyn n str = any (\l -> count (== l) str == n) ['a'..'z']

main = do input <- lines <$> readFile "input.txt"
          print (count (anyn 2) input * count (anyn 3) input)
          print [ z | x <- input, y <- input, let z = catMaybes $ zipWith (\x y -> [x|x==y]) x y, length (x \\ z) == 1]
