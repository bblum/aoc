import Data.List
import Data.Char

react (x:y:p) = if x /= y && toUpper x == toUpper y then react p else x:(react (y:p))
react p = p

reduce p = let q = react p in if q == p then q else reduce q

remove input noob = filter (\c -> c /= noob && c /= toUpper noob) input

main = do input <- head <$> lines <$> readFile "input.txt"
          print $ length $ reduce input
          print $ minimum $ map (length . reduce . remove input) "abcdefghijklmonpqrstuvwxyz"
