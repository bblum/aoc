import Data.List
import Data.Maybe
import Control.Monad

input = 289326

-- generates a sequence like either of the following depending on the 'n' parameter
-- 0 1 1 0 -1 -1 -1  0  1  2 2 2 2 1 0 -1 -2
-- 0 0 1 1  1  0 -1 -1 -1 -1 0 1 2 2 2  2  2
walk n from to | to > 0 = replicate n from ++ [from,from+1..to-1] ++ walk (n+1) to (0-to)
walk n from to | to < 0 = replicate n from ++ [from,from-1..to+1] ++ walk (n+1) to (1-to)

spiral = zip (walk 0 0 1) (walk 1 0 1)

part2 sums = sums ++ [sum $ map (\n -> if n < length sums then sums !! n else 0) nbrs]
    where (x,y) = spiral !! length sums
          nbrs = map (fromJust . flip elemIndex spiral) $ liftM2 (,) [x-1..x+1] [y-1..y+1]

main = print (let (x,y) = spiral !! (input-1) in abs x + abs y, -- part 1
              last $ until ((>input) . last) part2 [1])         -- part 2
