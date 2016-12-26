import Data.List
import Data.Maybe
import Control.Arrow

graph [x,_,"gain",n,_,_,_,_,_,_,y] = ((x,y), read n)
graph [x,_,"lose",n,_,_,_,_,_,_,y] = ((x,y), -(read n))

solve f g = maximum $ map cost $ permutations noobs
    where noobs = nub $ map (fst . fst) g
          cost seating = cost' seating + cost' (reverse seating)
          cost' seating = sum $ map (fromJust . flip lookup g) $ zip seating $ tail $ f seating

main = interact $ (++"\n") . show . (solve cycle &&& solve id) . map graph . map words . lines
