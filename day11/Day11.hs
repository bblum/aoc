import Data.List
import Data.Maybe
import Data.Ord

power (x,y) = (mod (div ((((x+10)*y)+5177)*(x+10)) 100) 10) - 5

pairs n (x,y) = [ (z,w) | z <- [x..x+n-1], w <- [y..y+n-1] ]

grid n (x,y) = (((x,y),n), sum $ map power $ pairs n (x,y))

bestgrid n = maximumBy (comparing snd) $ map (grid n) $ pairs (301-n) (1,1)

bestsize ((_,a):(_,b):_) = a > b

main = do print $ fst $ fst $ bestgrid 3
          print $ fst $ head $ fromJust $ find bestsize $ tails $  map bestgrid [1..]
