import Data.List
import Control.Arrow

nbr ([x1,y1],_) ([x2,y2],_) = elem x1 [x2-1..x2+1] && elem y1 [y2-1..y2+1]
fnbr ps p = any (nbr p) (ps \\ [p])
isword (ps,_) = all (fnbr ps) ps

update (xy,v) = (zipWith (+) xy v, v)

parse [_,x,y,_,vx,vy] = ([read x, read y], [read vx, read vy])

output xys = map row [minimum $ map last xys..maximum $ map last xys]
    where row y = map (col y) [minimum $ map head xys..maximum $ map head xys]
          col y x = if elem [x,y] xys then '#' else '.'

main = do input <- map (parse . words) <$> lines <$> readFile "input.txt"
          let (word, n) = until isword (map update *** (+1)) (input, 0)
          mapM print $ output $ map fst word
          print n
