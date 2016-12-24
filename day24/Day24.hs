{-# LANGUAGE FlexibleContexts #-}
import Data.Set (singleton, member, insert)
import Data.List hiding (insert)
import Data.Maybe
import Control.Monad.State
import Control.Arrow

graph input = ([ ([a,b], distance (xys !! a) (xys !! b)) | a <- ns, b <- ns, a < b], last ns)
    where (ns, xys) = unzip $ catMaybes $ takeWhile isJust $ map coord ['0'..]
          coord c = do y <- findIndex (any (== c)) input
                       x <- findIndex (== c) $ input !! y
                       return (read [c], (x,y))
          distance start goal = evalState (bfs 0 goal [start]) $ singleton start
          bfs n goal ps = if elem goal ps then return n
                          else bfs (n+1) goal =<< concat <$> mapM fnbrs ps
          fnbrs (x0,y0) = catMaybes <$> mapM try [(x0-1,y0),(x0+1,y0),(x0,y0-1),(x0,y0+1)]
          try pos@(x,y) = do visited <- get
                             if input !! y !! x == '#' || member pos visited then return Nothing
                             else modify (insert pos) >> return (Just pos)

solve endpath (g,n) = minimum $ map (cost . endpath . (0:)) $ permutations [1..n]
    where cost p = sum $ map (fromJust . flip lookup g) $ zipWith (\a b -> sort [a,b]) p $ tail p

main = interact $ show . (solve id &&& solve (++[0])) . graph . lines
