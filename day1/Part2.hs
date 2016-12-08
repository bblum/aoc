{-# LANGUAGE TupleSections, FlexibleContexts #-}
import Data.Set (empty,insert,member)
import Control.Monad.State

visit (point@(x,y)) = modify $ \(path,result) ->
    (insert point path, if member point path then (abs x + abs y):result else result)

step (x,y,0) dist = (mapM visit $ map (x,)           [y+1..y+dist]) >> return (x, y+dist, 0)
step (x,y,1) dist = (mapM visit $ map (,y)           [x+1..x+dist]) >> return (x+dist, y, 1)
step (x,y,2) dist = (mapM visit $ map (x,) $ reverse [y-dist..y-1]) >> return (x, y-dist, 2)
step (x,y,3) dist = (mapM visit $ map (,y) $ reverse [x-dist..x-1]) >> return (x-dist, y, 3)

turn 'L' heading = (heading + 3) `mod` 4
turn 'R' heading = (heading + 1) `mod` 4

walk (x,y,heading) (dir:dist) = step (x, y, turn dir heading) $ read dist

main = interact $ (++"\n") . show . head . reverse . snd . flip execState (empty,[]) . foldM walk (0,0,0) . lines
