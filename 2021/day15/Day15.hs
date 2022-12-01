import qualified Data.Heap as H
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char

fnbrs (y,x) = [(y-1,x), (y+1,x), (y,x-1), (y,x+1)]

part1 risks = search M.empty $ H.singleton (0,((0,0),0))
    where maxx = length (head risks) - 1
          maxy = length risks - 1
          astar ((y,x),cost) = cost + ((maxx-x) + (maxy-y))
          search m h = search' m $ fromJust $ H.uncons h
          search' m ((_,(yx,cost)),_) | yx == (maxy,maxx) = cost
          search' m ((_,(yx,cost)),h) | maybe False (<= cost) $ M.lookup yx m = search m h
          search' m ((_,(yx,cost)),h) = search (M.insert yx cost m) $ H.union h $ H.fromList $ mapMaybe checknbr $ fnbrs yx
              where checknbr (y2,x2) | y2 < 0 || y2 > maxy || x2 < 0 || x2 > maxx = Nothing
                    checknbr (y2,x2) = Just (astar nbr, nbr)
                        where cost2 = cost + (risks !! y2 !! x2)
                              nbr = ((y2,x2),cost2)

part2ify risks = map concat $ concat $ map transpose $ map (map plusify) $ zipWith (map . (+)) [0..4] $ repeat [0..4]
    where plusify i = map (map $ \r -> 1 + mod (r + i - 1) 9) risks

main = do input <- map (map digitToInt) <$> lines <$> readFile "input.txt"
          print $ part1 input
          print $ part1 $ part2ify input
