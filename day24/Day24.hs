import qualified Data.Set as S
import Data.Maybe

fnbrs6 (y,x) = [(y,x-2),(y,x+2),(y-1,x-1),(y-1,x+1),(y+1,x-1),(y+1,x+1)]

parse (ns:ew:rest) | elem ns "ns" = [ns,ew]:parse rest
parse (x:rest) = [x]:parse rest
parse [] = []

coord yx dir = fromJust $ lookup dir $ zip ["w","e","nw","ne","sw","se"] $ fnbrs6 yx

toggle s yx = if S.member yx s then S.delete yx s else S.insert yx s

run s = S.fromList [(y,x) | y <- range ys 1, x <- range xs 2, mod (x+y) 2 == 0, cell (y,x)]
    where (ys,xs) = unzip $ S.elems s
          range l step = [minimum l - step .. maximum l + step]
          cell yx = case length $ filter (flip S.member s) $ yx:fnbrs6 yx of
              2 -> True
              x | elem x [0,3] -> S.member yx s
              _ -> False

main = do state <- foldl toggle S.empty <$> map (foldl coord (0,0) . parse) <$> lines <$> readFile "input.txt"
          print $ S.size state
          print $ S.size $ iterate run state !! 100
