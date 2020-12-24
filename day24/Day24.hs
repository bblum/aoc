import qualified Data.Map as M

count = length . filter id

fnbrs6 (y,x) = [(y,x-2),(y,x+2),(y-1,x-1),(y-1,x+1),(y+1,x-1),(y+1,x+1)]

parse (ns:ew:rest) | elem ns "ns" && elem ew "ew" = [ns,ew]:parse rest
parse (x:rest) = [x]:parse rest
parse [] = []

coord yx dir = (M.! dir) $ M.fromList $ zip ["w","e","nw","ne","sw","se"] $ fnbrs6 yx

query m yx = maybe False id $ M.lookup yx m
toggle m yx = M.insert yx (not $ query m yx) m

run m = M.fromList [((y,x), cell (y,x)) | y <- range ys 1, x <- range xs 2, mod (x+y) 2 == 0]
    where (ys,xs) = unzip $ M.keys m
          range l step = [minimum l - step .. maximum l + step]
          cell yx = case count $ map (query m) $ yx:fnbrs6 yx of
              2 -> True
              x | elem x [0,3] -> query m yx
              _ -> False

main = do state <- foldl toggle M.empty <$> map (foldl coord (0,0) . parse) <$> lines <$> readFile "input.txt"
          print $ count $ M.elems state
          print $ count $ M.elems $ iterate run state !! 100
