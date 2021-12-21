import Data.List
import Data.Char

search p2 edges smalls (path@"end":_) = [path]
search p2 edges smalls (x:path) = concat $ map (recurse p2) nexts ++ map (recurse False) (if p2 then p2s \\ ["start"] else [])
    where (p2s,nexts) = partition (flip elem smalls) $ filter (edge x) $ nub $ concat edges
          edge x y = elem [x,y] edges || elem [y,x] edges
          recurse p2 next = search p2 edges (if map toLower next == next then next:smalls else smalls) $ next:x:path

main = do edges <- map words <$> lines <$> readFile "input.txt"
          let solve p2 = length $ search p2 edges ["start"] ["start"]
          print $ solve False
          print $ solve True
