import qualified Data.Map as M
import Data.List
import Control.Arrow

template = "PHOSBSKBBBFSPPPCCCHN"

add = foldl $ M.unionWith (+)

start [[a,b],_] = ((a,b), M.empty)

solve m = maximum totals - minimum totals
    where totals = map snd $ M.toList $ add tm $ map (m M.!) $ zip template $ tail template
          tm = M.fromList $ map (head &&& length) $ group $ sort template

part n rules = solve $ iterate generate (M.fromList $ map start rules) !! n
    where generate m = M.fromList $ map step rules
              where step [[a,b],[c]] = ((a,b), add (M.singleton c 1) [m M.! (a,c), m M.! (c,b)])

main = interact $ (++"\n") . show . (part 10 &&& part 40) . map words . lines
