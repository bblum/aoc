import Data.List
import Data.List.Split
import Control.Arrow

fnbrs input (bridge@(p@[_,n]:_)) = if null nbrs then [bridge] else map (:bridge) nbrs
    where nbrs = map (\[a,b] -> if a == n then [a,b] else [b,a]) $ filter fnbr input
          fnbr q = elem n q && not (elem q bridge || elem (reverse q) bridge)

bridges input = iterate (concatMap (fnbrs input)) zeroes !! length input
    where zeroes = map return $ filter ((== 0) . head) input

longest bridges = filter ((== maximum (map length bridges)) . length) bridges

solve f = maximum . map (sum . map sum) . f

main = interact $ (++"\n") . show . (solve id &&& solve longest) . bridges . map (map read . splitOn "/") . lines
