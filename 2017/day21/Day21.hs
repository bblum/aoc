import Data.List
import Data.List.Split
import Control.Monad
import Control.Arrow
import Data.Maybe

rotations x = foldr (liftM2 ($)) [x] $ map (:[id]) [reverse, map reverse, transpose]

zoom n = map (transpose . map (chunksOf n)) . chunksOf n

assemble n = concatMap (\xs -> map (flip concatMap xs . flip (!!)) [0..n])

solve i rules = sum $ map (length . filter (=='#')) $ iterate step [".#.","..#","###"] !! i
    where findrule = head . catMaybes . map (flip lookup rules) . rotations
          step x = assemble n $ map (map findrule) $ zoom n x
              where n = 2 + mod (length x) 2

main = interact $ show . (solve 5 &&& solve 18)
                  . map ((\[x,_,y]->(x,y)) . map (splitOn "/") . words) . lines
