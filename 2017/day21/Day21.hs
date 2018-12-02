import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad
import Control.Arrow

rotations x = foldr (liftM2 ($)) [x] $ map (:[id]) [reverse, map reverse, transpose]

chunkify n = map (transpose . map (chunksOf n)) . chunksOf n

assemble n = concatMap (\xs -> map (flip concatMap xs . flip (!!)) [0..n])

solve i rules = sum $ map (length . filter (=='#')) $ iterate enhance [".#.","..#","###"] !! i
    where findrule = head . mapMaybe (flip lookup rules) . rotations
          enhance x = let n = 2 + length x `mod` 2 in assemble n $ map (map findrule) $ chunkify n x

main = interact $ show . (solve 5 &&& solve 18) . map ((\[x,_,y]->(x,y)) . map (splitOn "/") . words) . lines
