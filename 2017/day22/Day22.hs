{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Arrow

-- facing: 0 up, 1 left, 2 down, 3 right
-- state: 0 weak, 1 clean, 2 flag, 3 infect

burst p ((xy,i,nobes),n) = ((step xy, j, M.insert xy s nobes), n + fromEnum (s == 3))
    where (s,j) = (turn p &&& turn i) $ fromMaybe 1 $ M.lookup xy nobes
          turn i s = mod (i + s) 4
          step = (+ axis * dir) *** (+ (1 - axis) * dir)
          axis = mod j 2
          dir = 2 * div j 2 - 1

solve p n input = snd $ iterate (burst p) (((x,x), 0, M.fromList nobes), 0) !! n
    where x = div (length input) 2
          parse c = if c == '#' then 3 else 1
          nobes = concat $ zipWith (\y -> zip (map (,y) [0..]) . map parse) [0..] input

main = interact $ show . (solve 2 10000 &&& solve 3 10000000) . lines
