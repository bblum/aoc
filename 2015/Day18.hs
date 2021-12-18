import Data.List
import Control.Arrow

nbrs z row0 = zip3 row (tail row) (drop 2 row)
    where row = [z] ++ row0 ++ [z]

life ((a,b,c),(d,e,f),(g,h,i)) = n == 3 || (e && n == 2)
    where n = length $ filter id [a,b,c,d,f,g,h,i]

stuck'' (_:rest) = True:rest
stuck' (row:rest) = (reverse $ stuck'' $ reverse $ stuck'' row):rest
stuck = reverse . stuck' . reverse . stuck'

update z f = f . map (map life) . transpose . map (nbrs (z,z,z)) . transpose . map (nbrs z)

solve f = length . filter id . concat . (!! 100) . iterate (update False f)

main = interact $ (++"\n") . show . (solve id &&& solve stuck) . map (map (=='#')) . lines
