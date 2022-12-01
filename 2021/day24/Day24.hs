import Data.List
import Data.List.Split
import Data.Maybe

variables line = if length (nub line) == 1 then Nothing else Just $ map (read . last) line

solve _ [] [] = ([], [])
solve f stack ([1,_,q]:rest) = (mes, me:ans)
    where (me:mes,ans) = solve f (q:stack) rest
solve f (q:stack) ([26,p,_]:rest) = (you:mes, me:ans)
    where (mes,ans) = solve f stack rest
          (you,me) = f [ (i,i+q+p) | i <- [1..9], elem (i+q+p) [1..9] ]

main = do input <- map words <$> lines <$> readFile "input.txt"
          let tape = transpose $ mapMaybe variables $ transpose $ splitOn [["inp","w"]] input
          print $ concatMap show $ snd $ solve last [] tape
          print $ concatMap show $ snd $ solve head [] tape
