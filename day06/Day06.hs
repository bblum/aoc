import qualified Data.Set as S
import Data.List.Split

solve f = sum . map (S.size . foldl1 f . map S.fromList)

main = do input <- splitOn [""] <$> lines <$> readFile "input.txt"
          print $ solve S.union input
          print $ solve S.intersection input
