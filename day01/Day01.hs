import Data.List
import Data.List.Split

main = do input <- map (sum . map read) <$> splitOn [""] <$> lines <$> readFile "input.txt"
          print $ maximum input
          print $ sum $ take 3 $ reverse $ sort input
