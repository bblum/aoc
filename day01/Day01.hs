import Data.List
import Data.List.Split

main = do totals <- map (sum . map read) <$> splitOn [""] <$> lines <$> readFile "input.txt"
          print $ maximum totals
          print $ sum $ take 3 $ reverse $ sort totals
