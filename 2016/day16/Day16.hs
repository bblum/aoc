import Data.List
import Data.List.Extra
import Data.Char

dragon x = x ++ [False] ++ map not (reverse x)

stuff n = take n $ until ((>= n) . length) dragon $ map (== '1') "10001110011110000"

checksum = map (foldr1 (==)) . chunksOf 2

main = print $ map (intToDigit . fromEnum) $ until (odd . length) checksum $ stuff 35651584 -- 272
