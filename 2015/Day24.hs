import Data.List

input = [1,2,3,7,11,13,17,19,23,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113]

fill limit xs | limit < 0 || sum xs < limit = []
              | limit == 0 = [[]]
              | sum xs == limit = [xs]
fill limit (x:xs) = map (x:) (fill (limit-x) xs) ++ fill limit xs

solve n = head $ sort $ map product $ filter ((==(length $ head $ sortOn length ways)) . length) ways
    where ways = fill (div (sum input) n) $ reverse input

main = print (solve 3, solve 4)
