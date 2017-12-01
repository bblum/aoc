import Data.List

row0 = map (== '.') "^^^^......^...^..^....^^^.^^^.^.^^^^^^..^...^^...^^^.^^....^..^^^.^.^^...^.^...^^.^^^.^^^^.^^.^..^.^"

rule (left:_:right:_) = left == right

next row = map rule $ drop 3 $ reverse $ tails $ [True] ++ row ++ [True]

solve n = length $ filter id $ concat $ take n $ iterate next row0

main = print (solve 40, solve 400000)
