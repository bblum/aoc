import Data.List

input = "^^^^......^...^..^....^^^.^^^.^.^^^^^^..^...^^...^^^.^^....^..^^^.^.^^...^.^...^^.^^^.^^^^.^^.^..^.^"

next row = [ left == right | left:_:right:_ <- tails $ [True]++row++[True] ]

solve n = length $ filter id $ concat $ take n $ iterate next $ map (== '.') input

main = print (solve 40, solve 400000)
