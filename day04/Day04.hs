import Data.List

input = map show [130254..678275]

part1 x = any (uncurry (==)) $ zip x $ tail x
part2 x = any ((== 2) . length) $ group x

ispass match_fn x = match_fn x && (all (uncurry (<=)) $ zip x $ tail x)

main = do print $ length $ filter (ispass part1) input
          print $ length $ filter (ispass part2) input
