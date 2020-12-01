import Data.List

part1 x = any (uncurry (==)) $ zip x $ tail x
part2 x = any ((== 2) . length) $ group x

ascending x = all (uncurry (<=)) $ zip x $ tail x

candidates = filter ascending $ map show [130254..678275]

main = do print $ length $ filter part1 candidates
          print $ length $ filter part2 candidates
