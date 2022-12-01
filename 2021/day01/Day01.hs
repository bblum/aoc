import Control.Arrow

part1 input = length $ filter (uncurry (<)) $ zip input $ tail input

part2 input = part1 $ zipWith (+) input $ zipWith (+) (tail input) (tail $ tail input)

main = print =<< (part1 &&& part2) <$> map read <$> lines <$> readFile "input.txt"
