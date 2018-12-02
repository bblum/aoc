import Data.Map ((!),update,fromList)

part1 x = Just $ x + 1
part2 x = Just $ if x >= 3 then x - 1 else x + 1

exec n i p = if i >= length p then n else exec (n + 1) (i + p ! i) (update part2 i p)

main = do input <- readFile "input.txt"
          print $ exec 0 0 $ fromList $ zip [0..] $ map read $ lines input


