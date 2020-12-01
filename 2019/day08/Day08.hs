import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord

width = 25
height = 6
size = width * height

part1 input = count '1' minzeroes * count '2' minzeroes
    where minzeroes = minimumBy (comparing (count '0')) input
          count c = length . filter (== c)

part2 input = chunksOf width $ map (fromJust . find (/='2')) $ transpose input

main = do input <- chunksOf size <$> head <$> lines <$> readFile "input.txt"
          print $ part1 input
          mapM_ putStrLn $ part2 input
