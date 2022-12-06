parse [[a],[x]] = (fromEnum a - 65, fromEnum x - 88) 

part1 (a,x) = x+1 + (3 * mod (4+x-a) 3)
part2 (a,x) = 3*x + (1 + mod (2+a+x) 3)

main = do input <- map (parse . words) <$> lines <$> readFile "input.txt"
          print $ sum $ map part1 input
          print $ sum $ map part2 input
