import Control.Arrow

p1 (d,x) ["forward",n] = (d, x + read n)
p1 (d,x) ["up",n] = (d - read n, x)
p1 (d,x) ["down",n] = (d + read n, x)

part1 = uncurry (*) . foldl p1 (0,0)

p2 (a,(d,x)) ["forward",n] = (a, (d + (a * read n), x + read n))
p2 (a,dx) ["up",n] = (a - read n, dx)
p2 (a,dx) ["down",n] = (a + read n, dx)

part2 = uncurry (*) . snd . foldl p2 (0,(0,0))

main = print =<< (part1 &&& part2) <$> map words <$> lines <$> readFile "input.txt"
