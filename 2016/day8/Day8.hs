import Data.List

rotate a l = drop (length l - a) l ++ take (length l - a) l

foo s ("rect", x, y)   = (map ((replicate x '#' ++) . drop x) $ take y s) ++ drop y s
foo s ("row", y, a)    = take y s ++ [rotate a (s !! y)] ++ drop (y+1) s
foo s ("column", x, a) = transpose $ foo (transpose s) ("row", x, a)

parse line = let [cmd, arg1, arg2] = words line in (cmd, read arg1, read arg2)

main = interact $ unlines . foldl foo (replicate 6 $ replicate 50 ' ') . map parse . lines
