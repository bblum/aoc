shape [_,"X"] = 1
shape [_,"Y"] = 2
shape [_,"Z"] = 3

outcome ["C","X"] = 6
outcome ["A","Y"] = 6
outcome ["B","Z"] = 6
outcome ["A","X"] = 3
outcome ["B","Y"] = 3
outcome ["C","Z"] = 3
outcome _ = 0

outcome2 [_,"X"] = 0
outcome2 [_,"Y"] = 3
outcome2 [_,"Z"] = 6

shape2 ["A","X"] = 3
shape2 ["A","Y"] = 1
shape2 ["A","Z"] = 2
shape2 ["B","X"] = 1
shape2 ["B","Y"] = 2
shape2 ["B","Z"] = 3
shape2 ["C","X"] = 2
shape2 ["C","Y"] = 3
shape2 ["C","Z"] = 1

main = do input <- map words <$> lines <$> readFile "input.txt"
          print $ sum $ map shape input ++ map outcome input
          print $ sum $ map shape2 input ++ map outcome2 input
