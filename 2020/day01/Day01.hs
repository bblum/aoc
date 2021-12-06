main = do input <- map read <$> lines <$> readFile "input.txt"
          print [ x * y | x <- input, y <- input, x + y == 2020, x < y ]
          print [ x * y * z | x <- input, y <- input, z <- input, x + y + z == 2020, x < y, y < z ]
