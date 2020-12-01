import Data.List

find2 target (i:is) =
    case find (== target-i) is of
        Just ans -> Just $ ans * i
        Nothing -> find2 target is
find2 target [] = Nothing

find3 (i:is) =
    case find2 (2020-i) is of
        Just ans -> ans * i
        Nothing -> find3 is

main = do input <- map read <$> lines <$> readFile "input.txt"
          print $ find2 2020 input
          print $ find3 input
