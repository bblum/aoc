import Data.List
import Data.Maybe

invalid input (i,n) = and [ (input !! a) + (input !! b) /= n | a <- noobs, b <- noobs, a /= b ]
    where noobs = [i-25..i-1]

solve input (i,n) = minimum range + maximum range
    where range = head [ range | start <- [0..i], end <- [start+1..i], let range = drop start $ take end input, sum range == n]

main = do input <- map read <$> lines <$> readFile "input.txt"
          let solution = fromJust $ find (invalid input) $ drop 25 $ zip [0..] input
          print $ snd solution
          print $ solve input solution
