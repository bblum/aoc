import Data.List
import Control.Arrow

fibs = 1:2:zipWith (+) fibs (tail fibs)
main = interact $ show . ((fibs !! 26 +) &&& (fibs !! 33 +)) . product . map (read . (!! 1) . words) . take 2 . drop 16 . lines
