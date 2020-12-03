{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad.State
import Control.Arrow
import Debug.Trace

count f = length . filter f

foo (line, n) = line !! index
    where index = (n * 3) `mod` (length line)

foo2 (line, n) = line !! index
    where index = (n * 1) `mod` (length line)
foo3 (line, n) = line !! index
    where index = (n * 5) `mod` (length line)
foo4 (line, n) = line !! index
    where index = (n * 7) `mod` (length line)

solve foo input =
    let width = length (input !! 0)
        noobs = map foo $ zip input [0..]
    in count (== '#') noobs

fux (i:j:is) = i:(fux is)
fux (i:[]) = []
fux [] = []

solve2 input = product (map (\f -> solve f input) [foo, foo2, foo3, foo4]) * five
    where five = solve foo2 $ fux input

main = do input <- lines <$> readFile "input.txt"
          print $ solve2 input
