{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Either
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad.State
import Control.Arrow
import Debug.Trace

fnbrs8 (y,x) = [(y2,x2) | y2 <- [y-1..y+1], x2 <- [x-1..x+1], (y2,x2) /= (y,x)]
fnbrs4 (y,x) = [(y-1,x), (y+1,x), (y,x-1), (y,x+1)]

coordify = zipWith (\y -> zipWith ((,) . (y,)) [0..]) [0..]

parse = words

main = do input <- map parse <$> lines <$> readFile "input.txt"
          print 42
