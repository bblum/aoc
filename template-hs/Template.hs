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

dyxs8 = [(dy,dx) | dy <- [-1..1], dx <- [-1..1], (dy,dx) /= 0]
dyxs4 = [(-1,0),(1,0),(0,-1),(0,1)]
fnbrs8 (y,x) = map (\(dy,dx) -> (y+dy,x+dx)) dyxs8
fnbrs4 (y,x) = map (\(dy,dx) -> (y+dy,x+dx)) dyxs4

parse = words

main = do input <- map parse <$> lines <$> readFile "input.txt"
          print 42
