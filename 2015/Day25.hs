{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

-- Enter the code at row 2981, column 3075.

triangle x = div (x*(x+1)) 2

grid row col = triangle (row + col - 1) - (row - 1)

input = grid 2981 3075

firstcode = 20151125
nextcode x = mod (x*252533) 33554393

-- period = (1+) $ length $ takeWhile (/= firstcode) $ tail $ iterate nextcode firstcode
period = 16777196

main = print $ iterate nextcode firstcode !! ((mod input period) - 1)
