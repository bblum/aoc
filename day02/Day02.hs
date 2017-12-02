{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

main = interact $ (++"\n") . show . sum . map ((\l -> head [div y x | x <- l, y <- l, x < y, mod y x == 0]) . map read . words) . lines
