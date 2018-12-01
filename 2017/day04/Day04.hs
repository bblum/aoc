{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

-- valid pass = length (nub pass) == length pass -- part 1
valid pass = length (nub $ map sort pass) == length pass

main = interact $ (++"\n") . show . length . filter valid . map words . lines
