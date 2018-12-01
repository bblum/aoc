{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

solve = undefined

main = do input <- lines <$> readFile "input.txt"
          mapM print $ transpose input
