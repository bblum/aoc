{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

-- part1 x = Just (x+1)
part2 x = if x >= 3 then Just (x-1) else Just (x+1)

step n i p = if i >= length p then n else step (n+1) (i + (p M.! i)) (M.update part2 i p)

main = interact $ (++"\n") . show . step 0 0 . M.fromList . zip [0..] . map read . lines
