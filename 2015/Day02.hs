{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

area [l,w,h] = let shit = [l*w,w*h,h*l] in 2*(sum shit) + minimum shit

ribbon [l,w,h] = let shit = [l+w,w+h,h+l] in 2*(minimum shit) + l*w*h

main = interact $ (++"\n") . show . (sum . map area &&& sum . map ribbon) . map (map read . words) . lines
