{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

checksum (x:xs) =
    case find (\y -> mod y x == 0) xs of
        Just y -> div y x
        Nothing -> checksum xs

main = interact $ (++"\n") . show . sum . map checksum . map (sort . map read . words) . lines
