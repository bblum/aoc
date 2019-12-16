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

base = [0,1,0,-1]

pattern n = tail $ cycle $ concat $ map (replicate n) base

phase input = map digit [1..length input]
    where digit n = abs (sum $ zipWith (*) input $ pattern n) `mod` 10

parse str = map (\c -> read [c]) str

-- offset = 5976963
-- 
-- digit phase index =
--     let subdigits = lkjadlfkjadsflkjsadflkjsdflkjdsf

main = do input <- head <$> lines <$> readFile "input.txt"
          print $ concat $ map show $ take 8 $ iterate phase (parse input) !! 100
          -- let realsignal = concat $ replicate 10000 $ parse input
          -- let ans = iterate phase realsignal !! 100
          -- print $ map (ans !!) [offset .. offset + 8]
