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

input = [5,8,3,9,7,6,2,4,1]

test = [3,8,9,1,2,5,4,6,7]

splitAfter n list = (take (idx+1) list, drop (idx+1) list)
    where idx = fst $ head $ filter ((== n) . snd) $ zip [0..] list

move (c:cups) = traceShow (current, dest, "part2 hint", take 2 after1) $ ans
    where cups2 = cups ++ [c]
          (next, rest) = (take 3 cups2, drop 3 cups2)
          current = c
          dest = case filter (< c) rest of
                     [] -> maximum rest -- wrap around
                     ok -> maximum ok -- take biggest not exceeding
          (pfx,sfx) = splitAfter dest rest
          newcups = pfx ++ next ++ sfx
          (pfx2,sfx2) = splitAfter (c) newcups
          ans = sfx2 ++ pfx2
          (_,after1) = splitAfter 1 ans

main = do --
          -- print $ iterate move input !! 100
          let input2 = take 1000000 $ input ++ (drop (length input) [1..])
          print $ iterate move input2 !! 10000000
