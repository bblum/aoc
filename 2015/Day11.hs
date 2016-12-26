{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

increment = map fst . reverse . drop 1 . reverse . scanr increment' (undefined, True)
    where increment' 'z' (_, True) = ('a', True)
          increment'  x  (_, True) = (chr (ord x + 1), False)
          increment'  x  (_, False) = (x, False)

bs1 (a:rest@(b:c:_)) = map ((subtract $ ord a) . ord) [a,b,c] == [0,1,2] || bs1 rest
bs1 _ = False

bs2 = not . any (flip elem "iol")

bs3 x = (length $ nub $ map (take 2) $ sort $ filter ((>1) . length) $ group x) > 1

valid pass = bs1 pass && bs2 pass && bs3 pass

main = print $ until valid increment $ increment "hxbxxyzz"
