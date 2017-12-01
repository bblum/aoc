import qualified Data.Map as M
import Data.Map hiding (map, filter, foldl, foldr)
import Data.List hiding (insert, lookup)
import Data.Maybe
import Control.Arrow

key val input = fst $ fromJust $ find ((== val) . snd) $ toList input

permutation = [1,3,5,7,2,4,6,0]
rotate input x magic = mapKeys (\k -> (k + (magic ! key x input) - key x input) `mod` size input) input

parse1 input ["swap","position",x,_,_,y]   = insert (read x) (input ! read y) $ insert (read y) (input ! read x) input
parse1 input ["swap","letter",[x],_,_,[y]] = insert (key x input) y $ insert (key y input) x input
parse1 input ["rotate",_,_,_,_,_,[x]]      = rotate input x $ fromList $ zip [0..] permutation
parse1 input ["rotate","left", n,_]        = mapKeys (\k -> (k - read n) `mod` size input) input
parse1 input ["rotate","right",n,_]        = mapKeys (\k -> (k + read n) `mod` size input) input
parse1 input ["reverse",_,n,_,m]           = fromList $ pfx ++ zip (reverse ks) vs ++ sfx
    where (pfx,(mid,sfx)) = splitAt (read m - read n + 1) <$>  splitAt (read n) (toList input)
          (ks,vs) = unzip mid
parse1 input ["move",_,n,_,_,m]            = fromList $ zip [0..] $ pfx ++ [input ! read n] ++ sfx
    where (pfx,sfx) = splitAt (read m) $ filter (/= input ! read n) $ snd $ unzip $ toList input

parse2 ["rotate",_,_,_,_,_,[x]] input = rotate input x $ fromList $ zip permutation [0..]
parse2 ["move",_,n,_,_,m]       input = parse1 input ["move","",m,"","",n]
parse2 ("rotate":"right":rest)  input = parse1 input $ "rotate":"left":rest
parse2 ("rotate":"left":rest)   input = parse1 input $ "rotate":"right":rest
parse2 cmd                      input = parse1 input cmd

p1 = fromList $ zip [0..] "abcdefgh"
p2 = fromList $ zip [0..] "fbgdceah"
main = interact $ (++"\n") . show . (elems . foldl parse1 p1 &&& elems . foldr parse2 p2) . map words . lines
