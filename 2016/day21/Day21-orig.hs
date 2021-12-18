{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

input = M.fromList $ zip [0..] "abcdefgh"
input2 = M.fromList $ zip [0..] "fbgdceah"

getKey val input = fst $ fromJust $ find ((== val) . snd) $ M.toList input

parse input ["swap","position",x,_,_,y] =
    let x0 = read x
        y0 = read y
        xval = fromJust $ M.lookup x0 input
        yval = fromJust $ M.lookup y0 input
    in M.insert x0 yval $ M.insert y0 xval input
parse input ["swap","letter",[x],_,_,[y]] =
    let xindex = getKey x input
        yindex = getKey y input
    in M.insert xindex y $ M.insert yindex x input
parse input ["rotate","based",_,_,_,_,[x]] =
    let xindex = getKey x input
        mystery = if xindex >= 4 then 1 else 0
    in parse input ["rotate","right",show (xindex + 1 + mystery),""]
parse input ["rotate","left",n,_] =
    let sz = M.size input
    in M.fromList $ map (\(k,v) -> ((k + sz - read n) `mod` sz, v)) $ M.toList input
parse input ["rotate","right",n,_] =
    let sz = M.size input
    in M.fromList $ map (\(k,v) -> ((k + read n) `mod` sz, v)) $ M.toList input
parse input ["reverse",_,n,_,m] =
    let (pfx,mid0) = splitAt (read n) $ M.toList input
        (mid,sfx) = splitAt (read m - read n + 1) mid0
        (ks,vs) = unzip mid
        newmid = zip (reverse ks) vs
    in M.fromList $ pfx ++ newmid ++ sfx
parse input ["move",_,n,_,_,m] =
    let (_,vals) = unzip $ M.toList input
        val = fromJust $ M.lookup (read n) input
        newlist = filter (/= val) vals
        (pfx,sfx) = splitAt (read m) newlist
    in M.fromList $ zip [0..] $ pfx ++ [val] ++ sfx

magic = M.fromList $ zip [1,3,5,7,2,4,6,0] [0..]

parse2 :: M.Map Int Char -> [String] -> M.Map Int Char
parse2 input ["swap","position",x,_,_,y] =
    let x0 = read x
        y0 = read y
        xval = fromJust $ M.lookup x0 input
        yval = fromJust $ M.lookup y0 input
    in M.insert x0 yval $ M.insert y0 xval input
parse2 input ["swap","letter",[x],_,_,[y]] =
    let xindex = getKey x input
        yindex = getKey y input
    in M.insert xindex y $ M.insert yindex x input
parse2 input ["rotate","based",_,_,_,_,[x]] =
    -- let xindex = getKey x input
    --     mystery = if xindex >= 4 then 1 else 0
    -- in parse2 input ["rotate","right",show (xindex + 1 + mystery),""]
    let xindex = getKey x input
        destination = fromJust $ M.lookup xindex magic
        sz = M.size input
    in M.fromList $ map (\(k,v) -> ((k + sz + destination - xindex) `mod` sz, v)) $ M.toList input
parse2 input ["rotate","left",n,_] =
    let sz = M.size input
    in M.fromList $ map (\(k,v) -> ((k + read n) `mod` sz, v)) $ M.toList input
parse2 input ["rotate","right",n,_] =
    let sz = M.size input
    in M.fromList $ map (\(k,v) -> ((k + sz - read n) `mod` sz, v)) $ M.toList input
parse2 input ["reverse",_,n,_,m] =
    let (pfx,mid0) = splitAt (read n) $ M.toList input
        (mid,sfx) = splitAt (read m - read n + 1) mid0
        (ks,vs) = unzip mid
        newmid = zip (reverse ks) vs
    in M.fromList $ pfx ++ newmid ++ sfx
parse2 input ["move",_,n,_,_,m] =
    let (_,vals) = unzip $ M.toList input
        val = fromJust $ M.lookup (read m) input
        newlist = filter (/= val) vals
        (pfx,sfx) = splitAt (read n) newlist
    in M.fromList $ zip [0..] $ pfx ++ [val] ++ sfx

-- main = interact $ (++"\n") . show . snd . unzip . M.toList . foldl parse input . map words . lines
main = interact $ (++"\n") . show . snd . unzip . M.toList . foldr (flip parse2) input2 . map words . lines
