{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Bits
import Data.Char
import Data.Ord
import Control.Monad.State
import Control.Arrow
import Debug.Trace

samp :: [[String]] -> [[[Int]]]
samp ( ("Before:":a):b:("After:":c):d:rest) = [map read a,map read b,map read c]:samp rest
samp _ = []

prog :: [[String]] -> [[Int]]
prog ( ("Before:":a):b:("After:":c):d:rest) = prog rest
prog ([]:rest) = prog rest
prog x = map (map read) x

wr 0 v [a,b,c,d] = [v,b,c,d]
wr 1 v [a,b,c,d] = [a,v,c,d]
wr 2 v [a,b,c,d] = [a,b,v,d]
wr 3 v [a,b,c,d] = [a,b,c,v]

match :: [[Int]] -> Int -> [Int]
match [regs,[i,j,k,l]] 0 = 
    wr l ((regs!!j)+(regs!!k)) regs
match [regs,[i,j,k,l]] 1 = 
    wr l ((regs!!j)+(k)) regs
match [regs,[i,j,k,l]] 2 = 
    wr l ((regs!!j)*(regs!!k)) regs
match [regs,[i,j,k,l]] 3 = 
    wr l ((regs!!j)*(k)) regs
match [regs,[i,j,k,l]] 4 = 
    wr l ((regs!!j) .&. (regs!!k)) regs
match [regs,[i,j,k,l]] 5 = 
    wr l ((regs!!j) .&. (k)) regs
match [regs,[i,j,k,l]] 6 = 
    wr l ((regs!!j) .|. (regs!!k)) regs
match [regs,[i,j,k,l]] 7 = 
    wr l ((regs!!j) .|. (k)) regs
match [regs,[i,j,k,l]] 8 = 
    wr l ((regs!!j)) regs
match [regs,[i,j,k,l]] 9 = 
    wr l ((j)) regs
-- gt
match [regs,[i,j,k,l]] 10 = 
    wr l (if (j) > (regs !! k) then 1 else 0) regs
match [regs,[i,j,k,l]] 11 = 
    wr l (if (regs !! j) > (k) then 1 else 0) regs
match [regs,[i,j,k,l]] 12 = 
    wr l (if (regs !! j) > (regs !! k) then 1 else 0) regs
-- eq
match [regs,[i,j,k,l]] 13 = 
    wr l (if (j) == (regs !! k) then 1 else 0) regs
match [regs,[i,j,k,l]] 14 = 
    wr l (if (regs !! j) == (k) then 1 else 0) regs
match [regs,[i,j,k,l]] 15 = 
    wr l (if (regs !! j) == (regs !! k) then 1 else 0) regs

match' :: [[Int]] -> Int -> Bool
match' [[a0,b0,c0,d0],[i,j,k,l],result] op = match [[a0,b0,c0,d0],[i,j,k,l]] op == result

part1 sample = (length $ filter (match' sample) [0..15]) >= 3

key = [(7,2),(3,3),(14,1),(6,7),(12,0),(5,6),(9,9),(11,14),(4,13),(8,12),(2,11),(1,15),(13,10),(0,8),(15,5),(10,4)]
getonly sample =
    let matches = filter (match' sample) $ [0..15] \\ [2,3,1,0,7,9,6,14,13,12,11,15,10,8,5,4]
        [_,(opcode:_),_] = sample
    in if length matches == 1 then Just (opcode,head matches) else Nothing

decode samples = mapMaybe getonly samples

execute :: [Int] -> [Int] -> [Int]
execute regs (instr@(opcode:_)) =
    let realopcode = fromJust $ lookup opcode key
    in match [regs,instr] realopcode

main = do input <- map words <$> lines <$> readFile "input.txt"
          print $ length $ filter part1 $ samp input
          -- print $ decode $ samp input
          print $ head $ foldl execute [0,0,0,0] $ prog input
