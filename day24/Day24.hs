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

asdf line = head $ if length (nub line) == 1 then nub line else [concat $ instr:vals]
    where instr = reverse $ tail $ reverse $ head line
          vals = map ((:[]) . last) line

-- just to validate my disassembly
opcode "add" a b = a + b
opcode "mul" a b = a * b
opcode "div" a b = div a b
opcode "mod" a b = mod a b
opcode "eql" a b = if a == b then 1 else 0
instruction (w,vars) [op,[a],b] = (w, M.insert a (opcode op (vars M.! a) bval) vars)
    where bval = fromMaybe (read b) $ M.lookup (head b) vars
instruction (w,vars) ["inp","w"] = (tail w, M.insert 'w' (digitToInt $ head w) vars)
interpret w = snd . foldl instruction (w, M.fromList $ zip "xyzw" $ repeat 0)

-- it is in fact like a lock
-- so
-- wait, what?
--
-- (INF,6) ==== stage 0 ==== -- x guaranteed to be set
-- z = [6 + w0]
-- (INF,14) ==== stage 1 ==== -- x guaranteed to be set
-- z = [6 + w0, 14 + w1]
-- (INF,14) ==== stage 2 ==== -- x guaranteed to be set
-- z = [6 + w0, 14 + w1, 14 + w2]
-- (-8,10) ==== stage 3 ==== b=26
-- we can turn off x here if w3 = [14 + w2 - 8] == w2+6
-- z = [6 + w0, 14 + w1]
-- (INF,9) ==== stage 4 ==== -- x guaranteed to be set
-- z = [6 + w0, 14 + w1, 9 + w4]
-- (INF,12) ==== stage 5 ==== -- x guaranteed to be set
-- z = [6 + w0, 14 + w1, 9 + w4, 12 + w5]
-- (-11,8) ==== stage 6 ==== b=26
-- we can turn off x here if w6 = [12 + w5 - 11] == w5+1
-- z = [6 + w0, 14 + w1, 9 + w4]
-- (-4,13) ==== stage 7 ==== b=26
-- we can turn off x here if w7 = [9 + w4 - 4] = w4+5
-- z = [6 + w0, 14 + w1]
-- (-15,12) ==== stage 8 ==== b=26
-- we can turn off x here if w8 = [14 + w1 - 14] = w1-1
-- z = [6 + w0]
-- (INF,6) ==== stage 9 ==== -- x guaranteed to be set
-- z = [6 + w0, 6 + w9]
-- (INF,9) ==== stage 10 ==== -- x guaranteed to be set
-- z = [6 + w0, 6 + w9, 9 + w10]
-- (-1,15) ==== stage 11 ==== b=26
-- we can turn off x here if w11 = [9 + w10 - 1] = w10+8
-- z = [6 + w0, 6 + w9]
-- (-8,4) ==== stage 12 ==== b=26
-- we can turn off x here if w12 = [6 + w9 - 8] = w9-2
-- z = [6 + w0]
-- (-14,10) ==== stage 13 ==== b=26
-- we can turn off x here if w13 = [6 + w0 - 14] - w0-8



--
--
-- old work
-- (-8,4) ==== stage 12 ==== b=26
-- z = 26 * z13 + ???
--
-- (-14,10) ==== stage 13 ==== b=26
-- 10 will fuck anything so this "pin" (-14) has to match
-- b is 1 so top bits hafta be all 0
-- z13 = w + 14
-- so z13 \in [14..23]


ps = [11,13,15,-8,13,15,-11,-4,-15,14,14,-1,-8,-14]
qs = [6,14,14,10,9,12,8,13,12,6,9,15,4,10]

bs = [1,1,1,26,1,1,26,26,26,1,1,26,26,26]

program :: Int -> (Int,(Int,Int,Int)) -> Int
program z (w,(b,p,q)) = if w == p + mod z 26 then z2 else (z2*26) + w + q
    where z2 = div z b

try :: String -> [Int]

tryAt index z w = scanl program z $ zip (map digitToInt w) $ drop index $ zip3 bs ps qs

try = tryAt 0 0

validate input d = last (try w) == interpret w input M.! 'z' where w = replicate 14 d

main = do input <- map words <$> lines <$> readFile "input.txt"
          when (not $ all id $ map (validate input) "0123456789") $ error "disassembly is wrong"
          mapM print $ map asdf $ transpose $ splitOn [["inp","w"]] $ input
          print $ try "99394899891971"
          print $ interpret "99394899891971" input
          print $ try "81060015020800"
          print $ interpret "81060015020800" input

w0 = undefined -- 8 or 9
w1 = undefined -- 1..9
w2 = undefined -- 0..3
w3 = w2+6
w4 = undefined -- 0..4
w5 = undefined -- 0..8
w6 = w5+1
w7 = w4+5
w8 = w1-1
w9 = undefined -- 2..9
w10 = undefined -- 0 or 1
w11 = w10+8
w12 = w9-2
w13 = w0-8
