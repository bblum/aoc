import Data.List
import Data.Maybe
import Data.Bits

samp (("Before:":a):b:("After:":c):d:rest) = map (map read) [a,b,c]:samp rest
samp _ = []

prog ( ("Before:":a):b:c:d:rest) = prog rest
prog ([]:rest) = prog rest
prog x = map (map read) x

wr 0 [a,b,c,d] v = [v,b,c,d]
wr 1 [a,b,c,d] v = [a,v,c,d]
wr 2 [a,b,c,d] v = [a,b,v,d]
wr 3 [a,b,c,d] v = [a,b,c,v]

exec regs [i,j,k,l]  0 = wr l regs $ (regs!!j) + (regs!!k)
exec regs [i,j,k,l]  1 = wr l regs $ (regs!!j) + k
exec regs [i,j,k,l]  2 = wr l regs $ (regs!!j) * (regs!!k)
exec regs [i,j,k,l]  3 = wr l regs $ (regs!!j) * k
exec regs [i,j,k,l]  4 = wr l regs $ (regs!!j) .&. (regs!!k)
exec regs [i,j,k,l]  5 = wr l regs $ (regs!!j) .&. k
exec regs [i,j,k,l]  6 = wr l regs $ (regs!!j) .|. (regs!!k)
exec regs [i,j,k,l]  7 = wr l regs $ (regs!!j) .|. k
exec regs [i,j,k,l]  8 = wr l regs $ regs!!j
exec regs [i,j,k,l]  9 = wr l regs j
exec regs [i,j,k,l] 10 = wr l regs $ fromEnum $ (j) > (regs !! k)
exec regs [i,j,k,l] 11 = wr l regs $ fromEnum $ (regs !! j) > (k)
exec regs [i,j,k,l] 12 = wr l regs $ fromEnum $ (regs !! j) > (regs !! k)
exec regs [i,j,k,l] 13 = wr l regs $ fromEnum $ (j) == (regs !! k)
exec regs [i,j,k,l] 14 = wr l regs $ fromEnum $ (regs !! j) == (k)
exec regs [i,j,k,l] 15 = wr l regs $ fromEnum $ (regs !! j) == (regs !! k)

match [input,instr,result] op = exec input instr op == result

part1 sample = (length $ filter (match sample) [0..15]) >= 3

key = [(7,2),(3,3),(14,1),(6,7),(12,0),(5,6),(9,9),(11,14),(4,13),(8,12),(2,11),(1,15),(13,10),(0,8),(15,5),(10,4)]

getonly sample =
    let matches = filter (match sample) $ [0..15] \\ [2,3,1,0,7,9,6,14,13,12,11,15,10,8,5,4]
        [_,(opcode:_),_] = sample
    in if length matches == 1 then Just (opcode,head matches) else Nothing

decode samples = mapMaybe getonly samples

execute regs (instr@(opcode:_)) = exec regs instr $ fromJust $ lookup opcode key

main = do input <- map words <$> lines <$> readFile "input.txt"
          print $ length $ filter part1 $ samp input
          -- print $ decode $ samp input
          print $ head $ foldl execute [0,0,0,0] $ prog input
