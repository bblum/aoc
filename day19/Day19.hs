import qualified Data.Set as S
import Math.NumberTheory.ArithmeticFunctions
import Data.Maybe

wr regnum f regs = map (\(r,v) -> if r == regnum then f v else v) $ zip [0..] regs
set regnum regs v = wr regnum (const v) regs

instr regs ("addi", [a,b,c]) = set c regs $ regs !! a + b
instr regs ("addr", [a,b,c]) = set c regs $ regs !! a + regs !! b
instr regs ("muli", [a,b,c]) = set c regs $ regs !! a * b
instr regs ("mulr", [a,b,c]) = set c regs $ regs !! a * regs !! b
instr regs ("seti", [a,b,c]) = set c regs $ a
instr regs ("setr", [a,b,c]) = set c regs $ regs !! a
instr regs ("gtrr", [a,b,c]) = set c regs $ fromEnum $ regs !! a > regs !! b
instr regs ("eqrr", [a,b,c]) = set c regs $ fromEnum $ regs !! a == regs !! b

execute ip prog regs | regs !! ip == 1 = sum [ x | y <- ns, x <- ns, y*x == regs !! 2 ]
    where ns = S.toList $ divisors $ regs !! 2
execute ip prog regs | (regs !! ip) >= length prog = regs !! 0
execute ip prog regs = execute ip prog $ wr ip (+1) $ instr regs $ prog !! (regs !! ip)

parse (instr:rest) | length rest >= 3 = Just (instr, map read $ take 3 rest)
parse _ = Nothing

main = do (["#ip",ip]:prog) <- map words <$> lines <$> readFile "input.txt"
          print $ execute (read ip) (mapMaybe parse prog) [0,0,0,0,0,0]
          print $ execute (read ip) (mapMaybe parse prog) [1,0,0,0,0,0]
