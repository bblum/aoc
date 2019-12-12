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

execute ip prog regs | (regs !! ip) >= length prog = regs !! 0
execute ip prog regs = execute ip prog $ wr ip (+1) $ instr regs $ prog !! (regs !! ip)

parse (instr:rest) = (instr, map read $ take 3 rest)

main = let (["#ip",ip]:prog) = map words input
       in print $ execute (read ip) (map parse prog) [1,0,0,0,0,0]

input = ["#ip 3",
    "addi 3 16 3",
    "seti 1  6 1",
    "seti 1  4 5",
    "mulr 1  5 4",
    "eqrr 4  2 4",
    "addr 4  3 3",
    "addi 3  1 3",
    "addr 1  0 0",
    "addi 5  1 5",
    "gtrr 5  2 4",
    "addr 3  4 3",
    "seti 2  6 3",
    "addi 1  1 1",
    "gtrr 1  2 4",
    "addr 4  3 3",
    "seti 1  1 3",
    "mulr 3  3 3",
    "addi 2  2 2",
    "mulr 2  2 2",
    "mulr 3  2 2",
    "muli 2 11 2",
    "addi 4  8 4",
    "mulr 4  3 4",
    "addi 4 12 4",
    "addr 2  4 2",
    "addr 3  0 3",
    "seti 0  2 3",
    "setr 3  9 4",
    "mulr 4  3 4",
    "addr 3  4 4",
    "mulr 3  4 4",
    "muli 4 14 4",
    "mulr 4  3 4",
    "addr 2  4 2",
    "seti 0  4 0",
    "seti 0  3 3"]
