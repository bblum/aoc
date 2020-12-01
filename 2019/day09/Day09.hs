import Debug.Trace

data Mode = Immediate | Address | Relative deriving Show

parseOpcode :: Int -> (Int, [Mode])
parseOpcode op = (mod op 100, map parseMode powers)
    where parseMode mode = [Address, Immediate, Relative] !! mod (div op mode) 10
          powers = 100:(map (*10) powers) -- [100,1000,10000,...]

set :: Int -> Int -> [Int] -> [Int]
set index value program = map (\(i,x) -> if i == index then value else x) $ zip [0..] program

intcode :: Int -> [Int] -> [Int] -> Int -> [Int]
intcode relbase input program index =
    let (op, mode1:mode2:mode3:_) = parseOpcode $ program !! index
        arg :: Mode -> Int -> Int
        arg mode offset =
            let immediate = program !! (index + offset)
            in case mode of Immediate -> immediate
                            Address -> program !! immediate
                            Relative -> program !! (relbase + immediate)
        oarg mode offset =
            let immediate = program !! (index + offset)
            in case mode of Relative -> relbase + immediate
                            _ -> immediate
    in case op of
           -- add
           1 -> intcode relbase input newprogram $ index + 4
                    where newprogram = set (oarg mode3 3) (arg mode1 1 + arg mode2 2) program
           -- mul
           2 -> intcode relbase input newprogram $ index + 4
                    where newprogram = set (oarg mode3 3) (arg mode1 1 * arg mode2 2) program
           -- input
           3 -> intcode relbase (tail input) newprogram $ index + 2
                    where newprogram = set (oarg mode1 1) (head input) program
           -- output
           4 -> arg mode1 1 : (intcode relbase input program $ index + 2)
           -- jump if true
           5 -> intcode relbase input program $ if arg mode1 1 /= 0 then arg mode2 2 else index + 3
           -- jump if false
           6 -> intcode relbase input program $ if arg mode1 1 == 0 then arg mode2 2 else index + 3
           -- less than
           7 -> intcode relbase input newprogram $ index + 4
                    where newprogram = set (oarg mode3 3) (if arg mode1 1 < arg mode2 2 then 1 else 0) program
           -- equals
           8 -> intcode relbase input newprogram $ index + 4
                    where newprogram = set (oarg mode3 3) (if arg mode1 1 == arg mode2 2 then 1 else 0) program
           -- relbase
           9 -> intcode newrelbase input program $ index + 2
                    where newrelbase = relbase + arg mode1 1
           -- halt
           99 -> []
           -- unknown
           x -> error $ "invalid opcode: " ++ show x

main = do input <- map read <$> words <$> readFile "input.txt"
          print $ intcode 0 [1] (input ++ repeat 0) 0
          print $ intcode 0 [2] (input ++ repeat 0) 0


