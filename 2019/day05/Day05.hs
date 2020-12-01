import Debug.Trace

data Mode = Immediate | Address deriving Show

parseOpcode :: Int -> (Int, [Mode])
parseOpcode op = (mod op 100, map parseMode powers)
    where parseMode mode = if mod (div op mode) 10 == 0 then Address else Immediate
          powers = 100:(map (*10) powers) -- [100,1000,10000,...]

set :: Int -> Int -> [Int] -> [Int]
set index value program = map (\(i,x) -> if i == index then value else x) $ zip [0..] program

intcode :: [Int] -> [Int] -> [Int] -> Int -> [Int]
intcode input output program index =
    let (op, mode1:mode2:_) = parseOpcode $ program !! index
        arg :: Mode -> Int -> Int
        arg mode offset =
            let immediate = program !! (index + offset)
            in case mode of Immediate -> immediate
                            Address -> program !! immediate
    in case op of
           -- add
           1 -> intcode input output newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (arg mode1 1 + arg mode2 2) program
           -- mul
           2 -> intcode input output newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (arg mode1 1 * arg mode2 2) program
           -- input
           3 -> intcode (tail input) output newprogram $ index + 2
                    where newprogram = set (arg Immediate 1) (head input) program
           -- output
           4 -> intcode input (arg mode1 1 : output) program $ index + 2
           -- jump if true
           5 -> intcode input output program $ if arg mode1 1 /= 0 then arg mode2 2 else index + 3
           -- jump if false
           6 -> intcode input output program $ if arg mode1 1 == 0 then arg mode2 2 else index + 3
           -- less than
           7 -> intcode input output newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (if arg mode1 1 < arg mode2 2 then 1 else 0) program
           -- equals
           8 -> intcode input output newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (if arg mode1 1 == arg mode2 2 then 1 else 0) program
           -- halt
           99 -> output
           -- unknown
           x -> error $ "invalid opcode: " ++ show x

main = do input <- map read <$> words <$> readFile "input.txt"
          print $ head $ intcode [1] [] input 0
          print $ head $ intcode [5] [] input 0


