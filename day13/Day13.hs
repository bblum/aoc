import qualified Data.Map as M
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

data Mode = Immediate | Address | Relative deriving Show

parseOpcode :: Int -> (Int, [Mode])
parseOpcode op = (mod op 100, map parseMode powers)
    where parseMode mode = [Address, Immediate, Relative] !! mod (div op mode) 10
          powers = 100:(map (*10) powers) -- [100,1000,10000,...]

set :: Int -> Int -> [Int] -> [Int]
set index value program = map (\(i,x) -> if i == index then value else x) $ zip [0..] program

data IntStream = Input Int Int [Int] Int | Output Int Int [Int] Int | Done

intcode :: Int -> [Int] -> Int -> IntStream
intcode relbase program index =
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
           1 -> intcode relbase newprogram $ index + 4
                    where newprogram = set (oarg mode3 3) (arg mode1 1 + arg mode2 2) program
           -- mul
           2 -> intcode relbase newprogram $ index + 4
                    where newprogram = set (oarg mode3 3) (arg mode1 1 * arg mode2 2) program
           -- input
           3 -> Input (oarg mode1 1) relbase program $ index + 2
           -- output
           -- 4 -> Output (arg mode1 1) relbase program $ index + 2
           -- 4 -> arg mode1 1 : (intcode relbase input program $ index + 2)
           4 -> let val = arg mode1 1
                in if program !! (index + 1) == 386 then traceShow ("output",index, [program !! index, program !! (index + 1)], arg Immediate 1, arg mode1 1) $ Output val relbase program $ index + 2
                else Output val relbase program $ index + 2
           -- 4 -> let val = arg mode1 1
           --      in if program !! (index + 1) == 386 then traceShow ("output",index, [program !! index, program !! (index + 1)], arg Immediate 1, arg mode1 1) $ val : (intcode relbase input program $ index + 2)
           --      else val : (intcode relbase input program $ index + 2)
           -- jump if true
           5 -> intcode relbase program $ if arg mode1 1 /= 0 then arg mode2 2 else index + 3
           -- jump if false
           6 -> intcode relbase program $ if arg mode1 1 == 0 then arg mode2 2 else index + 3
           -- less than
           7 -> intcode relbase newprogram $ index + 4
                    where newprogram = set (oarg mode3 3) (if arg mode1 1 < arg mode2 2 then 1 else 0) program
           -- equals
           8 -> intcode relbase newprogram $ index + 4
                    where newprogram = set (oarg mode3 3) (if arg mode1 1 == arg mode2 2 then 1 else 0) program
           -- relbase
           9 -> intcode newrelbase program $ index + 2
                    where newrelbase = relbase + arg mode1 1
           -- halt
           99 -> Done
           -- unknown
           x -> error $ "invalid opcode: " ++ show x

run input relbase program index =
    case intcode relbase program index of
        Input addr relbase1 program1 index1 -> run (tail input)relbase1 newprogram index1
            where newprogram = set addr (head input) program1
        Output val relbase1 program1 index1 -> val : run input relbase1 program1 index1
        Done -> []

-- balls: trail of places the ball has ever been, for legibility
-- board: map of the latest state for each pixel (or score)
game balls paddle board relbase program index =
    case intcode relbase program index of
        Input addr relbase1 program1 index1 -> game balls paddle board relbase1 newprogram index1
            where ball_x = fst $ head balls
                  -- move toward the ball
                  input = if ball_x < paddle then -1 else if ball_x > paddle then 1 else 0
                  newprogram = set addr input program1
        -- expect to read 3 values at a time
        Output x relbase1 program1 index1 ->
            case intcode relbase1 program1 index1 of
                Output y relbase2 program2 index2 ->
                    case intcode relbase2 program2 index2 of
                        Output pixel relbase3 program3 index3 -> game newballs newpaddle newboard relbase3 program3 index3
                            where newballs = if pixel == 4 then (x,y):balls else balls
                                  newpaddle = if pixel == 3 then x else paddle
                                  newboard = M.insert (x,y) pixel board
                        _ -> error "expected pixel or score value here"
                _ -> error "expected y coordinate here"
        -- overlay the ball trail on the board
        Done -> foldl add_ball_trail board $ tail balls
            where add_ball_trail b xy = M.insert xy 5 b

render :: M.Map (Int,Int) Int -> [String]
render board = image ++ [show scores]
    where isScore (-1,0) _ = True
          isScore _ _ = False
          (scores,pixels) = M.partitionWithKey isScore board
          xs = map fst $ M.keys pixels
          ys = map snd $ M.keys pixels
          xrange = [minimum xs .. maximum xs]
          yrange = [minimum ys .. maximum ys]
          -- print the latest version of each pixel
          -- adding special value here "." for the ball trail
          interpret y x = " #X_o." !! (pixels M.! (x,y))
          -- interpret y x = if ever_ball then '.' else " #X_o." !! tileid
          --     where ever_ball = any ((==4) . snd) $ filter ((== (x,y)) . fst) pixels
          --           tileid = fromMaybe 0 $ lookup (x,y) $ reverse pixels
          image = zipWith (\y xs -> map (interpret y) xs) yrange $ repeat xrange

play :: [Int] -> [String]
play program = render $ game [] undefined M.empty 0 (program ++ replicate 1000 0) 0

main = do input <- map read <$> words <$> readFile "input.txt"
          -- part 1
          let result = run [] 0 (input ++ repeat 0) 0
          print $ length $ filter (==2) $ map (!! 2) $ chunksOf 3 result
          -- part 2, doesn't work, thanks haskell >_<
          -- let program = set 0 2 input
          -- mapM putStrLn $ play program



