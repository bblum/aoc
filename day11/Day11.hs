import qualified Data.Map as M
import Debug.Trace

data Mode = Immediate | Address | Relative deriving Show

parseOpcode :: Int -> (Int, [Mode])
parseOpcode op = (mod op 100, map parseMode powers)
    where parseMode mode = [Address, Immediate, Relative] !! mod (div op mode) 10
          powers = 100:(map (*10) powers) -- [100,1000,10000,...]

set :: Int -> a -> [a] -> [a]
set index value program = map (\(i,x) -> if i == index then value else x) $ zip [0..] program

-- input: input addr, relbase, program, newindex
-- output: output val, relbase, program, newindex
-- done: ez
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
           4 -> Output (arg mode1 1) relbase program $ index + 2
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

data Facing = U | D | L | R
left U = L
left L = D
left D = R
left R = U
right f = left $ left $ left f

turn dir (yx,f) = (move yx newfacing, newfacing)
    where turn 0 f = left f
          turn 1 f = right f
          newfacing = turn dir f
          move (y,x) U = (y-1,x)
          move (y,x) D = (y+1,x)
          move (y,x) L = (y,x-1)
          move (y,x) R = (y,x+1)

black = 0
white = 1
getcolor yx colors = M.findWithDefault black yx colors
setcolor yx c colors = M.insert yx c colors
numpainted colors = M.size colors

type Coord = (Int,Int)
type ColorMap = M.Map Coord Int

robot :: (Coord,Facing) -> ColorMap -> Int -> [Int] -> Int -> ColorMap
robot (yxf@(yx,_)) colors relbase0 program0 index0 =
    case intcode relbase0 program0 index0 of
        Input addr relbase1 program1 index1 -> robot yxf colors relbase1 newprogram index1
            where color = getcolor yx colors
                  newprogram = set addr color program1
        Output color relbase1 program1 index1 ->
            case intcode relbase1 program1 index1 of
                Output dir relbase2 program2 index2 -> robot newyxf newcolors relbase2 program2 index2
                    where newcolors = setcolor yx color colors
                          newyxf = turn dir yxf
                _ -> error "expected two outputs here"
        Done -> colors

render :: ColorMap -> [String]
render colors = zipWith (map . paint) ys $ repeat xs
    where paintedCoords = M.keys colors
          paintedys = map fst paintedCoords
          paintedxs = map snd paintedCoords
          ys = [minimum paintedys .. maximum paintedys]
          xs = [minimum paintedxs .. maximum paintedxs]
          paint y x = pretty $ getcolor (y,x) colors
          pretty 1 = '#'
          pretty 0 = ' '

main = do input <- map read <$> words <$> readFile "input.txt"
          let program = input ++ repeat 0
          let startxy = (0,0)
          let startpos = (startxy, U)
          print $ numpainted $ robot startpos M.empty 0 program 0
          mapM putStrLn $ render $ robot startpos (M.fromList [(startxy,white)]) 0 program 0


