import Data.Maybe
import Data.List
import Debug.Trace

data Mode = Immediate | Address deriving Show

parseOpcode :: Int -> (Int, [Mode])
parseOpcode op = (mod op 100, map parseMode powers)
    where parseMode mode = if mod (div op mode) 10 == 0 then Address else Immediate
          powers = 100:(map (*10) powers) -- [100,1000,10000,...]

set :: Int -> a -> [a] -> [a]
set index element list = map (\(i,x) -> if i == index then element else x) $ zip [0..] list

-- blocked: inputindex, program (to be modified at inputindex with the value), index
-- ready: outputvalue, newprogram, newindex
data IntStream = Blocked (Int, [Int], Int) | Ready (Int, [Int], Int) | Done deriving (Show, Eq)

intcode :: [Int] -> Int -> IntStream
intcode program index =
    let (op, mode1:mode2:_) = parseOpcode $ program !! index
        arg :: Mode -> Int -> Int
        arg mode offset =
            let immediate = program !! (index + offset)
            in case mode of Immediate -> immediate
                            Address -> program !! immediate
    in case op of
           -- add
           1 -> intcode newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (arg mode1 1 + arg mode2 2) program
           -- mul
           2 -> intcode newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (arg mode1 1 * arg mode2 2) program
           -- input
           3 -> Blocked (arg Immediate 1, program, index + 2)
           -- output
           4 -> Ready (arg mode1 1, program, index + 2)
           -- jump if true
           5 -> intcode program $ if arg mode1 1 /= 0 then arg mode2 2 else index + 3
           -- jump if false
           6 -> intcode program $ if arg mode1 1 == 0 then arg mode2 2 else index + 3
           -- less than
           7 -> intcode newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (if arg mode1 1 < arg mode2 2 then 1 else 0) program
           -- equals
           8 -> intcode newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (if arg mode1 1 == arg mode2 2 then 1 else 0) program
           -- halt
           99 -> Done
           -- unknown
           x -> error $ "invalid opcode: " ++ show x

-- this works for part 1
-- amplify program [a,b,c,d,e] =
--     let outa = head $ intcode [a,0] [] program 0
--         outb = head $ intcode [b,outa] [] program 0
--         outc = head $ intcode [c,outb] [] program 0
--         outd = head $ intcode [d,outc] [] program 0
--         oute = head $ intcode [e,outd] [] program 0
--     in oute

-- clever solution, but doesn't work ;-;
-- amplify program [a,b,c,d,e] =
--     let outa = intcode "a" (a:0:oute) [] program 0
--         outb = intcode "b" (b:outa) [] program 0
--         outc = intcode "c" (c:outb) [] program 0
--         outd = intcode "d" (d:outc) [] program 0
--         oute = intcode "e" (e:outd) [] program 0
--     in last $ oute

-- index of other thread to input from / input buffer / output buffer / execution state
type Thread = (Int, [Int], [Int], IntStream)

run :: [Thread] -> Int
run [(4,_,_,Done),(0,_,_,Done),(1,_,_,Done),(2,_,_,Done),(3,_,answer:_,Done)] = answer
run threads = schedule (find isThreadReady $ zip [0..] threads) (find isThreadBlockedWithInput $ zip [0..] threads)
    where isThreadReady (_,(_,_,_,Ready _)) = True
          isThreadReady (_,(_,_,_,_)) = False
          isThreadBlockedWithInput (_,(_,(input:rest),_,Blocked _)) = True
          isThreadBlockedWithInput (_,(_,_,_,_)) = False
          schedule (Just (thisTid, (thisInputTid, thisInput, thisOutput, Ready (val, program, newindex)))) _ =
              let -- find the thread whats blocked on this one's output
                  otherthread = fromJust $ find (\(_,(otherInputTid,_,_,_)) -> otherInputTid == thisTid) $ zip [0..] threads
                  (otherTid, (_, otherinput, otheroutput, otherstate)) = otherthread
                  -- add this threads value into that threads buffer (at the end, ofc)
                  newotherthread = (thisTid, otherinput ++ [val], otheroutput, otherstate)
                  -- execute this thread again
                  newthisthread = (thisInputTid, thisInput, val:thisOutput, intcode program newindex)
              in run $ set thisTid newthisthread $ set otherTid newotherthread threads
          schedule _ (Just (thisTid, (thisInputTid, (val:rest), thisOutput, Blocked (inputindex, program, index)))) =
              let -- feed existing input to the blocked program
                  newprogram = set inputindex val program
                  newthisthread = (thisInputTid, rest, thisOutput, intcode newprogram index)
              in run $ set thisTid newthisthread threads
          schedule Nothing Nothing = error "deadlock"

amplify program [a,b,c,d,e] = run [(4,[a,0],[],p), (0,[b],[],p), (1,[c],[],p), (2,[d],[],p), (3,[e],[],p)]
    where p = intcode program 0

main = do input <- map read <$> words <$> readFile "input.txt"
          print $ maximum $ map (amplify input) $ permutations [0..4]
          print $ maximum $ map (amplify input) $ permutations [5..9]


