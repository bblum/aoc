import Data.Maybe
import Data.List
import Debug.Trace

data Mode = Immediate | Address deriving Show

parseOpcode :: Int -> (Int, [Mode])
parseOpcode op = (mod op 100, map parseMode powers)
    where parseMode mode = if mod (div op mode) 10 == 0 then Address else Immediate
          powers = 100:(map (*10) powers) -- [100,1000,10000,...]

set :: Int -> Int -> [Int] -> [Int]
set index value program = map (\(i,x) -> if i == index then value else x) $ zip [0..] program

-- blockeD: name, program, index
-- ready: output value, name, existinginput newprogram, newindex
data IntStream = Blocked (String, [Int], Int) | Ready (Int, String, [Int], [Int], Int) | Done deriving (Show, Eq)

intcode :: String -> [Int] -> [Int] -> Int -> IntStream
intcode name input program index =
    let (op, mode1:mode2:_) = parseOpcode $ program !! index
        arg :: Mode -> Int -> Int
        arg mode offset =
            let immediate = program !! (index + offset)
            in case mode of Immediate -> immediate
                            Address -> program !! immediate
    in case op of
           -- add
           1 -> intcode name input newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (arg mode1 1 + arg mode2 2) program
           -- mul
           2 -> intcode name input newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (arg mode1 1 * arg mode2 2) program
           -- input
           3 -> case input of
                    [] -> Blocked (name, program, index)
                    -- (x:rest) -> traceShow (name ++ " <- " ++ show x) $ intcode name rest (set (arg Immediate 1) x program) $ index + 2
                    (x:rest) -> intcode name rest (set (arg Immediate 1) x program) $ index + 2
           -- output
           -- 4 -> intcode name input (arg mode1 1 : output) program $ index + 2
           4 -> Ready (arg mode1 1, name, input, program, index + 2)
           -- jump if true
           5 -> intcode name input program $ if arg mode1 1 /= 0 then arg mode2 2 else index + 3
           -- jump if false
           6 -> intcode name input program $ if arg mode1 1 == 0 then arg mode2 2 else index + 3
           -- less than
           7 -> intcode name input newprogram $ index + 4
                    where newprogram = set (arg Immediate 3) (if arg mode1 1 < arg mode2 2 then 1 else 0) program
           -- equals
           8 -> intcode name input newprogram $ index + 4
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

isThreadReady (_,(_,_,_,Ready _)) = True
isThreadReady (_,(_,_,_,_)) = False

isThreadBlockedWithInput (_,(_,(input:rest),_,Blocked _)) = True
isThreadBlockedWithInput (_,(_,_,_,_)) = False

type Thread = (Int, [Int], [Int], IntStream)

executeThreads :: [Thread] -> Int
executeThreads [(4,_,_,Done),(0,_,_,Done),(1,_,_,Done),(2,_,_,Done),(3,_,answer:_,Done)] = answer
executeThreads threads =
    case find isThreadReady $ zip [0..] threads of
        -- TODO: find blocked threads and feed it input
        Nothing -> case find isThreadBlockedWithInput $ zip [0..] threads of
                       Nothing -> error "deadlock"
                       Just (thisTid, (thisInputTid, thisInput, thisOutput, Blocked (name, program, index))) ->
                           let newthisthread = (thisInputTid, [], thisOutput, intcode name thisInput program index)
                               replacethread (tid, thread) | tid == thisTid = newthisthread
                               replacethread (tid, thread) = thread
                               newallthreads = map replacethread $ zip [0..] threads
                           in executeThreads newallthreads
        -- unused values: this threads inputting thread, this threads buffered input
        Just (thisTid, (thisInputTid, thisInput, thisOutput, Ready (val, name, existingInput, program, newindex))) ->
            -- find the thread whats blocked on this one's output
            let otherthread = fromJust $ find (\(_,(otherInputTid,_,_,_)) -> otherInputTid == thisTid) $ zip [0..] threads
                (otherTid, (_, otherinput, otheroutput, otherstate)) = otherthread
                -- add this threads value into that threads buffer (at the end, ofc)
                newotherthread :: Thread
                newotherthread = (thisTid, otherinput ++ [val], otheroutput, otherstate)
                -- execute this thread again
                -- TODO: get rid of 'existingInput' - just let it block *always* on input opcode,
                -- and let the scheduler feed it
                newthisthread :: Thread
                newthisthread = (thisInputTid, thisInput, val:thisOutput, intcode name existingInput program newindex)
                replacethread (tid, thread) | tid == thisTid = newthisthread
                replacethread (tid, thread) | tid == otherTid = newotherthread
                replacethread (tid, thread) = thread
                newallthreads = map replacethread $ zip [0..] threads
            in executeThreads newallthreads
        Just (_,(_,_,_, asdf)) -> error $ "impossible " ++ show asdf

amplify program [a,b,c,d,e] =
    let proga = intcode "a" [a,0] program 0
        progb = intcode "b" [b] program 0
        progc = intcode "c" [c] program 0
        progd = intcode "d" [d] program 0
        proge = intcode "e" [e] program 0
        -- index of other thread to input from / input buffers / output / thread states
        threads = [(4,[],[],proga), (0,[],[],progb), (1,[],[],progc), (2,[],[],progd), (3,[],[],proge)]
    in executeThreads threads


main = do input <- map read <$> words <$> readFile "input.txt"
          print $ maximum $ map (amplify input) $ permutations [0..4]
          print $ maximum $ map (amplify input) $ permutations [5..9]


