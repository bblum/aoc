{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

data ProgState = Done | Send Int | Recv String

eval :: [[String]] -> Int -> (M.Map String Int, Int) -> [[String]] -> (Int, M.Map String Int, Int, ProgState)
eval code ip (regs,freq) [] = (ip,regs,freq,Done)
eval code ip (regs,freq) (["set",reg,val]:rest) = --traceShow regs $ traceShow "set" $
    eval code (ip+1) (M.insert reg (fromMaybe (read val) $ M.lookup val regs) regs, freq) rest
eval code ip (regs,freq) (["mul",reg,val]:rest) = --traceShow regs $ traceShow "mul" $
    eval code (ip+1) (M.alter (\x -> Just $ (fromMaybe (read val) $ M.lookup val regs) * (fromMaybe 0 x)) reg regs, freq) rest
eval code ip (regs,freq) (["add",reg,val]:rest) = --traceShow regs $ traceShow "add" $
    eval code (ip+1) (M.alter (\x -> Just $ (fromMaybe (read val) $ M.lookup val regs) + (fromMaybe 0 x)) reg regs, freq) rest
eval code ip (regs,freq) (["mod",reg,val]:rest) = --traceShow regs $ traceShow "mod" $
    eval code (ip+1) (M.alter (\x -> Just $ mod (fromMaybe 0 x) ((fromMaybe (read val) $ M.lookup val regs)) ) reg regs, freq) rest
eval code ip (regs,freq) (["rcv",reg]    :rest) = --traceShow regs $ traceShow "rcv" $
    (ip+1, regs, freq, Recv reg)
eval code ip (regs,freq) (["snd",reg]    :rest) = --traceShow regs $ traceShow "snd" $
    (ip+1, regs, freq + 1, Send $ fromMaybe (read reg) $ M.lookup reg regs)
eval code ip (regs,freq) (["jgz",reg,val]:rest) = --traceShow regs $ traceShow "jmp" $
    if (fromMaybe (read reg) $ M.lookup reg regs) > 0 then
        let newip = ip + (fromMaybe (read val) $ M.lookup val regs)
        in eval code newip (regs,freq) (drop newip code)
    else eval code (ip+1) (regs, freq) rest

run input (prog1@(ip1,regs1,freq1,Recv _,[])) (prog2@(ip2,regs2,freq2,Recv _,[])) =
    (freq1,freq2) -- deadlock
run input (prog1@(ip1,regs1,freq1,Recv _,[])) prog2 = traceShow ("recv block") $
    run input prog2 prog1
run input (prog1@(ip1,regs1,freq1,Recv reg1,val1:q1)) prog2 = traceShow ("recv " ++ show val1 ++ " -> " ++ reg1) $
    let regs1' = M.insert reg1 val1 regs1
        (ip1',regs1'',freq1',state1') = eval input ip1 (regs1',freq1) (drop ip1 input)
    in run input (ip1',regs1'',freq1',state1',q1) prog2
run input (prog1@(ip1,regs1,freq1,Send val1,q1)) (prog2@(ip2,regs2,freq2,state2,q2)) = traceShow ("send " ++ show val1) $
    let (ip1',regs1',freq1',state1') = eval input ip1 (regs1,freq1) (drop ip1 input)
    in run input (ip1',regs1',freq1',state1',q1) (ip2,regs2,freq2,state2,q2 ++ [val1])
run input (prog1@(ip1,regs1,freq1,Done,q1)) (prog2@(ip2,regs2,freq2,Done,q2)) =
    (freq1,freq2) -- done
run input (prog1@(ip1,regs1,freq1,Done,q1)) (prog2@(ip2,regs2,freq2,state2,q2)) = traceShow ("done") $
    run input prog2 prog1

main = do input <- map words <$> lines <$> readFile "input.txt"
          -- print $ eval input 0 (M.empty,1337) input
          print $ run input (0,M.empty,0,Recv "p",[0]) (0,M.empty,0,Recv "p",[1])

