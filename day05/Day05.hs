{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad.State
import Control.Arrow
import Debug.Trace

intcode :: [Int] -> Int -> Int
intcode program index =
    if (program !! index) `mod` 100 == 1 then
        let a = if ((program !! index) `div` 100 )`mod` 10 == 0 then
                    program !! (program !! (index + 1))
                else
                    program !! (index + 1)
            b = if (program !! index) `div` 1000 `mod` 10 == 0 then
                    program !! (program !! (index + 2))
                else
                    program !! (index + 2)
            c = program !! (index + 3)
            progindex = zip [0..] program
            newprog = map (\(index,instruction) -> if index == c then a + b else instruction) progindex
        in traceShow ("add", program !! index, a, b, c) $ intcode newprog (index+4)
    else if (program !! index) `mod` 100 == 2 then
        let a = if (program !! index) `div` 100 `mod` 10 == 0 then
                    program !! (program !! (index + 1))
                else
                    program !! (index + 1)
            b = if (program !! index) `div` 1000 `mod` 10 == 0 then
                    program !! (program !! (index + 2))
                else
                    program !! (index + 2)
            c = program !! (index + 3)
            progindex = zip [0..] program
            newprog = map (\(index,instruction) -> if index == c then a * b else instruction) progindex
        in traceShow "multiply" $ intcode newprog (index+4)
    else if (program !! index) `mod` 100 == 3 then
        let a = program !! (index + 1)
            progindex = zip [0..] program
            input = 5
            newprog = map (\(index,instruction) -> if index == a then input else instruction) progindex
        in traceShow "input" $ intcode newprog (index+2)
    else if (program !! index) `mod` 100 == 4 then
        let a = if (program !! index) `div` 100 `mod` 10 == 0 then
                    program !! (program !! (index + 1))
                else
                    program !! (index + 1)
        in traceShow ("output", program !! index, a) $ intcode program (index+2)
    else if (program !! index) `mod` 100 == 5 then
        let a = if (program !! index) `div` 100 `mod` 10 == 0 then
                    program !! (program !! (index + 1))
                else
                    program !! (index + 1)
            b = if (program !! index) `div` 1000 `mod` 10 == 0 then
                    program !! (program !! (index + 2))
                else
                    program !! (index + 2)
            newindex = if a /= 0 then b else index + 3
        in traceShow "jtrue" $ intcode program newindex
    else if (program !! index) `mod` 100 == 6 then
        let a = if (program !! index) `div` 100 `mod` 10 == 0 then
                    program !! (program !! (index + 1))
                else
                    program !! (index + 1)
            b = if (program !! index) `div` 1000 `mod` 10 == 0 then
                    program !! (program !! (index + 2))
                else
                    program !! (index + 2)
            newindex = if a == 0 then b else index + 3
        in traceShow "jfalse" $ intcode program newindex
    else if (program !! index) `mod` 100 == 7 then
        let a = if (program !! index) `div` 100 `mod` 10 == 0 then
                    program !! (program !! (index + 1))
                else
                    program !! (index + 1)
            b = if (program !! index) `div` 1000 `mod` 10 == 0 then
                    program !! (program !! (index + 2))
                else
                    program !! (index + 2)
            c = program !! (index + 3)
            progindex = zip [0..] program
            newprog = map (\(index,instruction) -> if index == c then (if a < b then 1 else 0) else instruction) progindex
        in traceShow "lt" $ intcode newprog (index + 4)
    else if (program !! index) `mod` 100 == 8 then
        let a = if (program !! index) `div` 100 `mod` 10 == 0 then
                    program !! (program !! (index + 1))
                else
                    program !! (index + 1)
            b = if (program !! index) `div` 1000 `mod` 10 == 0 then
                    program !! (program !! (index + 2))
                else
                    program !! (index + 2)
            c = program !! (index + 3)
            progindex = zip [0..] program
            newprog = map (\(index,instruction) -> if index == c then (if a == b then 1 else 0) else instruction) progindex
        in traceShow "lt" $ intcode newprog (index + 4)
    else if program !! index == 99 then
        head program
        -- error program
    else error "invalid opcode"

part2 program (noun,verb) = (head program):noun:verb:(drop 3 program)

main = do -- input <- map read <$> readFile "input.txt"
          -- print $ filter (\(noun,verb) -> intcode (part2 input (noun,verb)) 0 == 19690720) [(noun,verb) | noun <- [0..99], verb <- [0..99] ]
          print $ intcode input 0


input = [3,225,1,225,6,6,1100,1,238,225,104,0,1101,90,64,225,1101,15,56,225,1,14,153,224,101,-147,224,224,4,224,1002,223,8,223,1001,224,3,224,1,224,223,223,2,162,188,224,101,-2014,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,1001,18,81,224,1001,224,-137,224,4,224,1002,223,8,223,1001,224,3,224,1,223,224,223,1102,16,16,224,101,-256,224,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,101,48,217,224,1001,224,-125,224,4,224,1002,223,8,223,1001,224,3,224,1,224,223,223,1002,158,22,224,1001,224,-1540,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,83,31,225,1101,56,70,225,1101,13,38,225,102,36,192,224,1001,224,-3312,224,4,224,1002,223,8,223,1001,224,4,224,1,224,223,223,1102,75,53,225,1101,14,92,225,1101,7,66,224,101,-73,224,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1101,77,60,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,226,677,224,1002,223,2,223,1005,224,329,1001,223,1,223,1007,226,677,224,1002,223,2,223,1005,224,344,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,359,101,1,223,223,7,226,226,224,102,2,223,223,1005,224,374,101,1,223,223,8,677,677,224,1002,223,2,223,1005,224,389,1001,223,1,223,107,677,677,224,102,2,223,223,1006,224,404,101,1,223,223,1107,677,226,224,102,2,223,223,1006,224,419,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,434,1001,223,1,223,7,677,226,224,102,2,223,223,1006,224,449,1001,223,1,223,1107,226,226,224,1002,223,2,223,1005,224,464,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,479,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,494,1001,223,1,223,1107,226,677,224,1002,223,2,223,1005,224,509,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,524,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,539,1001,223,1,223,1108,677,677,224,1002,223,2,223,1005,224,554,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,569,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,584,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,108,677,677,224,102,2,223,223,1006,224,614,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,629,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,644,101,1,223,223,107,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,674,1001,223,1,223,4,223,99,226]
