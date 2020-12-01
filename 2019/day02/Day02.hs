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
    if program !! index == 1 then
        let a = program !! (program !! (index + 1))
            b = program !! (program !! (index + 2))
            c = program !! (index + 3)
            progindex = zip [0..] program
            newprog = map (\(index,instruction) -> if index == c then a + b else instruction) progindex
        in intcode newprog (index+4)
    else if program !! index == 2 then
        let a = program !! (program !! (index + 1))
            b = program !! (program !! (index + 2))
            c = program !! (index + 3)
            progindex = zip [0..] program
            newprog = map (\(index,instruction) -> if index == c then a * b else instruction) progindex
        in intcode newprog (index+4)
    else if program !! index == 99 then
        head program
        -- error program
    else error "invalid opcode"

part2 program (noun,verb) = (head program):noun:verb:(drop 3 program)

main = do input <- map read <$> lines <$> readFile "input.txt"
          print $ filter (\(noun,verb) -> intcode (part2 input (noun,verb)) 0 == 19690720) [(noun,verb) | noun <- [0..99], verb <- [0..99] ]
          print $ intcode input 0
