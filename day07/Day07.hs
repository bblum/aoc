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

free1 c s = all (\(_,c') -> c /= c') s
findstep1 s done = find (\c -> free1 c s) ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" \\ done)

makestep1 done s = traceShow s $
    let c = findstep1 s done
    in case c of
           Just c -> c:(makestep1 (c:done) $ filter (\(c',_) -> c' /= c) s)
           Nothing -> []


free c s = all (\(_,c') -> c /= c') s
notpending pending c = all (\(c',_) -> c /= c') pending
findstep s done pending =
    --let ans =
    find (\c -> free c s && notpending pending c) ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" \\ done)
    --in traceShow (done,s,pending,ans) ans

time c = 61 + fromEnum c - fromEnum 'A'
-- time c = 1 + fromEnum c - fromEnum 'A'

incr :: Int -> [(Char,Int)] -> ([Char],[(Char,Int)])
incr n [] = ([],[])
incr 0 x = ([],x)
incr n ((a,1):rest) =
    let (finished,result) = (incr (n-1) rest) in (a:finished,result)
incr n ((a,at):rest) =
    let (finished,result) = (incr (n-1) rest) in (finished,(a,at-1):result)


numworkers = 5
makestep timestamp done pending s = traceShow (timestamp,pending) $
    let c = if (length pending == numworkers) then Nothing else findstep s done pending
    in case c of
           Just c -> makestep (timestamp) (c:done) (sortBy (comparing fst) ((c,time c):pending)) s
           Nothing -> let (finished,newpending) = incr numworkers pending
                          news = filter (\(c',_) -> not $ elem c' finished) s
                      in finished ++ (if null pending then [] else makestep (timestamp+1) done (newpending) news)

main = print (makestep1 [] input, makestep 0 [] [] input)


testinput = [
    ('C','A'),
    ('C','F'),
    ('A','B'),
    ('A','D'),
    ('B','E'),
    ('D','E'),
    ('F','E')]

input = [
    ('E','Y'),
    ('Y','T'),
    ('I','C'),
    ('G','F'),
    ('C','P'),
    ('B','Q'),
    ('Z','N'),
    ('J','W'),
    ('W','P'),
    ('K','D'),
    ('Q','L'),
    ('V','D'),
    ('O','M'),
    ('A','P'),
    ('M','L'),
    ('R','S'),
    ('D','X'),
    ('X','N'),
    ('P','T'),
    ('F','N'),
    ('S','L'),
    ('U','N'),
    ('T','L'),
    ('N','H'),
    ('L','H'),
    ('N','L'),
    ('X','F'),
    ('P','F'),
    ('P','H'),
    ('B','D'),
    ('V','H'),
    ('X','S'),
    ('Q','O'),
    ('Z','T'),
    ('K','N'),
    ('S','H'),
    ('M','P'),
    ('Q','D'),
    ('R','U'),
    ('J','P'),
    ('P','S'),
    ('V','U'),
    ('R','T'),
    ('F','S'),
    ('D','T'),
    ('E','N'),
    ('J','N'),
    ('J','A'),
    ('K','U'),
    ('V','N'),
    ('V','S'),
    ('U','L'),
    ('F','U'),
    ('I','T'),
    ('J','L'),
    ('E','T'),
    ('T','N'),
    ('I','G'),
    ('R','D'),
    ('E','B'),
    ('X','H'),
    ('P','L'),
    ('Z','J'),
    ('O','L'),
    ('E','H'),
    ('F','T'),
    ('A','F'),
    ('U','H'),
    ('F','H'),
    ('C','W'),
    ('A','L'),
    ('V','M'),
    ('U','T'),
    ('E','P'),
    ('Y','U'),
    ('W','R'),
    ('E','X'),
    ('Q','U'),
    ('I','F'),
    ('V','F'),
    ('V','T'),
    ('R','P'),
    ('B','A'),
    ('S','T'),
    ('M','F'),
    ('Y','F'),
    ('C','K'),
    ('D','S'),
    ('O','S'),
    ('M','U'),
    ('Z','S'),
    ('R','H'),
    ('C','O'),
    ('G','Q'),
    ('Z','D'),
    ('B','N'),
    ('I','H'),
    ('I','P'),
    ('E','J'),
    ('V','L'),
    ('B','U')]
