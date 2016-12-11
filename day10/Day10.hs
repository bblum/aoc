{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Either
import Control.Monad.State
import Control.Arrow

newtype Bot = Bot (Int -> State (M.Map (String,Int) (Either Bot Int)) ())

send (("output",n), value) = modify $ M.insert ("part2",n) $ Right value
send (target, value) = M.lookup target <$> get >>= \(Just (Left (Bot b))) -> b value

-- A bot is a lambda which, receiving an int, executes in the state monad. It either
-- replaces itself to advance to stage 2, or if already in stage 2, sends its values.
bot n targets v1 = modify $ M.insert ("bot",n) $ Left $ Bot bot2
    where bot2 v2 = do when (sort [v1,v2] == [17,61]) $ modify $ M.insert ("part1",0) $ Right n
                       mapM_ send $ zip targets $ sort [v1,v2]

parse ["bot",n0,_,_,_,w1,n1,_,_,_,w2,n2] =
    modify $ M.insert ("bot",read n0) $ Left $ Bot $ bot (read n0) [(w1,read n1),(w2,read n2)]
parse ["value",v,_,_,_,n] = send (("bot",read n), read v)

-- I sort the commands before parsing to initialize all bots before sending any values.
main = interact $ show . (head &&& product . take 3 . tail) . rights . M.elems
                  . flip execState M.empty . mapM parse . sort . map words . lines
