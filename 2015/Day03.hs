{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

move (x,y) '^' = (x,y-1)
move (x,y) '<' = (x-1,y)
move (x,y) '>' = (x+1,y)
move (x,y) 'v' = (x,y+1)
move p c = p
deliver (pos, visited) c = (move pos c, S.insert (move pos c) visited)

main = interact $ (++"\n") . show . S.size . uncurry S.union . join (***) (snd . foldl deliver ((0,0), S.singleton (0,0)) . concat) . unzip . map (take 1 &&& drop 1) . chunksOf 2
