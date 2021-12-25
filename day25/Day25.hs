{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Either
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad.State
import Control.Arrow
import Debug.Trace

fnbrs8 (y,x) = [(y2,x2) | y2 <- [y-1..y+1], x2 <- [x-1..x+1], (y2,x2) /= (y,x)]
fnbrs4 (y,x) = [(y-1,x), (y+1,x), (y,x-1), (y,x+1)]

coordify = zipWith (\y -> zipWith ((,) . (y,)) [0..]) [0..]

type Coord = (Int,Int)
step :: Int -> Int -> Int -> M.Map Coord Bool -> Int
step maxx maxy n cukes = if nmoved == 0 then n else step maxx maxy (n+1) $  M.fromList newnoobs
    where (newnoobsh,nmovedh) = sum <$> (unzip $ map (steponeh cukes) $ M.toList cukes)
          (newnoobs,nmovedv) = sum <$> (unzip $ map (steponev $ M.fromList newnoobsh) newnoobsh)
          nmoved = nmovedh + nmovedv
          steponeh cukes ((y,x),True) = if M.member yx2 cukes then (((y,x),True),0) else ((yx2,True),1) where yx2 =(y,mod (x+1) maxx)
          steponeh cukes ((y,x),False) = (((y,x),False),0)
          steponev cukes ((y,x),False) = if M.member yx2 cukes then (((y,x),False),0) else ((yx2,False),1) where yx2 =(mod (y+1) maxy,x)
          steponev cukes ((y,x),True) = (((y,x),True),0)

parse (xy,'>') = Just (xy,True)
parse (xy,'v') = Just (xy,False)
parse _ = Nothing

main = do input <- lines <$> readFile "input.txt"
          let maxx = length $ head input
          let maxy = length input
          print $ step maxx maxy 1 $ M.fromList $ mapMaybe parse $ concat $ coordify input
