{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Char
import Data.Ord
import Control.Monad.State
import Control.Arrow
import Debug.Trace


iscart (c,x) = elem c "<>v^"
cartrow y row = map ((y,) <$>) $ filter iscart $ zip row [0..] 
carts input = concat $ zipWith cartrow [0..] input


getc 'v' (y,x) = (y+1,x)
getc '<' (y,x) = (y,x-1)
getc '>' (y,x) = (y,x+1)
getc '^' (y,x) = (y-1,x)

turnleft '<' = 'v'
turnleft 'v' = '>'
turnleft '>' = '^'
turnleft '^' = '<'
newcart m '/' '<'  = ('v',m)
newcart m '/' '^'  = ('>',m)
newcart m '/' '>'  = ('^',m)
newcart m '/' 'v'  = ('<',m)
newcart m '\\' '<' = ('^',m)
newcart m '\\' '^' = ('<',m)
newcart m '\\' '>' = ('v',m)
newcart m '\\' 'v' = ('>',m)
newcart 0 '+' c = (turnleft c, 1)
newcart 1 '+' c = (c, 2)
newcart 2 '+' c = (turnleft $ turnleft $ turnleft c, 0)
newcart m ' ' _ = error "fuck, went of ftrack"
newcart m '|' '<'  = error "umm, excuse me?"
newcart m '-' '^'  = error "umm, excuse me?"
newcart m '|' '>'  = error "umm, excuse me?"
newcart m '-' 'v'  = error "umm, excuse me?"
newcart m _ x = (x,m)

index input coord = (input !! (fst coord)) !! (snd coord)

movecart input ((cartchar,cartcoord),mem) =
    let newcoord = getc cartchar cartcoord
        (newcartchar,mem') = newcart mem (index input newcoord) cartchar
    in  -- traceShow (cartchar, cartcoord, newcartchar, newcoord) $
        ((newcartchar, newcoord),mem')

foo x y = snd (fst x) == snd (fst y)
-- crash (_,carts) = any ((>1) . length) $ groupBy (foo) $ sortBy (comparing (snd . fst)) carts
crash (i,(n,carts)) = nub coords /= coords where coords = map snd $ map fst carts

fuckedcart (((_,(x1,y1)),_),((_,(x2,y2)),_)) = (mod (x1+y1) 2) == (mod (x2+y2) 2)

fucked (n,ans) carts = any fuckedcart $ zip ans carts

moveonecart i input (j,cart) = if i == j then movecart input cart else cart

tick input (i,(n,carts)) = traceShow (carts) $
    (mod (i+1) (length carts), (n+1,map (moveonecart i input) $ zip [0..] $ sortBy (comparing (snd . fst)) carts))

findcrash (n,input) = (n, take 2 $ reverse $ sortBy (comparing length) $ groupBy foo $ sortBy (comparing (snd . fst)) input)
main = do input <- lines <$> readFile "asdf.txt"
          -- print $ carts input
          print $ findcrash $ snd $ until crash (tick input) (0,(0, map (,0) $ carts input))

