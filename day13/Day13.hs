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


iscart (c,_) = elem c "<>v^"
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

type Cart = ( (Char,(Int,Int)), Int) -- ((chara,coordinate),turning_memory)
movecarts  :: Int -> [String] -> [Cart] -> [Cart]
movecarts n input [] = []
movecarts n input (((cartchar,cartcoord),mem):rest) =
    let newcoord = getc cartchar cartcoord
        (newcartchar,mem') = newcart mem (index input newcoord) cartchar
        rest' = filter ((/= newcoord) . snd . fst) rest -- delete old thing if collided
        movedrest = movecarts n input rest' -- move remaining carts
        movedrest' = filter ((/= newcoord) . snd . fst) movedrest -- any new thing collided?
    in  if rest == rest' && movedrest == movedrest' -- collided into past or from future?
        then ((newcartchar, newcoord),mem'):movedrest' -- safe
        else traceShow (n,"collision at",newcoord,rest,rest',movedrest,movedrest') movedrest' -- delete this cart if collided


ticks input (n,carts) =
    let sortedcarts = sortBy (comparing (snd . fst)) carts
        result = movecarts n input sortedcarts
    in --traceShow (carts) $
       if any ((>1) . length) $ group $ sort $ map (snd . fst) result then error "fuck"
       else (n+1, result)

done (n,x) = length x == 1 -- || n == 200

main = do input <- lines <$> readFile "input.txt"
          -- part 1
          -- print $ findcrash $ until crash (tick input) (0, map (,0) $ carts input)
          -- part 2
          print $ until done (ticks input) (0, map (,0) $ carts input)

-- movecart :: [String] -> [Cart] -> (Int, Cart) -> Cart
-- movecart input oldcarts (i,((cartchar,cartcoord),mem)) =
--     let newcoord = getc cartchar cartcoord
--         (newcartchar,mem') = newcart mem (index input newcoord) cartchar
--         collide = elem newcoord $ map (snd . fst) $ drop i oldcarts
--     in  -- traceShow (cartchar, cartcoord, newcartchar, newcoord) $
--         if collide then error (show ("collision at",newcoord))
--         else ((newcartchar, newcoord),mem')
-- 
-- foo x y = snd (fst x) == snd (fst y)
-- -- crash (_,carts) = any ((>1) . length) $ groupBy (foo) $ sortBy (comparing (snd . fst)) carts
-- crash (n,carts) = nub coords /= coords where coords = map snd $ map fst carts
-- 
-- tick input (n,carts) =
--     let sortedcarts = sortBy (comparing (snd . fst)) carts
--     in traceShow (carts) $
--        (n+1,map (movecart input sortedcarts) $ zip [0..] sortedcarts)
-- 
-- findcrash (n,input) = (n, take 2 $ reverse $ sortBy (comparing length) $ groupBy foo $ sortBy (comparing (snd . fst)) input)
