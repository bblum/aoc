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

maxhp = 200
atk = 3

type Coord = (Int, Int) -- y,x so sort DTRT

type Score = Int
type Outcome = Int

type Noobs = M.Map Coord Int

data Board = B { spaces :: S.Set Coord, gobs :: Noobs, elfs :: Noobs, time :: Int } deriving (Ord, Eq)

instance Show Board where
    show b = intercalate "\n" $ map (flip map [0..maxx+1] . cell) [0..maxy+1]
        where maxx = maximum $ map snd $ S.elems $ spaces b
              maxy = maximum $ map fst $ S.elems $ spaces b
              cell y x = if M.member (y,x) $ gobs b then 'G' else
                         if M.member (y,x) $ elfs b then 'E' else
                         if S.member (y,x) $ spaces b then '.' else '#'

parseBoard :: [String] -> Board
parseBoard input = foldl row (B S.empty M.empty M.empty 0) $ zip [0..] input
    where row b (y,r) = foldl (cell y) b $ zip [0..] r
          cell y b (x,'#') = b
          cell y b (x,'.') = b { spaces = S.insert (y,x) $ spaces b }
          cell y b (x,'G') = (cell y b (x,'.')) { gobs = noob y x $ gobs b }
          cell y b (x,'E') = (cell y b (x,'.')) { elfs = noob y x $ elfs b }
          cell y b (x,c) = error $ "unexpected char " ++ show c 
          noob y x m = M.insert (y,x) maxhp m

type Race = Bool -- True => Elf
getRace False = gobs
getRace True  = elfs
updateRace False f b = b { gobs = f $ gobs b }
updateRace True  f b = b { elfs = f $ elfs b }

nbrs :: Coord -> [Coord]
nbrs (y,x) = [(y-1,x),(y,x-1),(y,x+1),(y+1,x)]

targetSquares :: (Coord, Race) -> State Board [Coord]
targetSquares = undefined

path :: Coord -> Coord -> State Board (Maybe Int) -- returns NEGATIVE total number of steps
path = undefined

turnMove :: Coord -> Race -> State Board ()
turnMove yx enemies =
    do targets <- targetSquares (yx, enemies)
       paths <- mapM (path yx) targets
       -- Nothing compares worse than any Just, so gets shortest possible path
       -- maximumBy breaks ties by preferring later elements, so reverse the read order
       let (target, path) = maximumBy (comparing snd) $ reverse $ zip targets paths
       -- XXX
       undefined
       return ()

turnAttack :: Race -> Coord -> State Board (Maybe Score)
turnAttack enemies noob =
    do modify $ updateRace enemies $ \m -> M.update attack noob m
       done <- M.null <$> getRace enemies <$> get
       if done then Just <$> score (not enemies) else return Nothing
    where attack hp = if hp <= atk then Nothing else Just $ hp - atk
          score race = sum <$> M.elems <$> getRace race <$> get

turn :: (Coord, Race) -> State Board (Maybe Score)
turn (yx, enemies) =
    do v <- victim
       when (isNothing v) $ turnMove yx enemies
       v' <- victim
       fromMaybe (return Nothing) $ turnAttack enemies <$> v' -- could golf better?
    where victim = listToMaybe <$> flip filter (nbrs yx) <$> flip M.member <$> getRace enemies <$> get

tick :: State Board (Maybe Outcome)
tick = do gs <- map (,True)  <$> M.keys <$> gobs <$> get
          es <- map (,False) <$> M.keys <$> elfs <$> get
          oldtime <- time <$> get
          modify $ \b -> b { time = time b + 1 }
          -- finds the first "Just" final-hp-result among turn results
          -- if any are Just it means combat should end there & return that total
          outcome <- join <$> find isJust <$> mapM turn (sortBy (comparing fst) $ gs ++ es)
          return $ (oldtime *) <$> outcome

simulate :: State Board Outcome
simulate = liftM2 (flip fromMaybe) tick simulate -- "while !done() tick();" :haskell_think:

main = do input <- parseBoard <$> lines <$> readFile "input.txt"
          print $ evalState simulate input
