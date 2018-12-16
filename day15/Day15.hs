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
    show b = intercalate "\n" $ map (flip concatMap [0..maxx+1] . cell) [0..maxy+1]
        where maxx = maximum $ map snd $ S.elems $ spaces b
              maxy = maximum $ map fst $ S.elems $ spaces b
              hp f yx = reverse $ take 3 $ (reverse $ show $ f b M.! yx) ++ repeat '0'
              cell y x = if M.member (y,x) $ gobs b then "G" ++ hp gobs (y,x) else
                         if M.member (y,x) $ elfs b then "E" ++ hp elfs (y,x) else
                         if S.member (y,x) $ spaces b then "...." else "####"

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
getEnemy r = getRace $ not r
updateEnemy r = updateRace $ not r

nbrs :: Coord -> [Coord]
nbrs (y,x) = [(y-1,x),(y,x-1),(y,x+1),(y+1,x)]

open :: Coord -> State Board Bool
open yx = do space <- S.member yx <$> spaces <$> get
             nogob <- M.notMember yx <$> gobs <$> get
             noelf <- M.notMember yx <$> elfs <$> get
             return $ space && nogob && noelf

targetSquares :: Coord -> Race -> State Board [Coord]
targetSquares yx enemies =
    do enemy_noobs <- M.keys <$> getRace enemies <$> get
       filterM open $ nub $ concatMap nbrs enemy_noobs

bfs :: Coord -> Int -> S.Set Coord -> [Coord] -> [Coord] -> State Board (Maybe Int)
bfs goal n seen [] [] = return Nothing
bfs goal n seen todo_later [] = bfs goal (n+1) seen [] todo_later
-- bfs goal n seen todo_later (yx:todo_now) | elem yx $ nbrs goal = return $ Just n
bfs goal n seen todo_later (yx:todo_now) | yx == goal = return $ Just n
bfs goal n seen todo_later (yx:todo_now) =
    do new_todos <- filterM open $ filter (flip S.notMember seen) $ nbrs yx
       let new_seen = S.union seen $ S.fromList new_todos
       bfs goal n new_seen (todo_later ++ new_todos) todo_now

-- coord is the first square you should go to in this path; int is the distance thereto
path :: Coord -> Coord -> State Board (Maybe (Coord, Int))
path yx goal =
    do steps <- filterM open $ nbrs yx -- floodfill 4 times, in reading order
       scores <- mapM (bfs goal 0 S.empty [] . (:[])) steps
       let steps_scores = catMaybes $ zipWith (\x y -> (x,) <$> y) steps scores
       if null steps_scores then
           return Nothing
       else
           return $ Just $ minimumBy (comparing snd) steps_scores

turnMove :: Coord -> Race -> State Board Coord
turnMove yx enemies =
    do targets <- targetSquares yx enemies
       paths <- catMaybes <$> mapM (path yx) targets
       if null paths then
           return yx
       else
           do let (newyx,_) = minimumBy (comparing snd) paths
              let move m = M.delete yx $ M.insert newyx (m M.! yx) m
              modify $ updateRace (not enemies) move
              return newyx

turnAttack :: Race -> Coord -> State Board (Maybe Score)
turnAttack enemies noob =
    do modify $ updateRace enemies $ \m -> M.update attack noob m
       done <- M.null <$> getRace enemies <$> get
       if done then Just <$> score (not enemies) else return Nothing
    where attack hp = if hp <= atk then Nothing else Just $ hp - atk
          score race = sum <$> M.elems <$> getRace race <$> get

turn :: (Coord, Race) -> State Board (Maybe Score)
turn (yx, enemies) =
    do v <- victim yx
       yx' <- if isNothing v then Just <$> turnMove yx enemies else return Nothing
       v' <- case yx' of Just newpos -> victim newpos; Nothing -> return v
       fromMaybe (return Nothing) $ turnAttack enemies <$> v' -- could golf better?
    where victim pos = listToMaybe <$> flip filter (nbrs pos) <$> flip M.member <$> getRace enemies <$> get

tick :: State Board (Maybe Outcome)
tick =
    do gs <- map (,True)  <$> M.keys <$> gobs <$> get
       es <- map (,False) <$> M.keys <$> elfs <$> get
       oldtime <- time <$> get
       modify $ \b -> b { time = time b + 1 }
       -- finds the first "Just" final-hp-result among turn results
       -- if any are Just it means combat should end there & return that total
       outcome <- join <$> find isJust <$> mapM turn (sortBy (comparing fst) $ gs ++ es)
       board <- get
       traceShow board $ return $ (oldtime *) <$> outcome

simulate :: State Board Outcome
simulate = liftM2 (flip fromMaybe) tick simulate -- "while !done() tick();" :haskell_think:

main = do input <- parseBoard <$> lines <$> readFile "input.txt"
          print $ evalState simulate input
