{-# LANGUAGE MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Ord
import Control.Monad.State
import Control.Arrow

data Regex = Dir Char | Seq [Regex] | Or [Regex]

instance Show Regex where
    show (Dir c) = [c]
    show (Seq rs) = concatMap show rs
    show (Or rs) = "(" ++ (concat $ intersperse "|" $ map show rs) ++ ")"

parse r ('^':rest) = parse r rest -- start
parse r ('$':_) = (r,"$") -- end

parse (Seq stuff) ('(':rest) = parse (Seq $ stuff ++ [choices]) rest2
    where (choices,rest2) = parse (Or []) rest
parse (Seq stuff) (c:rest) | elem c "|)" = (Seq stuff, c:rest)
parse (Seq stuff) (c:rest) | elem c "NESW" = parse (Seq $ stuff ++ [Dir c]) rest

parse (Or choices) ('|':rest) = parse (Or $ choices ++ [choice]) rest2
    where (choice,rest2) = parse (Seq []) rest
parse (Or choices) (')':rest) = (Or choices,rest)
parse (Or choices) rest = parse (Or $ choices ++ [choice]) rest2
    where (choice,rest2) = parse (Seq []) rest

type Coord = (Int, Int)
type Search = (Coord, S.Set (Coord, Char))

otherway d   = fromJust $ lookup d $ zip "NESW" "SWNE"
move d (y,x) = fromJust $ lookup d $ zip "NESW" [(y-1,x), (y,x+1), (y+1,x), (y,x-1)]

search :: Regex -> State Search [Coord]
search (Seq []) = do (yx,_) <- get; return [yx] -- one possible location
search (Seq ((Dir d):rest)) =
    do (yx,_) <- get -- current position
       modify $ second $ S.insert (yx,d) . S.insert (move d yx, otherway d) -- add 2 doors
       modify $ first $ move d -- go the way
       search $ Seq rest -- keep going
search (Seq ((Or choices):rest)) =
    do (yx,_) <- get -- current position
       let try yx r = modify (first $ const yx) >> search r -- teleport to yx, then follow r
       poses <- nub <$> concat <$> mapM (try yx) choices -- nub prunes
       concat <$> mapM (flip try $ Seq rest) poses -- search for all possible results

prettyprint :: S.Set (Coord, Char) -> [String]
prettyprint doors =
    let xys = map fst $ S.elems doors
        miny = fst $ minimumBy (comparing fst) xys
        minx = snd $ minimumBy (comparing snd) xys
        maxy = fst $ maximumBy (comparing fst) xys
        maxx = snd $ maximumBy (comparing snd) xys
        vert  y x = if S.member ((y,x),'N') doors then '-' else '#'
        horiz y x = if S.member ((y,x),'W') doors then '|' else '#'
        row y = ["#" ++ (intersperse '#' $ map (vert y)  [minx..maxx]) ++ "#",
                        (intersperse '.' $ map (horiz y) [minx..maxx]) ++ ".#"]
    in concatMap row [miny..maxy] ++ [replicate (2*(maxx-minx+1)+1) '#']

path :: Int -> S.Set (Coord, Char) -> M.Map Coord Int -> [Coord] -> [Coord] -> M.Map Coord Int
path n room seen [] [] = seen
path n room seen todo_next [] = path (n+1) room seen [] todo_next
path n room seen todo_next (yx:todo_now) =
    let nbrs = mapMaybe (\d -> [ move d yx | S.member (yx,d) room ]) "NESW"
    in path n room (M.insert yx n seen) (todo_next ++ filter (flip M.notMember seen) nbrs) todo_now

main = do input <- readFile "input.txt"
          let (_,room) = flip execState ((0,0), S.empty) $ search $ fst $ parse (Seq []) input
          -- mapM putStrLn $ prettyprint room
          let dists = map snd $ M.toList $ path 0 room M.empty [] [(0,0)]
          print $ maximum dists
          print $ length $ filter (>=1000) dists
