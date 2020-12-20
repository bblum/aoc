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

parse :: [String] -> (Int, [String])
parse (title:rest) = (read (last $ words title) :: Int, rest)

edges group = map (\edge -> [edge, reverse edge]) es
    where es = [head group, last group, head $ transpose group, last $ transpose group]

mapify m (i,group) = M.union m $ M.fromList $ map (\edge -> (edge, group)) $ concat $ edges group

iscorner input (i,group) = length (filter testedge $ edges group) == 2
    where testedge [edge,redge] = M.member edge m || M.member redge m
          m = foldl mapify M.empty $ filter ((/=i) . fst) input

-- all that is craperino

type Tile = (Int, [String])

validate tiles (i,group) = map (\edge -> length $ filter (anyedge edge) rest) thisedges
    where rest = filter ((/=i) . fst) tiles
          thisedges = map head $ edges group
          anyedge edge (_,other) = elem edge $ concat $ edges other

baleet i tiles = filter ((/= i) . fst) tiles

flip_ops :: [[String] -> [String]]
flip_ops = noobs ++ map (\f -> f . transpose) noobs
    where noobs = [id, reverse, map reverse, reverse . map reverse]

-- if is_vertical than were looking for edge to be == head $ transpose tile
find_puzzle :: [Tile] -> String -> Bool -> Maybe Tile
find_puzzle tiles edge is_vertical = case mapMaybe match candidates of x:_ -> Just x; [] -> Nothing
    where need_matching_edge tile = head $ if is_vertical then transpose tile else tile
          -- tile is already transposed by now, or not
          -- check left side of tile
          match (i,tile) = if edge == need_matching_edge tile then Just (i,tile) else Nothing
          candidates = concatMap (\(i,tile) -> map (i,) $ map ($ tile) flip_ops) tiles

-- nb to print the potutput row, transpose it
-- do this after grabbing the bottom tile
mkrow row tiles = ans $ find_puzzle tiles right_edge True
    where right_edge = last $ transpose $ head row -- still order top->bottom
          ans (Just (i, other)) = mkrow (other:row) $ baleet i tiles
          ans Nothing = (reverse row, tiles)

mkpicture :: [ [[String]] ] -> [Tile] -> [[[String]]]
mkpicture (row:prev_rows) [] = reverse (row:prev_rows) -- out of tiles must be done
mkpicture (row:prev_rows) tiles = mkpicture (newrow:row:prev_rows) newtiles
    where joiner :: String
          joiner = last $ head row
          Just next_first_noob = find_puzzle tiles joiner False -- must existerino
          (newrow, newtiles) = mkrow [snd next_first_noob] $ baleet (fst next_first_noob) tiles

pretty picture = map transpose picture

strip :: [[[String]]] -> [[[String]]]
strip picture = map striprow picture
    where striprow :: [[String]] -> [[String]]
          striprow row = map striptile row
          striptile :: [String] -> [String]
          striptile tile = map stripsides $ reverse $ tail $ reverse $ tail tile
          stripsides :: String -> String
          stripsides line = reverse $ tail $ reverse $ tail line
          
glue picture = concatMap gluerow picture
    where gluerow row = map glueline row
          glueline line = concat line

mosntex = ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]

count_mosntexs picture = sum [ 1 | y <- [0..maxy], x <- [0..maxx], mosntex_at y x]
    where maxy = length picture - length mosntex
          maxx = length (head picture) - length (head mosntex)
          mosntex_at y x = match_mosntex $ map (take (length $ head mosntex)) $ take (length mosntex) $ drop y $ map (drop x) picture
          match_mosntex cropped = all match_char $ zip (concat cropped) (concat mosntex)
          match_char ('#',_) = True
          match_char (_,' ') = True
          match_char ('.','#') = False

solve picture = num_water - (size_mosn * num_mosn)
    where num_water = length $ filter (== '#') $ concat picture
          size_mosn = length $ filter (== '#') $ concat mosntex
          num_mosn = maximum $ map count_mosntexs $ map ($ picture) flip_ops

main = do tiles <- map parse <$> splitOn [""] <$> lines <$> readFile "input.txt"
          -- print $ head tiles
          -- let m = foldl mapify M.empty tiles
          -- mapM print $ M.keys m
          let corners = filter (iscorner tiles) tiles
          mapM print $ map reverse $ snd $ head corners
          print $ product $ map fst $ corners
          --
          print $ map (validate tiles) tiles
          let first_noob = head corners
          let (first_row, newtiles) = mkrow [map reverse $ snd first_noob] $ baleet (fst first_noob) tiles
          let picture = glue $ pretty $ strip $ mkpicture [first_row] newtiles
          -- print $ strip $ [[ ["abcd","efgh","ijkl","mnop"] ]]
          print $ solve picture
          -- 
          testpicture <- lines <$> readFile "testpicture.txt"
          print $ solve testpicture
