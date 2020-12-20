import Data.List
import Data.List.Split
import Data.Maybe

parse (title:rest) = (read (last $ words title) :: Int, rest)

rm i tiles = filter ((/= i) . fst) tiles

-- part 1

corner :: [Tile] -> Tile -> Bool
corner input (i,tile) = length (filter (flip elem others) $ edges tile) == 4
    where others = concatMap edges $ map snd $ rm i input
          edges tile = es ++ map reverse es
              where es = map ($ tile) [head, last, head . transpose, last . transpose]

-- part 2

type Tile = (Int, [String])
type Square = [String]
type Row = [Square]
type Grid = [Row]

-- returns all 8 ways to rotate and or reflect a tile
flipify :: Square -> [Square]
flipify tile = map ($ tile) $ noobs ++ map (. transpose) noobs
    where noobs = [id, reverse, map reverse, reverse . map reverse]

-- finds the next tile that fits the specified edge and returns it already prerotated for ya
-- the next tile must match under the given `flip_fn`, e.g. for `id`, the top edge must match
puzzlify :: (Square -> Square) -> [Tile] -> String -> Maybe Tile
puzzlify flip_fn tiles edge = find match [(i,rt) | (i,t) <- tiles, rt <- flipify t]
    where match (_,tile) = edge == head (flip_fn tile)

-- builds a row rightwards from the available tile until it hits an edge
-- the starting tile of the row must already be specified so it should probs be a left edge
-- nb to turn the row into a serviceable part of the full picture, transpose it
mkrow row tiles = case puzzlify transpose tiles $ last $ transpose $ head row of
    Just (i,tile) -> mkrow (tile:row) $ rm i tiles
    Nothing -> (reverse row, tiles)

-- like mkrow but builds the whole dang picture, obvs
mkpicture :: Grid -> [Tile] -> Square
mkpicture grid [] = glue $ map transpose $ strip $ reverse grid -- out of tiles must be done
    where strip = map $ map $ map strip_line . strip_line
          strip_line = reverse . tail . reverse . tail
          glue = concatMap $ map concat
mkpicture grid tiles = mkpicture (newrow:grid) newtiles
    where Just (i,first_noob) = puzzlify id tiles $ last $ head $ head grid
          (newrow,newtiles) = mkrow [first_noob] $ rm i tiles

mosntex = ["                  # ",
           "#    ##    ##    ###",
           " #  #  #  #  #  #   "]

count_mosn picture = sum [ 1 | y <- [0..pix_y-mosn_y], x <- [0..pix_x-mosn_x], mosn_at y x]
    where dims p = (length p, length $ head p)
          (mosn_y,mosn_x) = dims mosntex
          (pix_y,pix_x) = dims picture
          mosn_at y x = match_mosn $ map (take mosn_x . drop x) $ take mosn_y $ drop y picture
          match_mosn cropped = all match_char $ zip (concat cropped) (concat mosntex)
          match_char (p,m) = p == '#' || m == ' '

solve picture = count picture - (count mosntex * maximum (map count_mosn $ flipify picture))
    where count = length . filter (== '#') . concat

main = do tiles <- map parse <$> splitOn [""] <$> lines <$> readFile "input.txt"
          let corners = filter (corner tiles) tiles
          print $ product $ map fst $ corners
          let (i,first_noob) = head corners
          -- trial and error determined `map reverse` is the right flip_op to start with
          -- cba to implement logic to figure that out all jidou-teki style
          let (first_row, newtiles) = mkrow [map reverse first_noob] $ rm i tiles
          print $ solve $ mkpicture [first_row] newtiles
