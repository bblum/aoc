import qualified Data.Set as S
import Data.Maybe

template = "##....###.#.##...##....####..###.#.######.#.#.##.#.####.#.#####.##.##..##.###.###..##.##..#####.##..#..##..#...#.####..#.###..#....####.#..##.##...#######.###...#.######..#..#...###..###.#####.##..#.#.###.#.###.#..#.###.###.#..##.....####..#.##.##.#..#...###.#.....##..#....#.##..#....#....####.#...#.#.##.#...#.##..#..#.#..###.###.#.##...##.#.##.##..#..##.#..#...######.#.#..###....##...##....#....##....#.#..##..#####.####.#.##...#...#.#.#....#.####...#.##..#...#..#....#..#..#..##.#.#.#.#######.###..##.#....."

parse n False = 2*n
parse n True = 2*n+1

setify = S.fromList . catMaybes . concat . zipWith mkrow [0..]
    where mkrow y = zipWith mkcol [0..]
              where mkcol x '.' = Nothing
                    mkcol x '#' = Just (y,x)

step b s = S.fromList $ filter cell $ map (,) [miny-4..maxy+4] <*> [minx-4..maxx+4]
    where xys = S.elems s
          [miny,minx,maxy,maxx] = [(minimum $), (maximum $)] <*> [map fst xys, map snd xys]
          cell (y,x) = (template !! foldl parse 0 key == '#') && retain b (y,x)
              where key = map (flip S.member s) $ map (,) [y-1..y+1] <*> [x-1..x+1]
          -- i hate this
          retain 0 _ = True
          retain 1 (y,x) = elem x [minx..maxx-1] && elem y [miny..maxy-1]

part n = S.size . (!! n) . iterate (step 1 . step 0) . setify

main = do input <- lines <$> readFile "input.txt"
          print $ part 1 input
          print $ part 25 input
