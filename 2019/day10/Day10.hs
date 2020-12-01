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

type Coord = (Int, (Int, Char))
type Point = (Int, Int)
type Slope = Point -- rise over run

minriserun (y,x) (y2,(x2,_)) = (minrise, minrun)
    where rise = y2 - y
          run  = x2 - x
          minrise = if rise == 0 then 0 else div rise $ gcd rise run
          minrun  = if run  == 0 then 0 else div run  $ gcd rise run

isasteroid coordsinput (y,x) = (snd $ snd $ (coordsinput !! y) !! x) == '#'

detect :: [[Coord]] -> Coord -> (Point, Int) -- snd is # detectable
detect coordsinput (y,(x,'.')) = ((y,x), 0)
detect coordsinput (y,(x,'#')) = ((y,x), sum $ map lineofsight $ concat coordsinput)
    where lineofsight (y2,(x2,'.')) = 0
          lineofsight (coord@(y2,(x2,'#'))) =
              let (minrise, minrun) = minriserun (y,x) coord
                  other_ys = tail [y, y + minrise .. y2 - minrise]
                  other_xs = tail [x, x + minrun  .. x2 - minrun]
                  other_coords = zip other_ys other_xs
              in if any (isasteroid coordsinput) other_coords then 0 else 1

part1 :: [[Coord]] -> (Point, Int)
part1 coordsinput = maximumBy (comparing snd) $ map (detect coordsinput) $ concat coordsinput

-- a type to allow sorting all existing pairwise slopes in the order that the laser spins.
data SortableSlope = U | R (Double, Int, Int) | D | L (Double, Int, Int) deriving (Eq, Ord, Show)

toSortableSlope :: Slope -> SortableSlope
toSortableSlope (0,0) = error "sakdjfhs????"
toSortableSlope (rise,0) | rise < 0 = U
toSortableSlope (rise,0)            = D
toSortableSlope (rise,run) | gcd rise run /= 1 = error "please reduce first"
toSortableSlope (rise,run) =
    (if run > 0 then R else L) (fromIntegral rise / fromIntegral run, rise, run)

fromSortableSlope U = (-1,0)
fromSortableSlope D = (1,0)
fromSortableSlope (R (_, rise, run)) = (rise, run)
fromSortableSlope (L (_, rise, run)) = (rise, run)

part2 (y,x) coordsinput =
    let risesruns = map (minriserun (y,x)) $ concat coordsinput
        risesruns_noself = risesruns \\ [(0,0)]
        slopes = map fromSortableSlope $ sort $ map toSortableSlope $ nub risesruns_noself
        -- returns edited board, and location of asteroid zapped if any
        zap :: ([[Coord]], Maybe Point) -> Slope -> ([[Coord]], Maybe Point)
        zap (board, _) (rise,run) =
            let -- find all coordinance to consider zapping
                candidate_ys = tail [y, y + rise ..]
                candidate_xs = tail [x, x + run  ..]
                candidates = takeWhile inbounds $ zip candidate_ys candidate_xs
                inbounds (y,x) = elem y [0..maxy] && elem x [0..maxx]
                    where maxy = length coordsinput - 1
                          maxx = length (head coordsinput) - 1
                target = find (isasteroid coordsinput) candidates
                newboard = map (map (\(y1,(x1,thing)) -> if Just (y1,x1) == target then (y1,(x1,'.')) else (y1,(x1,thing)))) board
            in (newboard, target)
        zapped = catMaybes $ map snd $ scanl zap (coordsinput, Nothing) $ cycle slopes
    in zapped !! 199

main = do input <- lines <$> readFile "input.txt"
          let coordsinput = map mkrow $ zip [0..] $ map (zip [0..]) input
                  where mkrow (y,row) = map (y,) row
          let ((y,x),ans) = part1 coordsinput
          print ans
          let (y2,x2) = part2 (y,x) coordsinput
          print $ x2 * 100 + y2
