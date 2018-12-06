import Data.List
import Data.Ord

maxx = maximum $ map fst input
maxy = maximum $ map snd input
grid = [ (x,y) | x <- [0..maxx], y <- [0..maxy] ]

distance (x0,y0) (x1,y1) = abs (x0-x1) + abs (y1-y0)

closest p = if d == d2 then 0 else nobe
    where (nobe,d):(_,d2):_ = sortBy (comparing snd) $ map (fmap (distance p)) $ zip [0..] input

area nobe = length $ filter (== nobe) $ map closest grid

part1 = last $ sort $ map area $ [0..length input] \\ map closest [ (x,y) | (x,y) <- grid, elem x [0,maxx] || elem y [0,maxy] ]

region p = sum (map (distance p) input) < 10000

part2 = length $ filter region grid

main = print (part1, part2)

input = [
    (336, 308),
    (262, 98),
    (352, 115),
    (225, 205),
    (292, 185),
    (166, 271),
    (251, 67),
    (266, 274),
    (326, 85),
    (191, 256),
    (62, 171),
    (333, 123),
    (160, 131),
    (211, 214),
    (287, 333),
    (231, 288),
    (237, 183),
    (211, 272),
    (116, 153),
    (336, 70),
    (291, 117),
    (156, 105),
    (261, 119),
    (216, 171),
    (59, 343),
    (50, 180),
    (251, 268),
    (169, 258),
    (75, 136),
    (305, 102),
    (154, 327),
    (187, 297),
    (270, 225),
    (190, 185),
    (339, 264),
    (103, 301),
    (90, 92),
    (164, 144),
    (108, 140),
    (189, 211),
    (125, 157),
    (77, 226),
    (177, 168),
    (46, 188),
    (216, 244),
    (346, 348),
    (272, 90),
    (140, 176),
    (109, 324),
    (128, 132)]
