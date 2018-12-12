import Data.List
import Data.Maybe

cell k p = fromJust $ lookup (map (fromMaybe '.' . flip lookup p) [k-2..k+2]) rules

generation p = [ (k,cell k p) | k <- [minimum (map fst p) - 2 .. maximum (map fst p) + 2] ]

score = sum . map fst . filter ((=='#') . snd)

trim = t . t . map snd where t = dropWhile (=='.') . reverse

glider ((_,p):(_,q):_) = trim p == trim q

solve (Just ((n,p):(_,q):_)) = score p + (50000000000 - n) * (score q - score p)

main = do print $ score $ iterate generation pots !! 20
          print $ solve $ find glider $ tails $ zip [0..] $ iterate generation pots

pots = zip [0..] "#..####.##..#.##.#..#.....##..#.###.#..###....##.##.#.#....#.##.####.#..##.###.#.......#............"

rules = [
    ("##...",'.'),
    ("##.##",'.'),
    (".#.#.",'#'),
    ("#..#.",'.'),
    ("#.###",'#'),
    (".###.",'.'),
    ("#.#..",'.'),
    ("##..#",'.'),
    (".....",'.'),
    ("...#.",'.'),
    (".#..#",'.'),
    ("####.",'#'),
    ("...##",'#'),
    ("..###",'#'),
    ("#.#.#",'#'),
    ("###.#",'#'),
    ("#...#",'#'),
    ("..#.#",'.'),
    (".##..",'#'),
    (".#...",'#'),
    (".##.#",'#'),
    (".####",'.'),
    (".#.##",'.'),
    ("..##.",'.'),
    ("##.#.",'.'),
    ("#.##.",'.'),
    ("#..##",'.'),
    ("###..",'.'),
    ("....#",'.'),
    ("#####",'#'),
    ("#....",'.'),
    ("..#..",'#')]
