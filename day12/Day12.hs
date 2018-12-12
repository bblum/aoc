import qualified Data.Map as M

cell k m =
    let nbrs = map (get m) [k-2..k+2]
        get m k = case M.lookup k m of Just x -> x; Nothing -> '.'
    in case lookup nbrs rules of
           Just result -> result
           Nothing -> '.'

generation m = M.fromList [ (k,cell k m) | let mink = minimum $ M.keys m, let maxk = maximum $ M.keys m, k <- [mink-2..maxk+2] ]

foo (key,'.') = 0
foo (key,'#') = key
solve result = sum $ map foo result

trim str = dropWhile (=='.') $ reverse $ dropWhile (=='.') $ reverse str
main = do print $ solve $ M.toList $ iterate generation start !! 20
          print $ take 500 $ map (solve . M.toList)$ iterate generation start
          -- mapM (print . trim . map snd . M.toList) $ iterate generation start
          -- print $ solve $ M.toList $ iterate generation start2 !! 20

start = M.fromList $ zip [0..] "#..####.##..#.##.#..#.....##..#.###.#..###....##.##.#.#....#.##.####.#..##.###.#.......#............"

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


start2 = M.fromList $ zip [0..] "#..#.#..##......###...###"
rules2 = [
    ("...##",'#'),
    ("..#..",'#'),
    (".#...",'#'),
    (".#.#.",'#'),
    (".#.##",'#'),
    (".##..",'#'),
    (".####",'#'),
    ("#.#.#",'#'),
    ("#.###",'#'),
    ("##.#.",'#'),
    ("##.##",'#'),
    ("###..",'#'),
    ("###.#",'#'),
    ("####.",'#')]
