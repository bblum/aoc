import qualified Data.Map as M
import Data.Bits

maskify (power,'1') val = val .|. (2^power)
maskify (power,'0') val = val .&. complement (2^power)
maskify (power,'X') val = val

part1 mask i val = M.insert i $ foldr maskify val mask

maskify2 (power,'1') is = map (maskify (power,'1')) is
maskify2 (power,'0') is = is -- this rule bamboozled me
maskify2 (power,'X') is = concatMap (\i -> map (\b -> maskify (power,b) i) "01") is

part2 mask i val mem = foldr (flip M.insert val) mem $ foldr maskify2 [i] mask

exec f (mask,mem) ["mask",_,mask2] = (mask2, mem)
exec f (mask,mem) ["mem",i,_,val] = (mask, f (zip [0..] $ reverse mask) (read i :: Int) (read val :: Int) mem)

solve f = sum . M.elems . snd . foldl (exec f) (undefined, M.empty)

main = do input <- map words <$> lines <$> readFile "input.txt"
          print $ solve part1 input
          print $ solve part2 input
