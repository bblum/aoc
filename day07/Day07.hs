import qualified Data.Map as M
import Data.Either

parse cwd dirmap [] = dirmap
parse (_:cwd) dirmap (["$","cd",".."]:rest) = parse cwd dirmap rest
parse cwd dirmap (["$","cd",dirname]:rest) = parse (dirname:cwd) dirmap rest
parse cwd dirmap (["$","ls"]:rest) = parse cwd (M.insert cwd (Left children) dirmap3) rest2
    where (output,rest2) = break ((== "$") . head) rest
          (dirmap3,children) = foldl parsechild (dirmap,[]) output
          parsechild (dirmap,children) ["dir",dirname] = (dirmap,(dirname:cwd):children)
          parsechild (dirmap,children) [size,filename] = (dirmap2,(filename:cwd):children)
              where dirmap2 = M.insert (filename:cwd) (Right $ read size) dirmap

size dirmap cwd = either (sum . map (size dirmap)) id $ dirmap M.! cwd

main = do (["$","cd","/"]:input) <- map words <$> lines <$> readFile "input.txt"
          let dirmap = parse [] M.empty input
          let dirsizes = map (size dirmap) $ M.keys $ M.filter isLeft dirmap
          print $ sum $ filter (<= 100000) dirsizes
          print $ minimum $ filter (>= maximum dirsizes - 40000000) dirsizes
