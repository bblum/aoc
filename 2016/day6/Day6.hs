import Data.List
import GHC.Exts

-- Part 1
main = interact $ map (head . head . reverse . sortWith length . group . sort) . transpose . lines
-- Part 2
-- main = interact $ map (head . head . sortWith length . group . sort) . transpose . lines
