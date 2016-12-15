import Data.List

main = print $ flip find [0..] $ \t -> all (\(d,(w,x)) -> (d+x+t)`mod`w==0) $ zip [1..] [(17,15),(3,2),(19,4),(13,2),(7,2),(5,0),(11,0)]
