import Data.List
import Control.Arrow

main = print $ (length . (!!40) &&& length . (!!50)) $ iterate (concat . map (\l -> show (length l) ++ [head l]) . group) "1113122113"
