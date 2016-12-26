{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Tuple
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

graph0 = [
    (("Faerun","Norrath"),129 ),
    (("Faerun","Tristram"),58),
    (("Faerun","AlphaCentauri"),13),
    (("Faerun","Arbre"),24),
    (("Faerun","Snowdin"),60),
    (("Faerun","Tambi"),71),
    (("Faerun","Straylight"),67),
    (("Norrath","Tristram"),142 ),
    (("Norrath","AlphaCentauri"),15),
    (("Norrath","Arbre"),135 ),
    (("Norrath","Snowdin"),75),
    (("Norrath","Tambi"),82),
    (("Norrath","Straylight"),54),
    (("Tristram","AlphaCentauri"),118 ),
    (("Tristram","Arbre"),122 ),
    (("Tristram","Snowdin"),103 ),
    (("Tristram","Tambi"),49),
    (("Tristram","Straylight"),97),
    (("AlphaCentauri","Arbre"),116 ),
    (("AlphaCentauri","Snowdin"),12),
    (("AlphaCentauri","Tambi"),18),
    (("AlphaCentauri","Straylight"),91),
    (("Arbre","Snowdin"),129 ),
    (("Arbre","Tambi"),53),
    (("Arbre","Straylight"),40),
    (("Snowdin","Tambi"),15),
    (("Snowdin","Straylight"),99),
    (("Tambi","Straylight"),70)]

graph = map (swap *** id) graph0 ++ graph0

cost path = sum $ map (fromJust . flip lookup graph) $ zip path $ tail path

main = print $ (minimum &&& maximum)  $ map cost $ permutations $ nub $ map (fst . fst) $ map (swap *** id) graph ++ graph
