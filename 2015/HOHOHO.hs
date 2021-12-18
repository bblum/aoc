import Data.List
import Data.Tuple
import Data.Maybe
import Data.Char
import Control.Arrow
import Debug.Trace

medicine = "ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF"

rules = [
    ("Al","ThF"),("Al","ThRnFAr"),("B","BCa"),("B","TiB"),("B","TiRnFAr"),
    ("Ca","CaCa"),("Ca","PB"),("Ca","PRnFAr"),("Ca","SiRnFYFAr"),("Ca","SiRnMgAr"),("Ca","SiTh"),
    ("F","CaF"),("F","PMg"),("F","SiAl"),
    ("H","CRnAlAr"),("H","CRnFYFYFAr"),("H","CRnFYMgAr"),("H","CRnMgYFAr"),
    ("H","HCa"),("H","NRnFYFAr"),("H","NRnMgAr"),("H","NTh"),("H","OB"),("H","ORnFAr"),
    ("Mg","BF"),("Mg","TiMg"),("N","CRnFAr"),("N","HSi"),
    ("O","CRnFYFAr"),("O","CRnMgAr"),("O","HP"),("O","NRnFAr"),("O","OTi"),
    ("P","CaP"),("P","PTi"),("P","SiRnFAr"),("Si","CaSi"),("Th","ThCa"),
    ("Ti","BP"),("Ti","TiTi"),("e","HF"),("e","NAl"),("e","OMg")]

rule input (src,dest) = map replace $ map fst $ filter snd $ zip [0..] $ map (isPrefixOf src) $ tails input
    where replace i = take i input ++ dest ++ drop (i + length src) input

longestrule m = filter (\((r,_),_) -> length r == (length $ fst $ fst $ head result)) result
    where result = reverse $ sortOn (\x -> (length $ fst $ fst x) - (length $ snd $ fst x)) $ filter (not . null . snd) $ map (\r -> (r, rule m r)) $ map swap rules

-- Then, I did it by hand using 'longestrule' in ghci.
