import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either

execute seen acc ip input = if S.member ip seen then Left acc else if ip >= length input then Right acc else ex $ input !! ip
    where newseen = S.insert ip seen
          op '-' = (-)
          op '+' = (+)
          ex ["nop", _] = execute newseen acc (ip+1) input
          ex ["acc", sign:val] = execute newseen (op sign acc $ read val) (ip+1) input
          ex ["jmp", sign:val] = execute newseen acc (op sign ip $ read val) input

uncorrupts input = [ uncorrupt i instr2 | (i,instr) <- input, instr2 <- switch instr ]
    where switch ["jmp", arg] = [["nop", arg]]
          switch ["nop", arg] = [["jmp", arg]]
          switch _ = []
          uncorrupt i instr2 = M.elems $ M.insert i instr2 $ M.fromList input

main = do input <- map words <$> lines <$> readFile "input.txt"
          print $ execute S.empty 0 0 input
          print $ rights $ map (execute S.empty 0 0) $ uncorrupts $ zip [0..] input
