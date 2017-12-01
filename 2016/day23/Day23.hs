{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Map as M
import Data.Char
import Data.Maybe
import Control.Monad.State
import Control.Arrow

register r = fromMaybe 0 <$> M.lookup r <$> get
argument (r@(c:_)) = if isAlpha c then register r else return $ read r

execute pc text [] = return ()
execute pc text (["tgl",r]:_) =
    do offset <- argument r
       let toggle [cmd, arg1, arg2] = [if cmd == "jnz" then "cpy" else "jnz", arg1, arg2]
           toggle [cmd, arg1]       = [if cmd == "inc" then "dec" else "inc", arg1]
           newtext = map snd $ M.toList $ M.adjust toggle (pc+offset) $ M.fromList $ zip [0..] text
       execute (pc+1) newtext $ drop (pc+1) newtext
execute pc text (["cpy","b","c"]:["inc","a"]:["dec","c"]:["jnz","c","-2"]:["dec","d"]:["jnz","d","-5"]:rest) =
    do bval <- register "b"
       dval <- register "d"
       modify $ M.adjust (+(bval*dval)) "a"
       execute (pc+6) text rest
execute pc text (["cpy",src,dest]:rest) =
    do modify . M.insert dest =<< argument src
       execute (pc+1) text rest
execute pc text (["jnz",arg1,arg2]:rest) =
    do value <- argument arg1
       offset <- if value == 0 then return 1 else argument arg2
       execute (pc + offset) text $ drop (pc + offset) text
execute pc text ([cmd,r]:rest) =
    do modify $ M.adjust (if cmd == "inc" then (+1) else subtract 1) r
       execute (pc+1) text rest

solve state input = execState (execute 0 input input) state M.! "a"

main = interact $ show . (solve (M.singleton "a" 7) &&& solve (M.singleton "a" 12)) . map words . lines
