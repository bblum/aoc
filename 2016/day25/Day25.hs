{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

register r = fromMaybe 0 <$> M.lookup r <$> get
argument (r@(c:_)) = if isAlpha c then register r else return $ read r

execute pc text [] = return []
execute pc text (["out",r]:rest) =
    do value <- argument r
       stream <- execute (pc+1) text rest
       return $ value:stream
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


try input value | signal /= cycle [0,1] = "it's not " ++ show value
    where signal = evalState (execute 0 input input) (M.singleton "a" value)

main = interact $ unlines . flip map [0..] . try . map words . lines
