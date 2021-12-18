{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

register r = fromMaybe 0 <$> M.lookup r <$> get

execute pc text [] = return ()
execute pc text (["inc",r]:rest) =
    do modify $ M.adjust (+1) r
       execute (pc+1) text rest
execute pc text (["tpl",r]:rest) =
    do modify $ M.adjust (*3) r
       execute (pc+1) text rest
execute pc text (["hlf",r]:rest) =
    do modify $ M.adjust (flip div 2) r
       execute (pc+1) text rest
execute pc text (["jmp",n]:_) =
    execute (pc+read n) text $ drop (pc+read n) text
execute pc text (["jie",r,n]:rest) =
    do value <- register r
       if even value then execute pc text (["jmp",n]:rest)
       else execute (pc+1) text rest
execute pc text (["jio",r,n]:rest) =
    do value <- register r
       if value == 1 then execute pc text (["jmp",n]:rest)
       else execute (pc+1) text rest

solve input = M.lookup "b" $ execState (execute 0 input input) $ M.fromList [("a",1),("b",0)]

main = interact $ (++"\n") . show . solve . map words . lines
