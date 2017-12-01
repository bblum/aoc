{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Map as M
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad.State
import Control.Arrow
import Debug.Trace

execute pc text [] = return ()
execute pc text (["inc",reg1]:["dec",reg2]:["jnz",reg3,"-2"]:rest) | reg2 == reg3 =
    do value <- fromMaybe 0 <$> M.lookup reg2 <$> get
       modify $ M.adjust (+value) reg1 . M.insert reg2 0
       execute (pc+3) text rest
execute pc text (["cpy",src@(d:_),dest]:rest) =
    do if isDigit d then modify $ M.insert dest (read src)
       else (modify . M.insert dest) =<< fromMaybe 0 <$> M.lookup src <$> get
       execute (pc+1) text rest
execute pc text (["jnz",reg@(d:_),offset]:rest) =
    do value <- if (isDigit d) then return $ read reg else fromMaybe 0 <$> M.lookup reg <$> get
       if (value /= 0) then execute (pc + read offset) text $ drop (pc + read offset) text
       else execute (pc+1) text rest
execute pc text ([incdec,reg]:rest) =
    do modify $ M.adjust (if incdec == "inc" then (+1) else subtract 1) reg
       execute (pc+1) text rest

solve state input = fromJust $ M.lookup "a" $ execState (execute 0 input input) state

main = interact $ (++"\n") . show . (solve M.empty *** solve (M.singleton "c" 1)) . join (,) . map words . lines
