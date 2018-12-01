{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace
import Math.NumberTheory.Primes

data P = P { pid :: Int, code :: [[String]], ip :: Int, regs :: M.Map String Int, nsent :: Int } deriving Show

val v p = fromMaybe (read v) (M.lookup v $ regs p)
op o r v p = M.alter (\x -> Just $ o (fromMaybe 0 x) (val v p)) r (regs p)

-- exec p ["rcv",r]   = (p, Recv r)
-- exec p ["snd",r]   = (p { nsent = nsent p + 1 }, Send $ val r p)
-- exec p ["jgz",r,v] = run $ p { ip = ip p + if val r p > 0 then val v p - 1 else 0 }
exec p ["set",r,v] = run $ p { regs = M.insert r (val v p) (regs p) }
exec p ["mul",r,v] = run $ p { regs = op (*) r v p, nsent = nsent p + 1 } 
exec p ["add",r,v] = run $ p { regs = op (+) r v p } 
exec p ["sub",r,v] = run $ p { regs = op (-) r v p } 
exec p ["mod",r,v] = run $ p { regs = op mod r v p } 
exec p ["prime","b","h"] =
    let hdelta = if (isPrime $ toInteger $ val "b" p) then 1 else 0
        hval = val "h" p
    in run $ p { regs = M.insert "h" hval $ regs p }
exec p ["jnz",r,v] = run $ p { ip = ip p + if val r p /= 0 then val v p - 1 else 0 }

run p = if (ip p < 0 || ip p >= length (code p)) then p else exec (p { ip = ip p + 1 }) (code p !! ip p)

asdf = M.insert "a" 1 $ M.fromList $ map (,0) $ map (:[]) ['a'..'h']

main = do input <- map words <$> lines <$> readFile "input.txt"
          print $ regs $ run $ P 0 input 0 asdf 0
          print 5
