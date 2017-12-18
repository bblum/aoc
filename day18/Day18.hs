import qualified Data.Map as M
import Data.Maybe

data P = P { pid :: Int, code :: [[String]], ip :: Int, regs :: M.Map String Int, nsent :: Int }
data S = Send Int | Recv String

val v p = fromMaybe (read v) (M.lookup v $ regs p)
op o r v p = M.alter (\x -> Just $ o (fromMaybe 0 x) (val v p)) r (regs p)

exec p ["rcv",r]   = (p, Recv r)
exec p ["snd",r]   = (p { nsent = nsent p + 1 }, Send $ val r p)
exec p ["set",r,v] = run $ p { regs = M.insert r (val v p) (regs p) }
exec p ["mul",r,v] = run $ p { regs = op (*) r v p }
exec p ["add",r,v] = run $ p { regs = op (+) r v p }
exec p ["mod",r,v] = run $ p { regs = op mod r v p }
exec p ["jgz",r,v] = run $ p { ip = ip p + if val r p > 0 then val v p - 1 else 0 }

run p = exec (p { ip = ip p + 1 }) (code p !! ip p)

part1 result (p, Send v) = part1 v $ run p
part1 result (p, Recv _) = result

sched ((p0, Recv _), []) ((p1, Recv _), []) = nsent $ [p0, p1] !! pid p1
sched ((p0, Recv r), []) t1 = sched t1 ((p0, Recv r), [])
sched ((p0, Recv r), v:q) t1 = sched (run (p0 { regs = M.insert r v $ regs p0 }), q) t1
sched ((p0, Send v), q0) (r1, q1) = sched (run p0, q0) (r1, q1 ++ [v])

main = do input <- map words <$> lines <$> readFile "input.txt"
          print $ part1 0 (P 0 input 0 M.empty 0, Send 0)
          print $ sched ((P 0 input 0 M.empty 0, Recv "p"), [0])
                        ((P 1 input 0 M.empty 0, Recv "p"), [1])
