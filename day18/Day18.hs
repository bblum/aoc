import qualified Data.Map as M
import Data.Maybe

data P = P { pid :: Int, code :: [[String]], ip :: Int, rs :: M.Map String Int, n :: Int }
data S = Done | Send Int | Recv String

val v p = fromMaybe (read v) $ M.lookup v $ rs p
op o r v p = M.alter (\x -> Just $ o (fromMaybe 0 x) (val v p)) r (rs p)

exec p ["rcv",r]   = (p { ip = ip p + 1 }, Recv r)
exec p ["snd",r]   = (p { ip = ip p + 1, n = n p + 1 }, Send $ val r p)
exec p ["set",r,v] = run (p { ip = ip p + 1, rs = M.insert r (val v p) (rs p) })
exec p ["mul",r,v] = run (p { ip = ip p + 1, rs = op (*) r v p })
exec p ["add",r,v] = run (p { ip = ip p + 1, rs = op (+) r v p })
exec p ["mod",r,v] = run (p { ip = ip p + 1, rs = op mod r v p })
exec p ["jgz",r,v] = run (p { ip = ip p + if val r p > 0 then val v p else 1 })

run p = if ip p >= length (code p) then (p, Done) else exec p (code p !! ip p)

part1 result (p, Send val) = part1 val $ run p
part1 result (p, Recv _) = result

part2 p0 p1 = n ([p0, p1] !! pid p1)

sched ((p0, Recv _), []) ((p1, Recv _), []) = part2 p0 p1
sched ((p0, Recv _), []) ((p1, Done), []) = part2 p0 p1
sched ((p0, Recv reg), []) t1 = sched t1 ((p0, Recv reg), [])
sched ((p0, Recv reg), val:q) t1 = sched (run (p0 { rs = M.insert reg val $ rs p0 }), q) t1
sched ((p0, Send val), q0) (r1, q1) = sched (run p0, q0) (r1, q1 ++ [val])
sched ((p0, Done), _) ((p1, Done), _) = part2 p0 p1
sched ((p0, Done), q) t1 = sched t1 ((p0, Done), q)

main = do input <- map words <$> lines <$> readFile "input.txt"
          print $ part1 0 (P 0 input 0 M.empty 0, Send 0)
          print $ sched ((P 0 input 0 M.empty 0, Recv "p"), [0])
                        ((P 1 input 0 M.empty 0, Recv "p"), [1])
