import Data.List
import Data.List.Split
import Data.Char

unhexify c = maybe (digitToInt c) id $ lookup c $ zip "ABCDEF" [10..]

-- im sorry, im ntan lazy
bits  0 = [0,0,0,0]
bits  1 = [0,0,0,1]
bits  2 = [0,0,1,0]
bits  3 = [0,0,1,1]
bits  4 = [0,1,0,0]
bits  5 = [0,1,0,1]
bits  6 = [0,1,1,0]
bits  7 = [0,1,1,1]
bits  8 = [1,0,0,0]
bits  9 = [1,0,0,1]
bits 10 = [1,0,1,0]
bits 11 = [1,0,1,1]
bits 12 = [1,1,0,0]
bits 13 = [1,1,0,1]
bits 14 = [1,1,1,0]
bits 15 = [1,1,1,1]

unbinify n d = n * 2 + d

-- literal: version, value
-- op: version, opcode, subpackets
data Packet = Literal Int Int | Op Int Int [Packet] deriving Show

parse :: [Int] -> (Packet,Int,[Int])
parse (v1:v2:v3:1:0:0:input) = (Literal version value, packetlength, concat rest)
    where (chunks,rest) = asdf $ chunksOf 5 input
          asdf ((1:chunk):rest) = (chunk:chunks, rest2) where (chunks, rest2) = asdf rest
          asdf ((0:chunk):rest) = ([chunk], rest)
          packetlength = 6 + 5 * length chunks
          version = foldl unbinify 0 [v1,v2,v3]
          value = foldl unbinify 0 $ concat chunks
parse (v1:v2:v3:i1:i2:i3:0:input) = (Op version opcode packets, 21 + subpacketlength, rest4)
    where version = foldl unbinify 0 [v1,v2,v3]
          opcode = foldl unbinify 0 [i1,i2,i3]
          (len,rest) = splitAt 15 input
          subpacketlength = foldl unbinify 0 len
          (subpackets,rest4) = splitAt subpacketlength rest
          packets = recurse subpackets
          recurse [] = []
          recurse input = packet:recurse rest where (packet,_,rest) = parse input
                    
parse (v1:v2:v3:i1:i2:i3:1:input) = (Op version opcode packets, 18 + subpacketlength, rest4)
    where version = foldl unbinify 0 [v1,v2,v3]
          opcode = foldl unbinify 0 [i1,i2,i3]
          (count,rest) = splitAt 11 input
          subpacketcount = foldl unbinify 0 count
          recurse 0 rest = ([],0,rest)
          recurse n rest = (packet:packets, packetlen + packetslen, rest3)
              where (packet,packetlen,rest2) = parse rest
                    (packets,packetslen,rest3) = recurse (n-1) rest2
          (packets,subpacketlength,rest4) = recurse subpacketcount rest

versions (Literal v _) = v
versions (Op v o ps) = v + sum (map versions ps)

eval (Literal _ v) = v
eval (Op _ 0 ps) = sum $ map eval ps
eval (Op _ 1 ps) = product $ map eval ps
eval (Op _ 2 ps) = minimum $ map eval ps
eval (Op _ 3 ps) = maximum $ map eval ps
eval (Op _ 5 [p1,p2]) = if eval p1 > eval p2 then 1 else 0
eval (Op _ 6 [p1,p2]) = if eval p1 < eval p2 then 1 else 0
eval (Op _ 7 [p1,p2]) = if eval p1 == eval p2 then 1 else 0

main = do input <- concatMap bits <$> map unhexify <$> (\\ "\n") <$> readFile "input.txt"
          let (e,_,_) = parse input
          print $ versions e
          print $ eval e
