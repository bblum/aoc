import Data.Map (empty, (!), insert, findWithDefault)
import Data.List hiding (insert)
import Data.Word
import Data.Bits
import Data.Char

value x = findWithDefault (read x :: Word16) x

wire m [x,"RSHIFT",y,"->",z] = insert z $ shiftR (value x m) (read y)
wire m [x,"LSHIFT",y,"->",z] = insert z $ shiftL (value x m) (read y)
wire m [x,   "AND",y,"->",z] = insert z $ value x m .&. value y m
wire m [x,    "OR",y,"->",z] = insert z $ value x m .|. value y m
wire m [     "NOT",y,"->",z] = insert z $ complement $ value y m
wire m [           y,"->",z] = insert z $ value y m

result input = m where m = foldr (wire m) empty input

main = interact $ (++"\n") . show . (! "a") . result . map words . lines
