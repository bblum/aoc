import Data.Map (empty, (!), insert, findWithDefault)
import Data.List hiding (insert)
import Data.Word
import Data.Bits
import Data.Char
import Control.Monad

value x = findWithDefault (read x :: Word16) x

wire m [x,"RSHIFT",y,"->",z] = shiftR (value x m) (read y)
wire m [x,"LSHIFT",y,"->",z] = shiftL (value x m) (read y)
wire m [x,   "AND",y,"->",z] = value x m .&. value y m
wire m [x,    "OR",y,"->",z] = value x m .|. value y m
wire m [     "NOT",y,"->",z] = complement $ value y m
wire m [           y,"->",z] = value y m

result input = m where m = foldr (liftM2 insert last $ wire m) empty input

main = interact $ (++"\n") . show . (! "a") . result . map words . lines
