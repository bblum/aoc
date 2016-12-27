import Data.Map (empty, (!), insert)
import Data.List hiding (insert)
import Data.Word
import Data.Bits
import Data.Char

value m x = if isDigit $ head x then read x :: Word16 else m ! x

wire m [x,"RSHIFT",y,"->",z] = insert z $ shiftR (value m x) (read y)
wire m [x,"LSHIFT",y,"->",z] = insert z $ shiftL (value m x) (read y)
wire m [x,   "AND",y,"->",z] = insert z $ value m x .&. value m y
wire m [x,    "OR",y,"->",z] = insert z $ value m x .|. value m y
wire m [     "NOT",y,"->",z] = insert z $ complement $ value m y
wire m [           y,"->",z] = insert z $ value m y

result input = foldr (wire $ result input) empty input

main = interact $ (++"\n") . show . (! "a") . result . map words . lines
