{-# LANGUAGE FlexibleContexts, TupleSections #-}
import Data.List
import Data.Word
import Data.Bits
import Data.Maybe
import Data.Char

compute m name circuit = wire $ fromJust $ find ((== name) . last) circuit
    where wire [x,"RSHIFT",y,"->",z] = unop (flip shiftR $ read y) x
          wire [x,"LSHIFT",y,"->",z] = unop (flip shiftL $ read y) x
          wire [x,   "AND",y,"->",z] = binop (.&.) x y
          wire [x,    "OR",y,"->",z] = binop (.|.) x y
          wire [     "NOT",y,"->",z] = unop complement y
          wire [           y,"->",z] = unop id y
          binop op x y = let (xv, m') = value m x; (yv, m'') = value m' y in (op xv yv, m'')
          unop  op x   = let (xv, m') = value m x                         in (op xv, m')
          value m x = if isDigit $ head x then (read x :: Word16, m)
                      else fromMaybe (let (xv, m') = compute m x circuit in (xv, (x,xv):m'))
                                     ((,m) <$> snd <$> find ((== x) . fst) m)


main = interact $ (++"\n") . show . fst . compute [] "a" . map words . lines
