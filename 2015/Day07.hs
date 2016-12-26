{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Word
import Data.Bits
import Data.Either
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

process cmd inputs | any (isAlpha . head) inputs = Left (cmd, inputs)
process "VALUE"  [x]   = Right $ read x
process "RSHIFT" [x,y] = Right $ shiftR (read x) (read y)
process "LSHIFT" [x,y] = Right $ shiftL (read x) (read y)
process "AND"    [x,y] = Right $ read x .&. read y
process "OR"     [x,y] = Right $ read x .|. read y
process "NOT"    [x]   = Right $ complement $ read x

signal wire value (Left (cmd, inputs)) | any (== wire) inputs =
    process cmd $ map (\x -> if x == wire then show value else x) inputs
signal wire value logic = logic

simulate wire (Right value) = M.map $ signal wire value
simulate _ _ = id

solve = M.lookup "a" . until (isRight . fromJust . M.lookup "a") (join $ M.foldWithKey simulate)

parse [x,cmd,y,"->",z] = M.insert z $ Left $ (cmd, [x,y])
parse [  cmd,y,"->",z] = M.insert z $ Left $ (cmd, [y])
parse [      y,"->",z] = M.insert z $ if isAlpha $ head y then Left $ ("VALUE", [y]) else Right $ (read y :: Word16)

main = interact $ (++"\n") . show . solve . foldr parse M.empty . map words . lines
