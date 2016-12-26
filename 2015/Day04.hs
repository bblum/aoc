{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Crypto.Hash.MD5
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base16 (encode)
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

gethash n = unpack $ encode $ hash $ pack $ "bgvyzdsv" ++ show n

main = print $ length $ takeWhile (not . isPrefixOf "000000") $ map gethash [0..]
