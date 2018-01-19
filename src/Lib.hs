module Lib
(
  verifyElf
, create_magic
)
where

import Data.Array
import Foreign.C.Types (CUChar)
import Data.List (intercalate)
import Numeric (showHex)

verifyElf :: Array Int CUChar -> Bool
verifyElf arr =
  (arr ! 0 == 0x7F) &&
  (arr ! 1 == 0x45) &&
  (arr ! 2 == 0x4c) &&
  (arr ! 3 == 0x46)

showHexLeadingZero :: (Integral a, Show a) => a -> String
showHexLeadingZero n = let s = (showHex n) "" in
  if n < 16
  then
    "0" ++ s
  else
    s

create_magic :: (Array Int CUChar) -> String
create_magic arr = intercalate " " (map showHexLeadingZero (elems arr))
