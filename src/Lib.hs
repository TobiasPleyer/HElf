module Lib
(
  verifyElf
)
where

import Data.Array
import Foreign.C.Types

verifyElf :: Array Int CUChar -> Bool
verifyElf arr =
  (arr ! 0 == 0x7F) &&
  (arr ! 1 == 0x45) &&
  (arr ! 2 == 0x4c) &&
  (arr ! 3 == 0x46)
