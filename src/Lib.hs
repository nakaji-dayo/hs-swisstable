module Lib where

import           Data.WideWord.Word256
import           Data.Word
import           Foreign.C.Types

foreign import ccall "_elm_cmp_vec" cElmCmpVec :: Word8 -> Word256 -> Word


f = cElmCmpVec 1 1
