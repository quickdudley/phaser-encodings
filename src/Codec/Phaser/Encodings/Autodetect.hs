module Codec.Phaser.Encodings.Autodetect (
  asciiSupersets
 ) where

import Data.Word
import Data.Bits
import Control.Applicative
import Control.Monad

import Codec.Phaser.Core
import Codec.Phaser.Common

-- | Begin decoding as ascii, then try all the encodings in the argument list
-- once a byte in the range 0x80 - 0xFF is encountered.
asciiSupersets :: Monoid p => [Phase p Word8 Char ()] -> Phase p Word8 Char ()
asciiSupersets l = go where
  fallback = foldr (<|>) empty l
  go = (<|> return ()) $ get >>= \b -> if b .&. 0x80 == 0
    then yield (toEnum $ fromIntegral b) >> go
    else put1 b >> fallback
