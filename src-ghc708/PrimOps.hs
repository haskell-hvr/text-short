{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE Unsafe           #-}

module PrimOps ( compareByteArrays# ) where

import           Foreign.C.Types  (CInt (..), CSize (..))
import           GHC.Exts         (Int (I#))
import           GHC.Exts         (ByteArray#, Int#)
import           System.IO.Unsafe (unsafeDupablePerformIO)

-- | Emulate GHC 8.4's 'GHC.Prim.compareByteArrays#'
compareByteArrays# :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
compareByteArrays# ba1# ofs1# ba2# ofs2# n#
    = unI (fromIntegral (unsafeDupablePerformIO (c_memcmp ba1# ofs1 ba2# ofs2 n)))
  where
    unI (I# i#) = i#
    ofs1 = fromIntegral (I# ofs1#)
    ofs2 = fromIntegral (I# ofs2#)
    n    = fromIntegral (I# n#)

foreign import ccall unsafe "hs_text_short_memcmp"
   c_memcmp :: ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt
