{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UnliftedFFITypes           #-}
{-# LANGUAGE Unsafe                     #-}

-- |
-- Module      : Data.Text.Short.Internal
-- Copyright   : © Herbert Valerio Riedel 2017
-- License     : BSD3
--
-- Maintainer  : hvr@gnu.org
-- Stability   : stable
--
-- Memory-efficient representation of Unicode text strings.
module Data.Text.Short.Internal
    ( -- * The 'ShortText' type
      ShortText(..)

      -- * Basic operations
    , Data.Text.Short.Internal.null
    , Data.Text.Short.Internal.length
    , Data.Text.Short.Internal.isAscii
    , Data.Text.Short.Internal.splitAt

      -- * Conversions
      -- ** 'String'
    , Data.Text.Short.Internal.fromString
    , toString

      -- ** 'T.Text'
    , fromText
    , toText

      -- ** 'BS.ByteString'
    , fromShortByteString
    , fromShortByteStringUnsafe
    , toShortByteString

    , fromByteString
    , fromByteStringUnsafe
    , toByteString

    , toBuilder

    ) where

import           Control.DeepSeq                (NFData)
-- import           Control.Exception as E
import           Data.Binary
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Builder        as BB
import           Data.ByteString.Short          (ShortByteString)
import qualified Data.ByteString.Short          as BSS
import qualified Data.ByteString.Short.Internal as BSSI
import           Data.Char
import           Data.Hashable                  (Hashable)
import           Data.Semigroup
import qualified Data.String                    as S
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Foreign.C
import           GHC.Exts                       (ByteArray#, Int (I#),
                                                 MutableByteArray#,
                                                 copyByteArray#, newByteArray#,
                                                 unsafeFreezeByteArray#)
import qualified GHC.Foreign                    as GHC
import           GHC.IO.Encoding
import           GHC.ST
import           System.IO.Unsafe

-- | A compact representation of Unicode strings.
--
-- This type relates to 'T.Text' as 'ShortByteString' relates to 'BS.ByteString' by providing a more compact type. Please consult the documentation of "Data.ByteString.Short" for more information.
--
-- Currently, a boxed unshared 'T.Text' has a memory footprint of 6 words (i.e. 48 bytes on 64-bit systems) plus 2 or 4 bytes per code-point (due to the internal UTF-16 representation). Each 'T.Text' value which can share its payload with another 'T.Text' requires only 4 words additionally. Unlike 'BS.ByteString', 'T.Text' use unpinned memory.
--
-- In comparison, the footprint of a boxed 'ShortText' is only 4 words (i.e. 32 bytes on 64-bit systems) plus 1/2/3/4 bytes per code-point (due to the internal UTF-8 representation).
-- It can be shown that for realistic data <http://utf8everywhere.org/#asian UTF-16 has a space overhead of 50% over UTF-8>.
--
newtype ShortText = ShortText ShortByteString
                  deriving (Eq,Ord,Monoid,Semigroup,Hashable,NFData)

instance Show ShortText where
    showsPrec p (ShortText b) = showsPrec p (decodeStringShort' utf8 b)
    show (ShortText b)        = show        (decodeStringShort' utf8 b)

instance Read ShortText where
    readsPrec p = map (\(x,s) -> (ShortText $ encodeStringShort utf8 x,s)) . readsPrec p

-- | Behaviour for @[U+D800 .. U+DFFF]@ matches the 'IsString' instance for 'T.Text'
instance S.IsString ShortText where
    fromString = fromString

-- | The 'Binary' encoding matches the one for 'T.Text'
#if MIN_VERSION_binary(0,8,1)
instance Binary ShortText where
    put = put . toShortByteString
    get = do
        sbs <- get
        case fromShortByteString sbs of
          Nothing -> fail "Binary.get(ShortText): Invalid UTF-8 stream"
          Just st -> return st
#else
-- fallback via 'ByteString' instance
instance Binary ShortText where
    put = put . toByteString
    get = do
        bs <- get
        case fromByteString bs of
          Nothing -> fail "Binary.get(ShortText): Invalid UTF-8 stream"
          Just st -> return st
#endif

-- | \(\mathcal{O}(1)\) Test whether a 'ShortText' is empty.
null :: ShortText -> Bool
null = BSS.null . toShortByteString

-- | \(\mathcal{O}(n)\) Count the number of Unicode code-points in a 'ShortText'.
length :: ShortText -> Int
length st = fromIntegral $ unsafePerformIO (c_text_short_length (toByteArray# st) (toCSize st))

foreign import ccall unsafe "hs_text_short_length" c_text_short_length :: ByteArray# -> CSize -> IO CSize

-- | \(\mathcal{O}(n)\) Test whether 'ShortText' contains only ASCII code-points (i.e. only U+0000 through U+007F).
isAscii :: ShortText -> Bool
isAscii st = (/= 0) $ unsafePerformIO (c_text_short_is_ascii (toByteArray# st) sz)
  where
    sz = toCSize st

foreign import ccall unsafe "hs_text_short_is_ascii" c_text_short_is_ascii :: ByteArray# -> CSize -> IO CInt

----------------------------------------------------------------------------

toCSize :: ShortText -> CSize
toCSize = fromIntegral . BSS.length . toShortByteString

toByteArray# :: ShortText -> ByteArray#
toByteArray# (ShortText (BSSI.SBS ba#)) = ba#

-- | \(\mathcal{O}(0)\) Converts to UTF-8 encoded 'ShortByteString'
--
-- This operation has effectively no overhead, as it's currently merely a @newtype@-cast.
toShortByteString :: ShortText -> ShortByteString
toShortByteString (ShortText b) = b

-- | \(\mathcal{O}(n)\) Converts to UTF-8 encoded 'BS.ByteString'
toByteString :: ShortText -> BS.ByteString
toByteString = BSS.fromShort . toShortByteString

-- | Construct a 'BB.Builder' that encodes 'ShortText' as UTF-8.
toBuilder :: ShortText -> BB.Builder
toBuilder = BB.shortByteString . toShortByteString

-- | \(\mathcal{O}(n)\) Convert to 'String'
toString :: ShortText -> String
toString = decodeStringShort' utf8 . toShortByteString

-- | \(\mathcal{O}(n)\) Convert to 'T.Text'
--
-- This is currently not \(\mathcal{O}(1)\) because currently 'T.Text' uses UTF-16 as its internal representation.
-- In the event that 'T.Text' will change its internal representation to UTF-8 this operation will become \(\mathcal{O}(1)\).
toText :: ShortText -> T.Text
toText = T.decodeUtf8 . toByteString

----

-- | \(\mathcal{O}(n)\) Construct/pack from 'String'
--
-- Note: This function is total because it replaces the (invalid) code-points U+D800 through U+DFFF with the replacement character U+FFFD.
fromString :: String -> ShortText
fromString = ShortText . encodeStringShort utf8 . map r
  where
    r c | 0xd800 <= x && x < 0xe000 = '\xFFFD'
        | otherwise                 = c
      where
        x = ord c

-- | \(\mathcal{O}(n)\) Construct 'ShortText' from 'T.Text'
--
-- This is currently not \(\mathcal{O}(1)\) because currently 'T.Text' uses UTF-16 as its internal representation.
-- In the event that 'T.Text' will change its internal representation to UTF-8 this operation will become \(\mathcal{O}(1)\).
fromText :: T.Text -> ShortText
fromText = fromByteStringUnsafe . T.encodeUtf8

-- | \(\mathcal{O}(n)\) Construct 'ShortText' from UTF-8 encoded 'ShortByteString'
--
-- This operation doesn't copy the input 'ShortByteString' but it
-- cannot be \(\mathcal{O}(1)\) because we need to validate the UTF-8 encoding.
--
-- Returns 'Nothing' in case of invalid UTF-8 encoding.
fromShortByteString :: ShortByteString -> Maybe ShortText
fromShortByteString sbs
  | isValidUtf8 st  = Just st
  | otherwise       = Nothing
  where
    st = ShortText sbs

-- | \(\mathcal{O}(0)\) Construct 'ShortText' from UTF-8 encoded 'ShortByteString'
--
-- This operation has effectively no overhead, as it's currently merely a @newtype@-cast.
--
-- __WARNING__: Unlike the safe 'fromShortByteString' conversion, this
-- conversion is /unsafe/ as it doesn't validate the well-formedness of the
-- UTF-8 encoding.
fromShortByteStringUnsafe :: ShortByteString -> ShortText
fromShortByteStringUnsafe = ShortText

-- | \(\mathcal{O}(n)\) Construct 'ShortText' from UTF-8 encoded 'BS.ByteString'
--
-- Returns 'Nothing' in case of invalid UTF-8 encoding.
fromByteString :: BS.ByteString -> Maybe ShortText
fromByteString = fromShortByteString . BSS.toShort

-- | \(\mathcal{O}(n)\) Construct 'ShortText' from UTF-8 encoded 'BS.ByteString'
--
-- This operation is \(\mathcal{O}(n)\) because the 'BS.ByteString' needs to be
-- copied into an unpinned 'ByteArray#'.
--
-- __WARNING__: Unlike the safe 'fromByteString' conversion, this
-- conversion is /unsafe/ as it doesn't validate the well-formedness of the
-- UTF-8 encoding.
fromByteStringUnsafe :: BS.ByteString -> ShortText
fromByteStringUnsafe = ShortText . BSS.toShort

----------------------------------------------------------------------------

encodeString :: TextEncoding -> String -> BS.ByteString
encodeString te str = unsafePerformIO $ GHC.withCStringLen te str BS.packCStringLen

-- decodeString :: TextEncoding -> BS.ByteString -> Maybe String
-- decodeString te bs = cvtEx $ unsafePerformIO $ try $ BS.useAsCStringLen bs (GHC.peekCStringLen te)
--   where
--     cvtEx :: Either IOException a -> Maybe a
--     cvtEx = either (const Nothing) Just

decodeString' :: TextEncoding -> BS.ByteString -> String
decodeString' te bs = unsafePerformIO $ BS.useAsCStringLen bs (GHC.peekCStringLen te)

decodeStringShort' :: TextEncoding -> ShortByteString -> String
decodeStringShort' te = decodeString' te . BSS.fromShort

encodeStringShort :: TextEncoding -> String -> BSS.ShortByteString
encodeStringShort te = BSS.toShort . encodeString te


isValidUtf8 :: ShortText -> Bool
isValidUtf8 st = (==0) $ unsafePerformIO (c_text_short_is_valid_utf8 (toByteArray# st) (toCSize st))

foreign import ccall unsafe "hs_text_short_is_valid_utf8" c_text_short_is_valid_utf8 :: ByteArray# -> CSize -> IO CInt

-- | \(\mathcal{O}(n)\) Split 'ShortText' into two halves.
--
-- @'splitAt' n t@ returns a pair of 'ShortText' with the following properties:
--
-- prop> length (fst (split n t)) == min (length t) (max 0 n)
--
-- prop> fst (split n t) <> snd (split n t) == t
--
-- @since TBD
splitAt :: Int -> ShortText -> (ShortText,ShortText)
splitAt i st
  | i    <= 0    = (mempty,st)
  | len2 <= 0    = (st,mempty)
  | otherwise    = (slice st 0 ofs, slice st ofs len2)
  where
    ofs   = unsafePerformIO (c_text_short_index_ofs (toByteArray# st) stsz (fromIntegral i))
    stsz  = toCSize st
    len2  = stsz-ofs

foreign import ccall unsafe "hs_text_short_index_ofs" c_text_short_index_ofs :: ByteArray# -> CSize -> CSize -> IO CSize

----------------------------------------------------------------------------

-- | Construct a new 'ShortText' from an existing one by slicing
--
-- NB: The 'CSize' arguments refer to byte-offsets
slice :: ShortText -> CSize -> CSize -> ShortText
slice (ShortText x) ofs_ len_ = ShortText (sliceSBS x (fromIntegral ofs_) (fromIntegral len_))
  where
    sliceSBS :: ShortByteString -> Int -> Int -> ShortByteString
    sliceSBS sbs@(BSSI.SBS ba#) ofs len
      | ofs < 0    = error "invalid offset"
      | len < 0    = error "invalid length"
      | len' == 0  = mempty
      | otherwise  = createSBS len' go
      where
        len0 = BSS.length sbs
        len' = max 0 (min len (len0-ofs))
        ofs' = max 0 ofs

        go :: MBA s -> ST s ()
        go (MBA# mba#) = ST $ \s -> case copyByteArray# ba# (toI ofs') mba# 0# (toI len') s of
                                      s' -> (# s', () #)

        toI (I# i#) = i#

----------------------------------------------------------------------------
-- low-level MutableByteArray# helpers

data MBA s = MBA# (MutableByteArray# s)

createSBS :: Int -> (forall s. MBA s -> ST s ()) -> ShortByteString
createSBS n go = runST $ do
  mba <- newByteArray n
  go mba
  unsafeFreezeSBS mba

unsafeFreezeSBS :: MBA s -> ST s ShortByteString
unsafeFreezeSBS (MBA# mba#)
  = ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', BSSI.SBS ba# #)

newByteArray :: Int -> ST s (MBA s)
newByteArray (I# n#)
  = ST $ \s -> case newByteArray# n# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)


{- TODO:
{-# RULES "ShortText strlit" forall s . fromString (unpackCString# s) = fromAddr# #-}
...
-}
