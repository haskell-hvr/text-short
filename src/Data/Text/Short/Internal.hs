{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UnliftedFFITypes           #-}
{-# LANGUAGE Unsafe                     #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Data.Text.Short.Internal
-- Copyright   : © Herbert Valerio Riedel 2017
-- License     : BSD3
--
-- Maintainer  : hvr@gnu.org
-- Stability   : stable
--
-- Memory-efficient representation of Unicode text strings.
--
-- @since 0.1
module Data.Text.Short.Internal
    ( -- * The 'ShortText' type
      ShortText(..)

      -- * Basic operations
    , null
    , length
    , isAscii
    , splitAt
    , splitAtEnd
    , indexEndMaybe
    , indexMaybe
    , isPrefixOf
    , stripPrefix
    , isSuffixOf
    , stripSuffix

    , cons
    , snoc
    , uncons
    , unsnoc

    , findIndex
    , find
    , all

    , span
    , spanEnd

    , intersperse
    , intercalate
    , reverse
    , filter

      -- * Conversions
      -- ** 'Char'
    , singleton

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

      -- * misc
      -- ** For Haddock

    , BS.ByteString
    , T.Text
    , module Prelude

      -- ** Internals
    , isValidUtf8
    ) where

import           Control.DeepSeq                (NFData)
import           Control.Monad.ST               (stToIO)
import           Data.Binary
import           Data.Bits                      (shiftR, (.&.), (.|.))
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Builder        as BB
import           Data.ByteString.Short          (ShortByteString)
import qualified Data.ByteString.Short          as BSS
import qualified Data.ByteString.Short.Internal as BSSI
import           Data.Char                      (chr, ord)
import           Data.Hashable                  (Hashable)
import qualified Data.List                      as List
import           Data.Semigroup
import qualified Data.String                    as S
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Foreign.C
import qualified GHC.CString                    as GHC
import           GHC.Exts                       (Addr#, ByteArray#, Int (I#),
                                                 Int#, MutableByteArray#,
                                                 Ptr (..), RealWorld, Word (W#),
                                                 copyAddrToByteArray#,
                                                 copyByteArray#, newByteArray#,
                                                 sizeofByteArray#,
                                                 unsafeFreezeByteArray#,
                                                 writeWord8Array#)
import qualified GHC.Foreign                    as GHC
import           GHC.IO.Encoding
import           GHC.ST
import           Prelude                        hiding (all, any, break, concat,
                                                 drop, dropWhile, filter, head,
                                                 init, last, length, null,
                                                 reverse, span, splitAt, tail,
                                                 take, takeWhile)
import           System.IO.Unsafe
import           Text.Printf                    (PrintfArg, formatArg,
                                                 formatString)

import qualified PrimOps

-- | A compact representation of Unicode strings.
--
-- A 'ShortText' value is a sequence of Unicode scalar values, as defined in
-- <http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35 §3.9, definition D76 of the Unicode 5.2 standard >;
-- This means that a 'ShortText' is a list of (scalar) Unicode code-points (i.e. code-points in the range @[U+00 .. U+D7FF] ∪ [U+E000 .. U+10FFFF]@).
--
-- This type relates to 'T.Text' as 'ShortByteString' relates to 'BS.ByteString' by providing a more compact type. Please consult the documentation of "Data.ByteString.Short" for more information.
--
-- Currently, a boxed unshared 'T.Text' has a memory footprint of 6 words (i.e. 48 bytes on 64-bit systems) plus 2 or 4 bytes per code-point (due to the internal UTF-16 representation). Each 'T.Text' value which can share its payload with another 'T.Text' requires only 4 words additionally. Unlike 'BS.ByteString', 'T.Text' use unpinned memory.
--
-- In comparison, the footprint of a boxed 'ShortText' is only 4 words (i.e. 32 bytes on 64-bit systems) plus 1, 2, 3, or 4 bytes per code-point (due to the internal UTF-8 representation).
-- It can be shown that for realistic data <http://utf8everywhere.org/#asian UTF-16 has a space overhead of 50% over UTF-8>.
--
-- @since 0.1
newtype ShortText = ShortText ShortByteString
                  deriving (Monoid,Data.Semigroup.Semigroup,Hashable,NFData)

instance Eq ShortText where
  {-# INLINE (==) #-}
  (==) x y
    | lx /= ly  = False
    | lx ==  0  = True
    | otherwise = case PrimOps.compareByteArrays# (toByteArray# x) 0# (toByteArray# y) 0# n# of
                    0# -> True
                    _  -> False
    where
      !lx@(I# n#) = toLength x
      !ly = toLength y

instance Ord ShortText where
  compare t1 t2
    | n == 0  = compare n1 n2
    | otherwise = case PrimOps.compareByteArrays# ba1# 0# ba2# 0# n# of
        r# | I# r# < 0 -> LT
           | I# r# > 0 -> GT
           | n1 < n2   -> LT
           | n1 > n2   -> GT
           | otherwise -> EQ
    where
      ba1# = toByteArray# t1
      ba2# = toByteArray# t2
      !n1 = toLength t1
      !n2 = toLength t2
      !n@(I# n#) = n1 `min` n2

instance Show ShortText where
    showsPrec p (ShortText b) = showsPrec p (decodeStringShort' utf8 b)
    show (ShortText b)        = show        (decodeStringShort' utf8 b)

instance Read ShortText where
    readsPrec p = map (\(x,s) -> (ShortText $ encodeStringShort utf8 x,s)) . readsPrec p

-- | @since 0.1.2
instance PrintfArg ShortText where
  formatArg txt = formatString $ toString txt

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
--
-- >>> null ""
-- True
--
-- prop> null (singleton c) == False
--
-- prop> null t == (length t == 0)
--
-- @since 0.1
null :: ShortText -> Bool
null = BSS.null . toShortByteString

-- | \(\mathcal{O}(n)\) Count the number of Unicode code-points in a 'ShortText'.
--
-- >>> length "abcd€"
-- 5
--
-- >>> length ""
-- 0
--
-- prop> length t >= 0
--
-- @since 0.1
length :: ShortText -> Int
length st = fromIntegral $ unsafeDupablePerformIO (c_text_short_length (toByteArray# st) (toCSize st))

foreign import ccall unsafe "hs_text_short_length" c_text_short_length :: ByteArray# -> CSize -> IO CSize

-- | \(\mathcal{O}(n)\) Test whether 'ShortText' contains only ASCII code-points (i.e. only U+0000 through U+007F).
--
-- This is a more efficient version of @'all' 'Data.Char.isAscii'@.
--
-- >>> isAscii ""
-- True
--
-- >>> isAscii "abc\NUL"
-- True
--
-- >>> isAscii "abcd€"
-- False
--
-- prop> isAscii t == all (< '\x80') t
--
-- @since 0.1
isAscii :: ShortText -> Bool
isAscii st = (/= 0) $ unsafeDupablePerformIO (c_text_short_is_ascii (toByteArray# st) sz)
  where
    sz = toCSize st

foreign import ccall unsafe "hs_text_short_is_ascii" c_text_short_is_ascii :: ByteArray# -> CSize -> IO CInt

-- | \(\mathcal{O}(n)\) Test whether /all/ code points in 'ShortText' satisfy a predicate.
--
-- >>> all (const False) ""
-- True
--
-- >>> all (> 'c') "abcdabcd"
-- False
--
-- >>> all (/= 'c') "abdabd"
-- True
--
-- @since 0.1.2
all :: (Char -> Bool) -> ShortText -> Bool
all p st = go 0
  where
    go ofs
      | ofs >= sz  = True
      | otherwise  = let !cp = readCodePoint st ofs
                         c = chr (fromIntegral cp)
                     in if p c
                        then go (ofs+cpLen (fromIntegral cp))
                        else False

    !sz = toCSize st

-- | \(\mathcal{O}(n)\) Return the left-most codepoint in 'ShortText' that satisfies the given predicate.
--
-- >>> find (> 'b') "abcdabcd"
-- Just 'c'
--
-- >>> find (> 'b') "ababab"
-- Nothing
--
-- @since 0.1.2
find :: (Char -> Bool) -> ShortText -> Maybe Char
find p st = go 0
  where
    go ofs
      | ofs >= sz  = Nothing
      | otherwise  = let !cp = readCodePoint st ofs
                         c = chr (fromIntegral cp)
                     in if p c
                        then Just c
                        else go (ofs+cpLen (fromIntegral cp))

    !sz = toCSize st

-- | \(\mathcal{O}(n)\) Return the index of the left-most codepoint in 'ShortText' that satisfies the given predicate.
--
-- >>> findIndex (> 'b') "abcdabcdef"
-- Just 2
--
-- >>> findIndex (> 'b') "ababab"
-- Nothing
--
-- prop> (indexMaybe t =<< findIndex p t) == find p t
--
-- @since 0.1.2
findIndex :: (Char -> Bool) -> ShortText -> Maybe Int
findIndex p st = go 0 0
  where
    go ofs i
      | ofs >= sz  = Nothing
      | otherwise  = let !cp = readCodePoint st ofs
                     in if p (chr (fromIntegral cp))
                        then Just i
                        else go (ofs+cpLen (fromIntegral cp)) (i+1)

    !sz = toCSize st

-- | \(\mathcal{O}(n)\) Split 'ShortText' into longest prefix satisfying the given predicate and the remaining suffix.
--
-- >>> span (< 'c') "abcdabcd"
-- ("ab","cdabcd")
--
-- prop> fst (span p t) <> snd (span p t) == t
--
-- @since 0.1.2
span :: (Char -> Bool) -> ShortText -> (ShortText,ShortText)
span p st = splitAt' (go 0) st
  where
    go ofs | ofs >= sz  = ofs
    go ofs
      | p (chr (fromIntegral cp)) = go (ofs+cpLen (fromIntegral cp))
      | otherwise                 = ofs
      where
        !cp = readCodePoint st ofs

    !sz = toCSize st

-- | \(\mathcal{O}(n)\) Split 'ShortText' into longest suffix satisfying the given predicate and the preceding prefix.
--
-- >>> spanEnd (> 'c') "abcdabcd"
-- ("abcdabc","d")
--
-- prop> fst (spanEnd p t) <> snd (spanEnd p t) == t
--
-- @since 0.1.2
spanEnd :: (Char -> Bool) -> ShortText -> (ShortText,ShortText)
spanEnd p st = splitAt' (go sz) st
  where
    go 0 = 0
    go ofs
      | p (chr (fromIntegral cp)) = go (ofs-cpLen (fromIntegral cp))
      | otherwise                 = ofs
      where
        !cp = readCodePointRev st ofs

    !sz = toCSize st

----------------------------------------------------------------------------

toCSize :: ShortText -> CSize
toCSize = fromIntegral . BSS.length . toShortByteString

toLength :: ShortText -> Int
toLength st = I# (toLength# st)

toLength# :: ShortText -> Int#
toLength# st = sizeofByteArray# (toByteArray# st)

toByteArray# :: ShortText -> ByteArray#
toByteArray# (ShortText (BSSI.SBS ba#)) = ba#

-- | \(\mathcal{O}(0)\) Converts to UTF-8 encoded 'ShortByteString'
--
-- This operation has effectively no overhead, as it's currently merely a @newtype@-cast.
--
-- @since 0.1
toShortByteString :: ShortText -> ShortByteString
toShortByteString (ShortText b) = b

-- | \(\mathcal{O}(n)\) Converts to UTF-8 encoded 'BS.ByteString'
--
-- @since 0.1
toByteString :: ShortText -> BS.ByteString
toByteString = BSS.fromShort . toShortByteString

-- | Construct a 'BB.Builder' that encodes 'ShortText' as UTF-8.
--
-- @since 0.1
toBuilder :: ShortText -> BB.Builder
toBuilder = BB.shortByteString . toShortByteString

-- | \(\mathcal{O}(n)\) Convert to 'String'
--
-- prop> (fromString . toString) t == t
--
-- __Note__: See documentation of 'fromString' for why @('toString' . 'fromString')@ is not an identity function.
--
-- @since 0.1
toString :: ShortText -> String
toString = decodeStringShort' utf8 . toShortByteString

-- | \(\mathcal{O}(n)\) Convert to 'T.Text'
--
-- prop> (fromText . toText) t == t
--
-- prop> (toText . fromText) t == t
--
-- This is currently not \(\mathcal{O}(1)\) because currently 'T.Text' uses UTF-16 as its internal representation.
-- In the event that 'T.Text' will change its internal representation to UTF-8 this operation will become \(\mathcal{O}(1)\).
--
-- @since 0.1
toText :: ShortText -> T.Text
toText = T.decodeUtf8 . toByteString

----

-- | \(\mathcal{O}(n)\) Construct/pack from 'String'
--
-- >>> fromString []
-- ""
--
-- >>> fromString ['a','b','c']
-- "abc"
--
-- >>> fromString ['\55295','\55296','\57343','\57344'] -- U+D7FF U+D800 U+DFFF U+E000
-- "\55295\65533\65533\57344"
--
-- __Note__: This function is total because it replaces the (invalid) code-points U+D800 through U+DFFF with the replacement character U+FFFD.
--
-- @since 0.1
fromString :: String -> ShortText
fromString []  = mempty
fromString [c] = singleton c
fromString s = ShortText . encodeStringShort utf8 . map r $ s
  where
    r c | 0xd800 <= x && x < 0xe000 = '\xFFFD'
        | otherwise                 = c
      where
        x = ord c

-- | \(\mathcal{O}(n)\) Construct 'ShortText' from 'T.Text'
--
-- This is currently not \(\mathcal{O}(1)\) because currently 'T.Text' uses UTF-16 as its internal representation.
-- In the event that 'T.Text' will change its internal representation to UTF-8 this operation will become \(\mathcal{O}(1)\).
--
-- @since 0.1
fromText :: T.Text -> ShortText
fromText = fromByteStringUnsafe . T.encodeUtf8

-- | \(\mathcal{O}(n)\) Construct 'ShortText' from UTF-8 encoded 'ShortByteString'
--
-- This operation doesn't copy the input 'ShortByteString' but it
-- cannot be \(\mathcal{O}(1)\) because we need to validate the UTF-8 encoding.
--
-- Returns 'Nothing' in case of invalid UTF-8 encoding.
--
-- >>> fromShortByteString "\x00\x38\xF0\x90\x8C\x9A" -- U+00 U+38 U+1031A
-- Just "\NUL8\66330"
--
-- >>> fromShortByteString "\xC0\x80" -- invalid denormalised U+00
-- Nothing
--
-- >>> fromShortByteString "\xED\xA0\x80" -- U+D800 (non-scalar code-point)
-- Nothing
--
-- >>> fromShortByteString "\xF4\x8f\xbf\xbf" -- U+10FFFF
-- Just "\1114111"
--
-- >>> fromShortByteString "\xF4\x90\x80\x80" -- U+110000 (invalid)
-- Nothing
--
-- prop> fromShortByteString (toShortByteString t) == Just t
--
-- @since 0.1
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
--
-- @since 0.1.1
fromShortByteStringUnsafe :: ShortByteString -> ShortText
fromShortByteStringUnsafe = ShortText

-- | \(\mathcal{O}(n)\) Construct 'ShortText' from UTF-8 encoded 'BS.ByteString'
--
-- 'fromByteString' accepts (or rejects) the same input data as 'fromShortByteString'.
--
-- Returns 'Nothing' in case of invalid UTF-8 encoding.
--
-- @since 0.1
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
--
-- @since 0.1.1
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

-- isValidUtf8' :: ShortText -> Int
-- isValidUtf8' st = fromIntegral $ unsafeDupablePerformIO (c_text_short_is_valid_utf8 (toByteArray# st) (toCSize st))

isValidUtf8 :: ShortText -> Bool
isValidUtf8 st = (==0) $ unsafeDupablePerformIO (c_text_short_is_valid_utf8 (toByteArray# st) (toCSize st))

foreign import ccall unsafe "hs_text_short_is_valid_utf8" c_text_short_is_valid_utf8 :: ByteArray# -> CSize -> IO CInt

foreign import ccall unsafe "hs_text_short_index_cp" c_text_short_index :: ByteArray# -> CSize -> CSize -> IO Word32

-- | \(\mathcal{O}(n)\) Lookup /i/-th code-point in 'ShortText'.
--
-- Returns 'Nothing' if out of bounds.
--
-- prop> indexMaybe (singleton c) 0 == Just c
--
-- prop> indexMaybe t 0 == fmap fst (uncons t)
--
-- prop> indexMaybe mempty i == Nothing
--
-- @since 0.1.2
indexMaybe :: ShortText -> Int -> Maybe Char
indexMaybe st i
  | i < 0         = Nothing
  | cp < 0x110000 = Just (chr (fromIntegral cp))
  | otherwise     = Nothing
  where
    cp = unsafeDupablePerformIO (c_text_short_index (toByteArray# st) (toCSize st) (fromIntegral i))

-- | \(\mathcal{O}(n)\) Lookup /i/-th code-point from the end of 'ShortText'.
--
-- Returns 'Nothing' if out of bounds.
--
-- prop> indexEndMaybe (singleton c) 0 == Just c
--
-- prop> indexEndMaybe t 0 == fmap snd (unsnoc t)
--
-- prop> indexEndMaybe mempty i == Nothing
--
-- @since 0.1.2
indexEndMaybe :: ShortText -> Int -> Maybe Char
indexEndMaybe st i
  | i < 0         = Nothing
  | cp < 0x110000 = Just (chr (fromIntegral cp))
  | otherwise     = Nothing
  where
    cp = unsafeDupablePerformIO (c_text_short_index_rev (toByteArray# st) (toCSize st) (fromIntegral i))

foreign import ccall unsafe "hs_text_short_index_cp_rev" c_text_short_index_rev :: ByteArray# -> CSize -> CSize -> IO Word32


-- | \(\mathcal{O}(n)\) Split 'ShortText' into two halves.
--
-- @'splitAt' n t@ returns a pair of 'ShortText' with the following properties:
--
-- prop> length (fst (splitAt n t)) == min (length t) (max 0 n)
--
-- prop> fst (splitAt n t) <> snd (splitAt n t) == t
--
-- >>> splitAt 2 "abcdef"
-- ("ab","cdef")
--
-- >>> splitAt 10 "abcdef"
-- ("abcdef","")
--
-- >>> splitAt (-1) "abcdef"
-- ("","abcdef")
--
-- @since 0.1.2
splitAt :: Int -> ShortText -> (ShortText,ShortText)
splitAt i st
  | i <= 0    = (mempty,st)
  | otherwise = splitAt' ofs st
  where
    ofs   = unsafeDupablePerformIO (c_text_short_index_ofs (toByteArray# st) stsz (fromIntegral i))
    stsz  = toCSize st

-- | \(\mathcal{O}(n)\) Split 'ShortText' into two halves.
--
-- @'splitAtEnd' n t@ returns a pair of 'ShortText' with the following properties:
--
-- prop> length (snd (splitAtEnd n t)) == min (length t) (max 0 n)
--
-- prop> fst (splitAtEnd n t) <> snd (splitAtEnd n t) == t
--
-- prop> splitAtEnd n t == splitAt (length t - n) t
--
-- >>> splitAtEnd 2 "abcdef"
-- ("abcd","ef")
--
-- >>> splitAtEnd 10 "abcdef"
-- ("","abcdef")
--
-- >>> splitAtEnd (-1) "abcdef"
-- ("abcdef","")
--
-- @since 0.1.2
splitAtEnd :: Int -> ShortText -> (ShortText,ShortText)
splitAtEnd i st
  | i <= 0      = (st,mempty)
  | ofs >= stsz = (mempty,st)
  | otherwise   = splitAt' ofs st
  where
    ofs   = unsafeDupablePerformIO (c_text_short_index_ofs_rev (toByteArray# st) stsz (fromIntegral (i-1)))
    stsz  = toCSize st

{-# INLINE splitAt' #-}
splitAt' :: CSize -> ShortText -> (ShortText,ShortText)
splitAt' ofs st
  | ofs  == 0    = (mempty,st)
  | ofs  >  stsz = (st,mempty)
  | otherwise    = (slice st 0 ofs, slice st ofs (stsz-ofs))
  where
    !stsz  = toCSize st

foreign import ccall unsafe "hs_text_short_index_ofs" c_text_short_index_ofs :: ByteArray# -> CSize -> CSize -> IO CSize

foreign import ccall unsafe "hs_text_short_index_ofs_rev" c_text_short_index_ofs_rev :: ByteArray# -> CSize -> CSize -> IO CSize


-- | \(\mathcal{O}(n)\) Inverse operation to 'cons'
--
-- Returns 'Nothing' for empty input 'ShortText'.
--
-- prop> uncons (cons c t) == Just (c,t)
--
-- >>> uncons ""
-- Nothing
--
-- >>> uncons "fmap"
-- Just ('f',"map")
--
-- @since 0.1.2
uncons :: ShortText -> Maybe (Char,ShortText)
uncons st
  | null st    = Nothing
  | len2 == 0  = Just (c0, mempty)
  | otherwise  = Just (c0, slice st ofs len2)
  where
    c0  = chr (fromIntegral cp0)
    cp0 = fromIntegral (readCodePoint st 0)
    ofs = cpLen cp0
    len2 = toCSize st - ofs

-- | \(\mathcal{O}(n)\) Inverse operation to 'snoc'
--
-- Returns 'Nothing' for empty input 'ShortText'.
--
-- prop> unsnoc (snoc t c) == Just (t,c)
--
-- >>> unsnoc ""
-- Nothing
--
-- >>> unsnoc "fmap"
-- Just ("fma",'p')
--
-- @since 0.1.2
unsnoc :: ShortText -> Maybe (ShortText,Char)
unsnoc st
  | null st    = Nothing
  | len1 == 0  = Just (mempty, c0)
  | otherwise  = Just (slice st 0 len1, c0)
  where
    c0  = chr (fromIntegral cp0)
    cp0 = fromIntegral (readCodePointRev st stsz)
    stsz = toCSize st
    len1 = stsz - cpLen cp0

-- | \(\mathcal{O}(n)\) Tests whether the first 'ShortText' is a prefix of the second 'ShortText'
--
-- >>> isPrefixOf "ab" "abcdef"
-- True
--
-- >>> isPrefixOf "ac" "abcdef"
-- False
--
-- prop> isPrefixOf "" t == True
--
-- prop> isPrefixOf t t == True
--
-- @since 0.1.2
isPrefixOf :: ShortText -> ShortText -> Bool
isPrefixOf x y
  | lx > ly = False
  | lx == 0 = True
  | otherwise = case PrimOps.compareByteArrays# (toByteArray# x) 0# (toByteArray# y) 0# n# of
                  0# -> True
                  _  -> False
  where
    !lx@(I# n#) = toLength x
    !ly = toLength y

-- | \(\mathcal{O}(n)\) Strip prefix from second 'ShortText' argument.
--
-- Returns 'Nothing' if first argument is not a prefix of the second argument.
--
-- >>> stripPrefix "text-" "text-short"
-- Just "short"
--
-- >>> stripPrefix "test-" "text-short"
-- Nothing
--
-- @since 0.1.2
stripPrefix :: ShortText -> ShortText -> Maybe ShortText
stripPrefix pfx t
  | isPrefixOf pfx t = Just $! snd (splitAt' (toCSize pfx) t)
  | otherwise        = Nothing

-- | \(\mathcal{O}(n)\) Tests whether the first 'ShortText' is a suffix of the second 'ShortText'
--
-- >>> isSuffixOf "ef" "abcdef"
-- True
--
-- >>> isPrefixOf "df" "abcdef"
-- False
--
-- prop> isSuffixOf "" t == True
--
-- prop> isSuffixOf t t == True
--
-- @since 0.1.2
isSuffixOf :: ShortText -> ShortText -> Bool
isSuffixOf x y
  | lx > ly = False
  | lx == 0 = True
  | otherwise = case PrimOps.compareByteArrays# (toByteArray# x) 0# (toByteArray# y) ofs2# n# of
                  0# -> True
                  _  -> False
  where
    !(I# ofs2#) = ly - lx
    !lx@(I# n#) = toLength x
    !ly = toLength y

-- | \(\mathcal{O}(n)\) Strip suffix from second 'ShortText' argument.
--
-- Returns 'Nothing' if first argument is not a suffix of the second argument.
--
-- >>> stripSuffix "-short" "text-short"
-- Just "text"
--
-- >>> stripSuffix "-utf8" "text-short"
-- Nothing
--
-- @since 0.1.2
stripSuffix :: ShortText -> ShortText -> Maybe ShortText
stripSuffix sfx t
  | isSuffixOf sfx t = Just $! fst (splitAt' pfxLen t)
  | otherwise        = Nothing
  where
    pfxLen = toCSize t - toCSize sfx

----------------------------------------------------------------------------

-- | \(\mathcal{O}(n)\) Insert character between characters of 'ShortText'.
--
-- >>> intersperse '*' "_"
-- "_"
--
-- >>> intersperse '*' "MASH"
-- "M*A*S*H"
--
-- @since 0.1.2
intersperse :: Char -> ShortText -> ShortText
intersperse c st
  | null st = mempty
  | sn == 1 = st
  | otherwise = create newsz $ \mba -> do
      let cp0 = readCodePoint st 0
          cp0sz = cpLen cp0
      writeCodePointN cp0sz mba 0 cp0
      go mba (sn - 1) cp0sz cp0sz
  where
    newsz = fromIntegral (ssz + (fromIntegral (sn-1) * csz))
    ssz = toCSize st
    sn  = length st
    csz = cpLen cp
    cp  = ch2cp c

    go _   0 !_  !_   = return ()
    go mba n ofs ofs2 = do
      let cp1 = readCodePoint st ofs2
          cp1sz = cpLen cp1
      writeCodePointN csz   mba (fromIntegral ofs) cp
      writeCodePointN cp1sz mba (fromIntegral (ofs+csz)) cp1
      go mba (n-1) (ofs+csz+cp1sz) (ofs2+cp1sz)

-- | \(\mathcal{O}(n)\) Insert 'ShortText' inbetween list of 'ShortText's.
--
-- >>> intercalate ", " []
-- ""
--
-- >>> intercalate ", " ["foo"]
-- "foo"
--
-- >>> intercalate ", " ["foo","bar","doo"]
-- "foo, bar, doo"
--
-- @since 0.1.2
intercalate :: ShortText -> [ShortText] -> ShortText
intercalate sep ts = mconcat (List.intersperse sep ts)

-- | \(\mathcal{O}(n)\) Reverse characters in 'ShortText'.
--
-- >>> reverse "star live desserts"
-- "stressed evil rats"
--
-- prop> reverse (singleton c) == singleton c
--
-- prop> reverse (reverse t) == t
--
-- @since 0.1.2
reverse :: ShortText -> ShortText
reverse st
  | null st   = mempty
  | sn == 1   = st
  | otherwise = create sz $ go sn 0
  where
    sz = fromIntegral (toCSize st)
    sn = length st

    go 0 !_  _   = return ()
    go i ofs mba = do
      let cp   = readCodePoint st ofs
          cpsz = cpLen cp
          ofs' = ofs+cpsz
      writeCodePointN cpsz mba (sz - fromIntegral ofs') cp
      go (i-1) ofs' mba


-- | \(\mathcal{O}(n)\) Remove characters from 'ShortText' which don't satisfy given predicate.
--
-- >>> filter (`notElem` ['a','e','i','o','u']) "You don't need vowels to convey information!"
-- "Y dn't nd vwls t cnvy nfrmtn!"
--
-- prop> filter (const False) t == ""
--
-- prop> filter (const True) t == t
--
-- prop> length (filter p t) <= length t
--
-- prop> filter p t == pack [ c | c <- unpack t, p c ]
--
-- @since 0.1.2
filter :: (Char -> Bool) -> ShortText -> ShortText
filter p t = fromString [ c | c <- toString t, p c ]

----------------------------------------------------------------------------

-- | Construct a new 'ShortText' from an existing one by slicing
--
-- NB: The 'CSize' arguments refer to byte-offsets
slice :: ShortText -> CSize -> CSize -> ShortText
slice (ShortText (BSSI.SBS ba#)) (fromIntegral -> ofs) (fromIntegral -> len)
  | ofs < 0    = error "invalid offset"
  | len < 0    = error "invalid length"
  | len' == 0  = mempty
  | otherwise  = create len' go
  where
    len0 = I# (sizeofByteArray# ba#)
    !len'@(I# len'#) = max 0 (min len (len0-ofs))
    !(I# ofs'#) = max 0 ofs

    go :: MBA s -> ST s ()
    go (MBA# mba#) = ST $ \s -> case copyByteArray# ba# ofs'# mba# 0# len'# s of
                                  s' -> (# s', () #)

----------------------------------------------------------------------------
-- low-level MutableByteArray# helpers

data MBA s = MBA# { unMBA# :: MutableByteArray# s }

{-# INLINE create #-}
create :: Int -> (forall s. MBA s -> ST s ()) -> ShortText
create n go = runST $ do
  mba <- newByteArray n
  go mba
  unsafeFreeze mba

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: MBA s -> ST s ShortText
unsafeFreeze (MBA# mba#)
  = ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', ShortText (BSSI.SBS ba#) #)

{-# INLINE copyByteArray #-}
copyByteArray :: ShortText -> Int -> MBA s -> Int -> Int -> ST s ()
copyByteArray (ShortText (BSSI.SBS src#)) (I# src_off#) (MBA# dst#) (I# dst_off#) (I# len#)
  = ST $ \s -> case copyByteArray# src# src_off# dst# dst_off# len# s of
                 s' -> (# s', () #)

{-# INLINE newByteArray #-}
newByteArray :: Int -> ST s (MBA s)
newByteArray (I# n#)
  = ST $ \s -> case newByteArray# n# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

{-# INLINE writeWord8Array #-}
writeWord8Array :: MBA s -> Int -> Word -> ST s ()
writeWord8Array (MBA# mba#) (I# i#) (W# w#)
  = ST $ \s -> case writeWord8Array# mba# i# w# s of
                 s' -> (# s', () #)

{-# INLINE copyAddrToByteArray #-}
copyAddrToByteArray :: Ptr a -> MBA RealWorld -> Int -> Int -> ST RealWorld ()
copyAddrToByteArray (Ptr src#) (MBA# dst#) (I# dst_off#) (I# len#)
  = ST $ \s -> case copyAddrToByteArray# src# dst# dst_off# len# s of
                 s' -> (# s', () #)

----------------------------------------------------------------------------
-- Helpers for encoding code points into UTF-8 code units
--
--   7 bits| <    0x80 | 0xxxxxxx
--  11 bits| <   0x800 | 110yyyyx  10xxxxxx
--  16 bits| < 0x10000 | 1110yyyy  10yxxxxx  10xxxxxx
--  21 bits|           | 11110yyy  10yyxxxx  10xxxxxx  10xxxxxx

ch2cp :: Char -> Word
ch2cp = fromIntegral . ord

{-# INLINE cpLen #-}
cpLen :: Word -> CSize
cpLen cp
  | cp <    0x80  = 1
  | cp <   0x800  = 2
  | cp < 0x10000  = 3
  | otherwise     = 4

-- | \(\mathcal{O}(1)\) Construct 'ShortText' from single codepoint.
--
-- prop> singleton c == pack [c]
--
-- prop> length (singleton c) == 1
--
-- >>> singleton 'A'
-- "A"
--
-- >>> map singleton ['\55295','\55296','\57343','\57344'] -- U+D7FF U+D800 U+DFFF U+E000
-- ["\55295","\65533","\65533","\57344"]
--
-- __Note__: This function is total because it replaces the (invalid) code-points U+D800 through U+DFFF with the replacement character U+FFFD.
--
-- @since 0.1.2
singleton :: Char -> ShortText
singleton = singleton' . ch2cp

singleton' :: Word -> ShortText
singleton' cp
  | cp <    0x80  = create 1 $ \mba -> writeCodePoint1 mba 0 cp
  | cp <   0x800  = create 2 $ \mba -> writeCodePoint2 mba 0 cp
  | cp <  0xd800  = create 3 $ \mba -> writeCodePoint3 mba 0 cp
  | cp <  0xe000  = create 3 $ \mba -> writeRepChar    mba 0
  | cp < 0x10000  = create 3 $ \mba -> writeCodePoint3 mba 0 cp
  | otherwise     = create 4 $ \mba -> writeCodePoint4 mba 0 cp

-- | \(\mathcal{O}(n)\) Prepend a character to a 'ShortText'.
--
-- prop> cons c t == singleton c <> t
--
-- @since 0.1.2
cons :: Char -> ShortText -> ShortText
cons (ch2cp -> cp) sfx
  | n == 0        = singleton' cp
  | cp <    0x80  = create (n+1) $ \mba -> writeCodePoint1 mba 0 cp >> copySfx 1 mba
  | cp <   0x800  = create (n+2) $ \mba -> writeCodePoint2 mba 0 cp >> copySfx 2 mba
  | cp <  0xd800  = create (n+3) $ \mba -> writeCodePoint3 mba 0 cp >> copySfx 3 mba
  | cp <  0xe000  = create (n+3) $ \mba -> writeRepChar    mba 0    >> copySfx 3 mba
  | cp < 0x10000  = create (n+3) $ \mba -> writeCodePoint3 mba 0 cp >> copySfx 3 mba
  | otherwise     = create (n+4) $ \mba -> writeCodePoint4 mba 0 cp >> copySfx 4 mba
  where
    !n = toLength sfx
    copySfx ofs mba = copyByteArray sfx 0 mba ofs n

-- | \(\mathcal{O}(n)\) Append a character to the ond of a 'ShortText'.
--
-- prop> snoc t c == t <> singleton c
--
-- @since 0.1.2
snoc :: ShortText -> Char -> ShortText
snoc pfx (ch2cp -> cp)
  | n == 0        = singleton' cp
  | cp <    0x80  = create (n+1) $ \mba -> copyPfx mba >> writeCodePoint1 mba n cp
  | cp <   0x800  = create (n+2) $ \mba -> copyPfx mba >> writeCodePoint2 mba n cp
  | cp <  0xd800  = create (n+3) $ \mba -> copyPfx mba >> writeCodePoint3 mba n cp
  | cp <  0xe000  = create (n+3) $ \mba -> copyPfx mba >> writeRepChar    mba n
  | cp < 0x10000  = create (n+3) $ \mba -> copyPfx mba >> writeCodePoint3 mba n cp
  | otherwise     = create (n+4) $ \mba -> copyPfx mba >> writeCodePoint4 mba n cp
  where
    !n = toLength pfx
    copyPfx mba = copyByteArray pfx 0 mba 0 n

{-
writeCodePoint :: MBA s -> Int -> Word -> ST s ()
writeCodePoint mba ofs cp
  | cp <    0x80  = writeCodePoint1 mba ofs cp
  | cp <   0x800  = writeCodePoint2 mba ofs cp
  | cp <  0xd800  = writeCodePoint3 mba ofs cp
  | cp <  0xe000  = writeRepChar mba ofs
  | cp < 0x10000  = writeCodePoint3 mba ofs cp
  | otherwise     = writeCodePoint4 mba ofs cp
-}

writeCodePointN :: CSize -> MBA s -> Int -> Word -> ST s ()
writeCodePointN 1 = writeCodePoint1
writeCodePointN 2 = writeCodePoint2
writeCodePointN 3 = writeCodePoint3
writeCodePointN 4 = writeCodePoint4
writeCodePointN _ = undefined

writeCodePoint1 :: MBA s -> Int -> Word -> ST s ()
writeCodePoint1 mba ofs cp =
  writeWord8Array mba ofs cp

writeCodePoint2 :: MBA s -> Int -> Word -> ST s ()
writeCodePoint2 mba ofs cp = do
  writeWord8Array mba  ofs    (0xc0 .|. (cp `shiftR` 6))
  writeWord8Array mba (ofs+1) (0x80 .|. (cp               .&. 0x3f))

writeCodePoint3 :: MBA s -> Int -> Word -> ST s ()
writeCodePoint3 mba ofs cp = do
  writeWord8Array mba  ofs    (0xe0 .|.  (cp `shiftR` 12))
  writeWord8Array mba (ofs+1) (0x80 .|. ((cp `shiftR` 6)  .&. 0x3f))
  writeWord8Array mba (ofs+2) (0x80 .|. (cp               .&. 0x3f))

writeCodePoint4 :: MBA s -> Int -> Word -> ST s ()
writeCodePoint4 mba ofs cp = do
  writeWord8Array mba  ofs    (0xf0 .|.  (cp `shiftR` 18))
  writeWord8Array mba (ofs+1) (0x80 .|. ((cp `shiftR` 12) .&. 0x3f))
  writeWord8Array mba (ofs+2) (0x80 .|. ((cp `shiftR` 6)  .&. 0x3f))
  writeWord8Array mba (ofs+3) (0x80 .|. (cp               .&. 0x3f))

writeRepChar :: MBA s -> Int -> ST s ()
writeRepChar mba ofs = do
  writeWord8Array mba ofs     0xef
  writeWord8Array mba (ofs+1) 0xbf
  writeWord8Array mba (ofs+2) 0xbd

-- beware: UNSAFE!
readCodePoint :: ShortText -> CSize -> Word
readCodePoint st ofs = fromIntegral $ unsafeDupablePerformIO (c_text_short_ofs_cp (toByteArray# st) ofs)

foreign import ccall unsafe "hs_text_short_ofs_cp" c_text_short_ofs_cp :: ByteArray# -> CSize -> IO Word32

readCodePointRev :: ShortText -> CSize -> Word
readCodePointRev st ofs = fromIntegral $ unsafeDupablePerformIO (c_text_short_ofs_cp_rev (toByteArray# st) ofs)

foreign import ccall unsafe "hs_text_short_ofs_cp_rev" c_text_short_ofs_cp_rev :: ByteArray# -> CSize -> IO Word32

----------------------------------------------------------------------------
-- string literals

{- TODO

instance GHC.Exts.IsList ShortText where
    type (Item ShortText) = Char
    fromList = fromString
    toList = toString
-}

-- | Surrogate pairs (@[U+D800 .. U+DFFF]@) in literals are replaced by U+FFFD.
--
-- This matches the behaviour of 'IsString' instance for 'T.Text'.
instance S.IsString ShortText where
    fromString = fromStringLit

-- i.e., don't inline before Phase 0
{-# INLINE [0] fromStringLit #-}
fromStringLit :: String -> ShortText
fromStringLit = fromString

{-# RULES "ShortText empty literal" fromStringLit "" = mempty #-}

-- TODO: this doesn't seem to fire
{-# RULES "ShortText singleton literal" forall c . fromStringLit [c] = singleton c #-}

{-# RULES "ShortText literal ASCII" forall s . fromStringLit (GHC.unpackCString# s) = fromLitAsciiAddr# s #-}

{-# RULES "ShortText literal UTF-8" forall s . fromStringLit (GHC.unpackCStringUtf8# s) = fromLitMUtf8Addr# s #-}

{-# NOINLINE fromLitAsciiAddr# #-}
fromLitAsciiAddr# :: Addr# -> ShortText
fromLitAsciiAddr# (Ptr -> ptr) = unsafeDupablePerformIO $ do
  sz <- fromIntegral `fmap` c_strlen ptr

  case sz `compare` 0 of
    EQ -> return mempty -- should not happen if rules fire correctly
    GT -> stToIO $ do
      mba <- newByteArray sz
      copyAddrToByteArray ptr mba 0 sz
      unsafeFreeze mba
    LT -> return (error "fromLitAsciiAddr#") -- should never happen unless strlen(3) overflows

foreign import ccall unsafe "strlen" c_strlen :: CString -> IO CSize

-- GHC uses an encoding resembling Modified UTF-8 for non-ASCII string-literals
{-# NOINLINE fromLitMUtf8Addr# #-}
fromLitMUtf8Addr# :: Addr# -> ShortText
fromLitMUtf8Addr# (Ptr -> ptr) = unsafeDupablePerformIO $ do
  sz <- c_text_short_mutf8_strlen ptr

  case sz `compare` 0 of
    EQ -> return mempty -- should not happen if rules fire correctly
    GT -> stToIO $ do
      mba <- newByteArray sz
      copyAddrToByteArray ptr mba 0 sz
      unsafeFreeze mba
    LT -> do
      mba <- stToIO (newByteArray (abs sz))
      c_text_short_mutf8_trans ptr (unMBA# mba)
      stToIO (unsafeFreeze mba)

foreign import ccall unsafe "hs_text_short_mutf8_strlen" c_text_short_mutf8_strlen :: CString -> IO Int

foreign import ccall unsafe "hs_text_short_mutf8_trans" c_text_short_mutf8_trans :: CString -> MutableByteArray# RealWorld -> IO ()

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text.Short (pack, unpack)
-- >>> import Text.Show.Functions ()
-- >>> import qualified Test.QuickCheck.Arbitrary as QC
-- >>> import Test.QuickCheck.Instances ()
-- >>> instance QC.Arbitrary ShortText where { arbitrary = fmap fromString QC.arbitrary }
