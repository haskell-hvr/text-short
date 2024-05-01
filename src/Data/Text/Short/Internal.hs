{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
{-# LANGUAGE TypeFamilies               #-}
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
    , split

    , intersperse
    , intercalate
    , reverse
    , replicate

    , filter
    , dropAround

    , foldl
    , foldl'
    , foldr
    , foldl1
    , foldl1'
    , foldr1

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
    , module Data.Monoid

      -- ** Internals
    , isValidUtf8
    ) where

import           Prelude
  (Bool(..), Ordering(..), Int, Char, String, Maybe(..), IO, Eq, Ord, Num, Read,
   Show, ($), ($!), (.), (==), (/=), (+), (*), (-), (>>), (<=), (<), (>), (>=),
   compare, show, showsPrec, readsPrec, abs, return, fmap, error,
   undefined, otherwise, fromIntegral, max, min, not, fst, snd, map, seq, fail, maybe)

import           Control.DeepSeq                (NFData)
import           Control.Monad.ST               (stToIO)
import           Data.Binary
import           Data.Bits
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Builder        as BB
import           Data.ByteString.Short          (ShortByteString)
import qualified Data.ByteString.Short          as BSS
import qualified Data.ByteString.Short.Internal as BSSI
import           Data.Char                      (ord)
import           Data.Data                      (Data(..),constrIndex, Constr,
                                                 mkConstr, DataType, mkDataType,
                                                 Fixity(Prefix))
import           Data.Hashable                  (Hashable)
import           Data.Typeable                  (Typeable)
import qualified Data.List                      as List
import           Data.Maybe                     (fromMaybe, isNothing)
import           Data.Monoid                    (Monoid, mempty, mconcat)
import           Data.Semigroup
import qualified Data.String                    as S
import qualified Data.Text                      as T
import           Foreign.C
import           GHC.Base                       (assert, unsafeChr)
import qualified GHC.CString                    as GHC
import           GHC.Exts                       (Addr#, ByteArray#, Int (I#),
                                                 Int#, MutableByteArray#,
                                                 Ptr (..), RealWorld, Word (W#))
import qualified GHC.Exts
import qualified GHC.Foreign                    as GHC
import           GHC.IO.Encoding
import           GHC.ST
import           System.IO.Unsafe
import           Text.Printf                    (PrintfArg, formatArg,
                                                 formatString)

import qualified Language.Haskell.TH.Syntax     as TH

#if MIN_VERSION_text(2,0,0)
import qualified Data.Text.Internal             as TI
import qualified Data.Text.Array                as TA
#else
import qualified Data.Text.Encoding             as T
#endif

import qualified PrimOps

-- | A compact representation of Unicode strings.
--
-- A 'ShortText' value is a sequence of Unicode scalar values, as defined in
-- <http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35 §3.9, definition D76 of the Unicode 5.2 standard >;
-- This means that a 'ShortText' is a list of (scalar) Unicode code-points (i.e. code-points in the range @[U+00 .. U+D7FF] ∪ [U+E000 .. U+10FFFF]@).
--
-- This type relates to 'T.Text' as 'ShortByteString' relates to 'BS.ByteString' by providing a more compact type. Please consult the documentation of "Data.ByteString.Short" for more information.
--
-- Currently, a boxed unshared 'T.Text' has a memory footprint of 6 words (i.e. 48 bytes on 64-bit systems) plus 1, 2, 3 or 4 bytes per code-point for text-2 (due to the internal UTF-8 representation) or 2 or 4 bytes per  code-point for text-1 (due to the internal UTF-16 representation). Each 'T.Text' value which can share its payload with another 'T.Text' requires only 4 words additionally. Unlike 'BS.ByteString', 'T.Text' use unpinned memory.
--
-- In comparison, the footprint of a boxed 'ShortText' is only 4 words (i.e. 32 bytes on 64-bit systems) plus 1, 2, 3, or 4 bytes per code-point (due to the internal UTF-8 representation).
--
-- It can be shown that for realistic data <http://utf8everywhere.org/#asian UTF-16, which is used by text-1, has a space overhead of 50% over UTF-8>.
--
-- __NOTE__: The `Typeable` instance isn't defined for GHC 7.8 (and older) prior to @text-short-0.1.3@
--
-- @since 0.1
newtype ShortText = ShortText ShortByteString
                  deriving (Hashable,Monoid,NFData,Data.Semigroup.Semigroup,Typeable)

-- | It exposes a similar 'Data' instance abstraction as 'T.Text' (see
-- discussion referenced there for more details), preserving the
-- @[Char]@ data abstraction at the cost of inefficiency.
--
-- @since 0.1.3
instance Data ShortText where
  gfoldl f z txt = z fromString `f` (toString txt)
  toConstr _ = packConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z fromString)
    _ -> error "gunfold"
  dataTypeOf _ = shortTextDataType

packConstr :: Constr
packConstr = mkConstr shortTextDataType "fromString" [] Prefix

shortTextDataType :: DataType
shortTextDataType = mkDataType "Data.Text.Short" [packConstr]

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
instance Binary ShortText where
    put = put . toShortByteString
    get = do
        sbs <- get
        case fromShortByteString sbs of
          Nothing -> fail "Binary.get(ShortText): Invalid UTF-8 stream"
          Just st -> return st

-- | Since 0.1.3
instance TH.Lift ShortText where
    -- TODO: Use DeriveLift with bytestring-0.11.2.0
    lift t = [| fromString s |]
      where s = toString t

#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = TH.unsafeTExpCoerce . TH.lift
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
all p st = isNothing (findOfs (not . p) st (B 0))

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
    go !ofs
      | ofs >= sz  = Nothing
      | otherwise  = let (c,ofs') = decodeCharAtOfs st ofs
                     in c `seq` ofs' `seq`
                        if p c
                        then Just c
                        else go ofs'

    !sz = toB st

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
    go !ofs !i
      | ofs >= sz  = Nothing
      | otherwise  = let (c,ofs') = decodeCharAtOfs st ofs
                     in c `seq` ofs' `seq`
                        if p c
                        then Just i
                        else go ofs' (i+1)

    !sz = toB st


-- | \(\mathcal{O}(n)\) Splits a string into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- >>> split (=='a') "aabbaca"
-- ["","","bb","c",""]
--
-- >>> split (=='a') ""
-- [""]
--
-- prop> intercalate (singleton c) (split (== c) t) = t
--
-- __NOTE__: 'split' never returns an empty list to match the semantics of its counterpart from "Data.Text".
--
-- @since 0.1.3
split :: (Char -> Bool) -> ShortText -> [ShortText]
split p st0 = go 0
  where
    go !ofs0 = case findOfs' p st0 ofs0 of
      Just (ofs1,ofs2) -> slice st0 ofs0 (ofs1-ofs0) : go ofs2
      Nothing
        | ofs0 == 0 -> st0 : []
        | otherwise -> slice st0 ofs0 (maxOfs-ofs0) : []

    !maxOfs = toB st0

-- internal helper
{-# INLINE findOfs #-}
findOfs :: (Char -> Bool) -> ShortText -> B -> Maybe B
findOfs p st = go
  where
    go :: B -> Maybe B
    go !ofs | ofs >= sz = Nothing
    go !ofs | p c       = Just ofs
            | otherwise = go ofs'
      where
        (c,ofs') = decodeCharAtOfs st ofs

    !sz = toB st

{-# INLINE findOfs' #-}
findOfs' :: (Char -> Bool) -> ShortText -> B -> Maybe (B,B)
findOfs' p st = go
  where
    go :: B -> Maybe (B,B)
    go !ofs | ofs >= sz = Nothing
    go !ofs | p c       = Just (ofs,ofs')
            | otherwise = go ofs'
      where
        (c,ofs') = decodeCharAtOfs st ofs

    !sz = toB st


{-# INLINE findOfsRev #-}
findOfsRev :: (Char -> Bool) -> ShortText -> B -> Maybe B
findOfsRev p st = go
  where
    go (B 0) = Nothing
    go !ofs
      | p (cp2ch cp) = Just ofs
      | otherwise    = go (ofs-cpLen cp)
      where
        !cp = readCodePointRev st ofs

-- | \(\mathcal{O}(n)\) Split 'ShortText' into longest prefix satisfying the given predicate and the remaining suffix.
--
-- >>> span (< 'c') "abcdabcd"
-- ("ab","cdabcd")
--
-- prop> fst (span p t) <> snd (span p t) == t
--
-- @since 0.1.2
span :: (Char -> Bool) -> ShortText -> (ShortText,ShortText)
span p st
  | Just ofs <- findOfs (not . p) st (B 0) = splitAtOfs ofs st
  | otherwise = (st,mempty)

-- | \(\mathcal{O}(n)\) Split 'ShortText' into longest suffix satisfying the given predicate and the preceding prefix.
--
-- >>> spanEnd (> 'c') "abcdabcd"
-- ("abcdabc","d")
--
-- prop> fst (spanEnd p t) <> snd (spanEnd p t) == t
--
-- @since 0.1.2
spanEnd :: (Char -> Bool) -> ShortText -> (ShortText,ShortText)
spanEnd p st
  | Just ofs <- findOfsRev (not . p) st (toB st) = splitAtOfs ofs st
  | otherwise = (mempty,st)

----------------------------------------------------------------------------

toCSize :: ShortText -> CSize
toCSize = fromIntegral . BSS.length . toShortByteString

toB :: ShortText -> B
toB = fromIntegral . BSS.length . toShortByteString

toLength :: ShortText -> Int
toLength st = I# (toLength# st)

toLength# :: ShortText -> Int#
toLength# st = GHC.Exts.sizeofByteArray# (toByteArray# st)

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
-- NOTE: impl below beats
--   toString = decodeStringShort' utf8 . toShortByteString
-- except for smallish strings
toString st = go 0
  where
    go !ofs
      | ofs >= sz  = []
      | otherwise  = let (c,ofs') = decodeCharAtOfs st ofs
                     in c `seq` ofs' `seq` (c : go ofs')

    !sz = toB st

----------------------------------------------------------------------------
-- Folds

-- | \(\mathcal{O}(n)\) Reduces the characters of the 'ShortText' with
-- the binary operator and an initial in forward direction (i.e. from
-- left to right).
--
-- >>> foldl (\_ _ -> True) False ""
-- False
--
-- >>> foldl (\s c -> c : s) ['.'] "abcd"
-- "dcba."
--
-- @since 0.1.2
foldl :: (a -> Char -> a) -> a -> ShortText -> a
foldl f z st = go 0 z
  where
    go !ofs acc
      | ofs >= sz  = acc
      | otherwise  = let (c,ofs') = decodeCharAtOfs st ofs
                     in c `seq` ofs' `seq` go ofs' (f acc c)

    !sz = toB st

-- | \(\mathcal{O}(n)\) Reduces the characters of the 'ShortText' with the binary operator.
--
-- >>> foldl1 max "abcdcba"
-- 'd'
--
-- >>> foldl1 const "abcd"
-- 'a'
--
-- >>> foldl1 (flip const) "abcd"
-- 'd'
--
-- __Note__: Will throw an 'error' exception if index is out of bounds.
--
-- @since 0.1.2
foldl1 :: (Char -> Char -> Char) -> ShortText -> Char
foldl1 f st
  | sz == 0    = error "foldl1: empty ShortText"
  | otherwise  = go c0sz c0
  where
    go !ofs acc
      | ofs >= sz  = acc
      | otherwise  = let (c,ofs') = decodeCharAtOfs st ofs
                     in c `seq` ofs' `seq` go ofs' (f acc c)
    !sz = toB st
    (c0,c0sz) = decodeCharAtOfs st (B 0)

-- | \(\mathcal{O}(n)\) Strict version of 'foldl'.
--
-- @since 0.1.2
foldl' :: (a -> Char -> a) -> a -> ShortText -> a
foldl' f !z st = go 0 z
  where
    go !ofs !acc
      | ofs >= sz  = acc
      | otherwise  = let (c,ofs') = decodeCharAtOfs st ofs
                     in c `seq` ofs' `seq` go ofs' (f acc c)

    !sz = toB st

-- | \(\mathcal{O}(n)\) Strict version of 'foldl1'.
--
-- @since 0.1.2
foldl1' :: (Char -> Char -> Char) -> ShortText -> Char
foldl1' f st
  | sz == 0    = error "foldl1: empty ShortText"
  | otherwise  = go c0sz c0
  where
    go !ofs !acc
      | ofs >= sz  = acc
      | otherwise  = let (c,ofs') = decodeCharAtOfs st ofs
                     in c `seq` ofs' `seq` go ofs' (f acc c)
    !sz = toB st
    (c0,c0sz) = decodeCharAtOfs st (B 0)

-- | \(\mathcal{O}(n)\) Reduces the characters of the 'ShortText' with
-- the binary operator and an initial in reverse direction (i.e. from
-- right to left).
--
-- >>> foldr (\_ _ -> True) False ""
-- False
--
-- >>> foldr (:) ['.'] "abcd"
-- "abcd."
--
-- @since 0.1.2
foldr :: (Char -> a -> a) -> a -> ShortText -> a
foldr f z st = go 0
  where
    go !ofs
      | ofs >= sz  = z
      | otherwise  = let (c,ofs') = decodeCharAtOfs st ofs
                     in c `seq` ofs' `seq` f c (go ofs')

    !sz = toB st

-- | \(\mathcal{O}(n)\) Reduces the characters of the 'ShortText' with the binary operator.
--
-- >>> foldr1 max "abcdcba"
-- 'd'
--
-- >>> foldr1 const "abcd"
-- 'a'
--
-- >>> foldr1 (flip const) "abcd"
-- 'd'
--
-- __Note__: Will throw an 'error' exception if index is out of bounds.
--
-- @since 0.1.2
foldr1 :: (Char -> Char -> Char) -> ShortText -> Char
foldr1 f st
  | sz == 0    = error "foldr1: empty ShortText"
  | otherwise  = go 0
  where
    go !ofs = let (c,ofs') = decodeCharAtOfs st ofs
              in c `seq` ofs' `seq`
                 (if ofs' >= sz then c else f c (go ofs'))

    !sz = toB st

-- | \(\mathcal{O}(n)\) Convert to 'T.Text'
--
-- prop> (fromText . toText) t == t
--
-- prop> (toText . fromText) t == t
--
-- This is \(\mathcal{O}(1)\) with @text-2@.
-- Previously it wasn't because 'T.Text' used UTF-16 as its internal representation.
--
-- @since 0.1
toText :: ShortText -> T.Text
#if MIN_VERSION_text(2,0,0)
toText (ShortText (BSSI.SBS ba)) = TI.Text (TA.ByteArray ba) 0 (I# (GHC.Exts.sizeofByteArray# ba))
#else
toText = T.decodeUtf8 . toByteString
#endif

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
fromString s = case s of
  []  -> mempty
  [c] -> singleton $ r c
  _   -> ShortText . encodeStringShort utf8 . map r $ s
  where
    r c | isSurr (ord c) = '\xFFFD'
        | otherwise      = c

-- | \(\mathcal{O}(n)\) Construct 'ShortText' from 'T.Text'
--
-- This is \(\mathcal{O}(1)\) with @text-2@ when the 'T.Text' is not sliced.
-- Previously it wasn't because 'T.Text' used UTF-16 as its internal representation.
--
-- @since 0.1
fromText :: T.Text -> ShortText
#if MIN_VERSION_text(2,0,0)
fromText (TI.Text (TA.ByteArray ba) off len) =
    ShortText (BSSI.SBS (case sliceByteArray (TA.ByteArray ba) off len of TA.ByteArray ba' -> ba'))

sliceByteArray :: TA.Array -> Int -> Int -> TA.Array
sliceByteArray ta@(TA.ByteArray ba) 0 len
    | len == I# (GHC.Exts.sizeofByteArray# ba)
    = ta
sliceByteArray ta off len = TA.run $ do
    ma <- TA.new len
    TA.copyI len ma 0 ta off
    return ma

#else
fromText = fromByteStringUnsafe . T.encodeUtf8
#endif

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

type CCodePoint = Word

foreign import ccall unsafe "hs_text_short_is_valid_utf8" c_text_short_is_valid_utf8 :: ByteArray# -> CSize -> IO CInt

foreign import ccall unsafe "hs_text_short_index_cp" c_text_short_index :: ByteArray# -> CSize -> CSize -> IO CCodePoint

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
  | i < 0      = Nothing
  | otherwise  = cp2chSafe cp
  where
    cp = CP $ unsafeDupablePerformIO (c_text_short_index (toByteArray# st) (toCSize st) (fromIntegral i))

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
  | i < 0      = Nothing
  | otherwise  = cp2chSafe cp
  where
    cp = CP $ unsafeDupablePerformIO (c_text_short_index_rev (toByteArray# st) (toCSize st) (fromIntegral i))

foreign import ccall unsafe "hs_text_short_index_cp_rev" c_text_short_index_rev :: ByteArray# -> CSize -> CSize -> IO CCodePoint


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
  | otherwise = splitAtOfs ofs st
  where
    ofs   = csizeToB $
            unsafeDupablePerformIO (c_text_short_index_ofs (toByteArray# st) stsz (fromIntegral i))
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
  | otherwise   = splitAtOfs ofs st
  where
    ofs   = csizeToB $
            unsafeDupablePerformIO (c_text_short_index_ofs_rev (toByteArray# st) (toCSize st) (fromIntegral (i-1)))
    stsz  = toB st

{-# INLINE splitAtOfs #-}
splitAtOfs :: B -> ShortText -> (ShortText,ShortText)
splitAtOfs ofs st
  | ofs  == 0    = (mempty,st)
  | ofs  >= stsz = (st,mempty)
  | otherwise    = (slice st 0 ofs, slice st ofs (stsz-ofs))
  where
    !stsz  = toB st

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
    c0  = cp2ch cp0
    cp0 = readCodePoint st 0
    ofs = cpLen cp0
    len2 = toB st - ofs

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
    c0  = cp2ch cp0
    cp0 = readCodePointRev st stsz
    stsz = toB st
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
  | isPrefixOf pfx t = Just $! snd (splitAtOfs (toB pfx) t)
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
  | isSuffixOf sfx t = Just $! fst (splitAtOfs pfxLen t)
  | otherwise        = Nothing
  where
    pfxLen = toB t - toB sfx

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
      let !cp0 = readCodePoint st 0
          !cp0sz = cpLen cp0
      writeCodePointN cp0sz mba 0 cp0
      go mba (sn - 1) cp0sz cp0sz
  where
    newsz = ssz + ((sn-1) `mulB` csz)
    ssz = toB st
    sn  = length st
    csz = cpLen cp
    cp  = ch2cp c

    go :: MBA s -> Int -> B -> B -> ST s ()
    go _   0 !_  !_   = return ()
    go mba n ofs ofs2 = do
      let !cp1 = readCodePoint st ofs2
          !cp1sz = cpLen cp1
      writeCodePointN csz   mba ofs cp
      writeCodePointN cp1sz mba (ofs+csz) cp1
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
-- prop> intercalate "" ts == concat ts
--
-- @since 0.1.2
intercalate :: ShortText -> [ShortText] -> ShortText
intercalate _ []  = mempty
intercalate _ [t] = t
intercalate sep ts
  | null sep   = mconcat ts
  | otherwise  = mconcat (List.intersperse sep ts)

-- | \(\mathcal{O}(n*m)\) Replicate a 'ShortText'.
--
-- A repetition count smaller than 1 results in an empty string result.
--
-- >>> replicate 3 "jobs!"
-- "jobs!jobs!jobs!"
--
-- >>> replicate 10000 ""
-- ""
--
-- >>> replicate 0 "nothing"
-- ""
--
-- prop> length (replicate n t) == max 0 n * length t
--
-- @since 0.1.2
replicate :: Int -> ShortText -> ShortText
replicate n0 t
  | n0 < 1     = mempty
  | null t    = mempty
  | otherwise = create (n0 `mulB` sz) (go 0)
  where
    go :: Int -> MBA s -> ST s ()
    go j mba
      | j == n0    = return ()
      | otherwise  = do
          copyByteArray t 0 mba (j `mulB` sz) sz
          go (j+1) mba

    sz = toB t

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
    sz = toB st
    sn = length st

    go :: Int -> B -> MBA s -> ST s ()
    go 0 !_  _   = return ()
    go i ofs mba = do
      let !cp   = readCodePoint st ofs
          !cpsz = cpLen cp
          !ofs' = ofs+cpsz
      writeCodePointN cpsz mba (sz - ofs') cp
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
filter p t
  = case (mofs1,mofs2) of
      (Nothing,   _)       -> t -- no non-accepted characters found
      (Just 0,    Nothing) -> mempty -- no accepted characters found
      (Just ofs1, Nothing) -> slice t 0 ofs1 -- only prefix accepted
      (Just ofs1, Just ofs2) -> createShrink (t0sz-(ofs2-ofs1)) $ \mba -> do
        -- copy accepted prefix
        copyByteArray t 0 mba 0 ofs1
        -- [ofs1 .. ofs2) are a non-accepted region
        -- filter rest after ofs2
        t1sz <- go mba ofs2 ofs1
        return t1sz
  where
    mofs1 = findOfs (not . p) t (B 0) -- first non-accepted Char
    mofs2 = findOfs p t (fromMaybe (B 0) mofs1) -- first accepted Char

    t0sz = toB t

    go :: MBA s -> B -> B -> ST s B
    go mba !t0ofs !t1ofs
      | t0ofs >= t0sz = return t1ofs
      | otherwise = let !cp = readCodePoint t t0ofs
                        !cpsz = cpLen cp
                    in if p (cp2ch cp)
                       then writeCodePointN cpsz mba t1ofs cp >>
                            go mba (t0ofs+cpsz) (t1ofs+cpsz)
                       else go mba (t0ofs+cpsz) t1ofs -- skip code-point

-- | \(\mathcal{O}(n)\) Strip characters from the beginning end and of 'ShortText' which satisfy given predicate.
--
-- >>> dropAround (== ' ') "   white   space   "
-- "white   space"
--
-- >>> dropAround (> 'a') "bcdefghi"
-- ""
--
-- @since 0.1.2
dropAround :: (Char -> Bool) -> ShortText -> ShortText
dropAround p t0 = case (mofs1,mofs2) of
                    (Nothing,_) -> mempty
                    (Just ofs1,Just ofs2)
                      | ofs1 == 0, ofs2 == t0sz  -> t0
                      | ofs1 < ofs2  -> create (ofs2-ofs1) $ \mba -> do
                          copyByteArray t0 ofs1 mba (B 0) (ofs2-ofs1)
                    (_,_) -> error "dropAround: the impossible happened"
  where
    mofs1 = findOfs    (not . p) t0 (B 0)
    mofs2 = findOfsRev (not . p) t0 t0sz
    t0sz = toB t0

----------------------------------------------------------------------------

-- | Construct a new 'ShortText' from an existing one by slicing
--
-- NB: The 'CSize' arguments refer to byte-offsets
slice :: ShortText -> B -> B -> ShortText
slice st ofs len
  | ofs < 0    = error "invalid offset"
  | len < 0    = error "invalid length"
  | len' == 0  = mempty
  | otherwise  = create len' $ \mba -> copyByteArray st ofs' mba 0 len'
  where
    len0 = toB st
    len' = max 0 (min len (len0-ofs))
    ofs' = max 0 ofs

----------------------------------------------------------------------------
-- low-level MutableByteArray# helpers

-- | Byte offset (or size) in bytes
--
-- This currently wraps an 'Int' because this is what GHC's primops
-- currently use for byte offsets/sizes.
newtype B = B { unB :: Int }
          deriving (Ord,Eq,Num)

{- TODO: introduce operators for 'B' to avoid 'Num' -}

mulB :: Int -> B -> B
mulB n (B b) = B (n*b)

csizeFromB :: B -> CSize
csizeFromB = fromIntegral . unB

csizeToB :: CSize -> B
csizeToB = B . fromIntegral

data MBA s = MBA# { unMBA# :: MutableByteArray# s }

{-# INLINE create #-}
create :: B -> (forall s. MBA s -> ST s ()) -> ShortText
create n go = runST $ do
  mba <- newByteArray n
  go mba
  unsafeFreeze mba

{-# INLINE createShrink #-}
createShrink :: B -> (forall s. MBA s -> ST s B) -> ShortText
createShrink n go = runST $ do
  mba <- newByteArray n
  n' <- go mba
  if n' < n
    then unsafeFreezeShrink mba n'
    else unsafeFreeze mba

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: MBA s -> ST s ShortText
unsafeFreeze (MBA# mba#)
  = ST $ \s -> case GHC.Exts.unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', ShortText (BSSI.SBS ba#) #)

{-# INLINE copyByteArray #-}
copyByteArray :: ShortText -> B -> MBA s -> B -> B -> ST s ()
copyByteArray (ShortText (BSSI.SBS src#)) (B (I# src_off#)) (MBA# dst#) (B (I# dst_off#)) (B (I# len#))
  = ST $ \s -> case GHC.Exts.copyByteArray# src# src_off# dst# dst_off# len# s of
                 s' -> (# s', () #)

{-# INLINE newByteArray #-}
newByteArray :: B -> ST s (MBA s)
newByteArray (B (I# n#))
  = ST $ \s -> case GHC.Exts.newByteArray# n# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

{-# INLINE writeWord8Array #-}
writeWord8Array :: MBA s -> B -> Word -> ST s ()
writeWord8Array (MBA# mba#) (B (I# i#)) (W# w#)
  = ST $ \s ->
#if __GLASGOW_HASKELL__ >= 902
      case GHC.Exts.writeWord8Array# mba# i# (GHC.Exts.wordToWord8# w#) s of
#else
      case GHC.Exts.writeWord8Array# mba# i# w# s of
#endif
                 s' -> (# s', () #)
{- not needed yet
{-# INLINE indexWord8Array #-}
indexWord8Array :: ShortText -> B -> Word
indexWord8Array (ShortText (BSSI.SBS ba#)) (B (I# i#)) = W# (GHC.Exts.indexWord8Array# ba# i#)
-}

{-# INLINE copyAddrToByteArray #-}
copyAddrToByteArray :: Ptr a -> MBA RealWorld -> B -> B -> ST RealWorld ()
copyAddrToByteArray (Ptr src#) (MBA# dst#) (B (I# dst_off#)) (B (I# len#))
  = ST $ \s -> case GHC.Exts.copyAddrToByteArray# src# dst# dst_off# len# s of
                 s' -> (# s', () #)

----------------------------------------------------------------------------
-- unsafeFreezeShrink

#if __GLASGOW_HASKELL__ >= 710
-- for GHC versions which have the 'shrinkMutableByteArray#' primop
{-# INLINE unsafeFreezeShrink #-}
unsafeFreezeShrink :: MBA s -> B -> ST s ShortText
unsafeFreezeShrink mba n = do
  shrink mba n
  unsafeFreeze mba

{-# INLINE shrink #-}
shrink :: MBA s -> B -> ST s ()
shrink (MBA# mba#) (B (I# i#))
  = ST $ \s -> case GHC.Exts.shrinkMutableByteArray# mba# i# s of
                 s' -> (# s', () #)
#else
-- legacy code for GHC versions which lack `shrinkMutableByteArray#` primop
{-# INLINE unsafeFreezeShrink #-}
unsafeFreezeShrink :: MBA s -> B -> ST s ShortText
unsafeFreezeShrink mba0 n = do
  mba' <- newByteArray n
  copyByteArray2 mba0 0 mba' 0 n
  unsafeFreeze mba'

{-# INLINE copyByteArray2 #-}
copyByteArray2 :: MBA s -> B -> MBA s -> B -> B -> ST s ()
copyByteArray2 (MBA# src#) (B (I# src_off#)) (MBA# dst#) (B (I# dst_off#)) (B( I# len#))
  = ST $ \s -> case GHC.Exts.copyMutableByteArray# src# src_off# dst# dst_off# len# s of
                 s' -> (# s', () #)
#endif

----------------------------------------------------------------------------
-- Helpers for encoding code points into UTF-8 code units
--
--   7 bits| <    0x80 | 0xxxxxxx
--  11 bits| <   0x800 | 110yyyyx  10xxxxxx
--  16 bits| < 0x10000 | 1110yyyy  10yxxxxx  10xxxxxx
--  21 bits|           | 11110yyy  10yyxxxx  10xxxxxx  10xxxxxx

-- | Unicode Code-point
--
-- Keeping it as a 'Word' is more convenient for bit-ops and FFI
newtype CP = CP Word

{-# INLINE ch2cp #-}
ch2cp :: Char -> CP
ch2cp (ord -> ci)
  | isSurr ci  = CP 0xFFFD
  | otherwise  = CP (fromIntegral ci)

{-# INLINE isSurr #-}
isSurr :: (Num i, Bits i) => i -> Bool
isSurr ci = ci .&. 0xfff800 == 0xd800

{-# INLINE cp2ch #-}
cp2ch :: CP -> Char
cp2ch (CP w) = (w < 0x110000) `assert` unsafeChr (fromIntegral w)

-- used/needed by index-lookup functions to encode out of bounds
cp2chSafe :: CP -> Maybe Char
cp2chSafe cp
  | cpNull cp = Nothing
  | otherwise = Just $! cp2ch cp
  where
    cpNull :: CP -> Bool
    cpNull (CP w) = w >= 0x110000

{-# INLINE cpLen #-}
cpLen :: CP -> B
cpLen (CP cp)
  | cp <    0x80  = B 1
  | cp <   0x800  = B 2
  | cp < 0x10000  = B 3
  | otherwise     = B 4

-- convenience wrapper; unsafe like readCodePoint
{-# INLINE decodeCharAtOfs #-}
decodeCharAtOfs :: ShortText -> B -> (Char,B)
decodeCharAtOfs st ofs = (c,ofs')
  where
    c    = cp2ch cp
    ofs' = ofs + cpLen cp
    cp   = readCodePoint st ofs
{- pure version of decodeCharAtOfs, but unfortunately significantly slower

decodeCharAtOfs st ofs
  | b0 < 0x80 = (cp2ch $ CP b0,ofs + B 1)
  | otherwise = case b0 `unsafeShiftR` 4 of
                  0xf -> (cp2ch $ CP go4, ofs + B 4)
                  0xe -> (cp2ch $ CP go3, ofs + B 3)
                  _   -> (cp2ch $ CP go2, ofs + B 2)
  where
    b0    = buf 0
    buf j = indexWord8Array st (ofs+j)

    go2 =     ((b0    .&. 0x1f) `unsafeShiftL` 6)
          .|.  (buf 1 .&. 0x3f)

    go3 =     ((b0    .&. 0x0f) `unsafeShiftL` (6+6))
          .|. ((buf 1 .&. 0x3f) `unsafeShiftL` 6)
          .|.  (buf 2 .&. 0x3f)

    go4 =     ((b0    .&. 0x07) `unsafeShiftL` (6+6+6))
          .|. ((buf 1 .&. 0x3f) `unsafeShiftL` (6+6))
          .|. ((buf 2 .&. 0x3f) `unsafeShiftL` 6)
          .|.  (buf 3 .&. 0x3f)
-}


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

singleton' :: CP -> ShortText
singleton' cp@(CP cpw)
  | cpw <    0x80  = create 1 $ \mba -> writeCodePoint1 mba 0 cp
  | cpw <   0x800  = create 2 $ \mba -> writeCodePoint2 mba 0 cp
  | cpw < 0x10000  = create 3 $ \mba -> writeCodePoint3 mba 0 cp
  | otherwise      = create 4 $ \mba -> writeCodePoint4 mba 0 cp

-- | \(\mathcal{O}(n)\) Prepend a character to a 'ShortText'.
--
-- prop> cons c t == singleton c <> t
--
-- @since 0.1.2
cons :: Char -> ShortText -> ShortText
cons (ch2cp -> cp@(CP cpw)) sfx
  | n == 0         = singleton' cp
  | cpw <    0x80  = create (n+1) $ \mba -> writeCodePoint1 mba 0 cp >> copySfx 1 mba
  | cpw <   0x800  = create (n+2) $ \mba -> writeCodePoint2 mba 0 cp >> copySfx 2 mba
  | cpw < 0x10000  = create (n+3) $ \mba -> writeCodePoint3 mba 0 cp >> copySfx 3 mba
  | otherwise      = create (n+4) $ \mba -> writeCodePoint4 mba 0 cp >> copySfx 4 mba
  where
    !n = toB sfx

    copySfx :: B -> MBA s -> ST s ()
    copySfx ofs mba = copyByteArray sfx 0 mba ofs n

-- | \(\mathcal{O}(n)\) Append a character to the ond of a 'ShortText'.
--
-- prop> snoc t c == t <> singleton c
--
-- @since 0.1.2
snoc :: ShortText -> Char -> ShortText
snoc pfx (ch2cp -> cp@(CP cpw))
  | n == 0         = singleton' cp
  | cpw <    0x80  = create (n+1) $ \mba -> copyPfx mba >> writeCodePoint1 mba n cp
  | cpw <   0x800  = create (n+2) $ \mba -> copyPfx mba >> writeCodePoint2 mba n cp
  | cpw < 0x10000  = create (n+3) $ \mba -> copyPfx mba >> writeCodePoint3 mba n cp
  | otherwise      = create (n+4) $ \mba -> copyPfx mba >> writeCodePoint4 mba n cp
  where
    !n = toB pfx

    copyPfx :: MBA s -> ST s ()
    copyPfx mba = copyByteArray pfx 0 mba 0 n

{-
writeCodePoint :: MBA s -> Int -> Word -> ST s ()
writeCodePoint mba ofs cp
  | cp <    0x80  = writeCodePoint1 mba ofs cp
  | cp <   0x800  = writeCodePoint2 mba ofs cp
  | cp < 0x10000  = writeCodePoint3 mba ofs cp
  | otherwise     = writeCodePoint4 mba ofs cp
-}

writeCodePointN :: B -> MBA s -> B -> CP -> ST s ()
writeCodePointN 1 = writeCodePoint1
writeCodePointN 2 = writeCodePoint2
writeCodePointN 3 = writeCodePoint3
writeCodePointN 4 = writeCodePoint4
writeCodePointN _ = undefined

writeCodePoint1 :: MBA s -> B -> CP -> ST s ()
writeCodePoint1 mba ofs (CP cp) =
  writeWord8Array mba ofs cp

writeCodePoint2 :: MBA s -> B -> CP -> ST s ()
writeCodePoint2 mba ofs (CP cp) = do
  writeWord8Array mba  ofs    (0xc0 .|. (cp `unsafeShiftR` 6))
  writeWord8Array mba (ofs+1) (0x80 .|. (cp                     .&. 0x3f))

writeCodePoint3 :: MBA s -> B -> CP -> ST s ()
writeCodePoint3 mba ofs (CP cp) = do
  writeWord8Array mba  ofs    (0xe0 .|.  (cp `unsafeShiftR` 12))
  writeWord8Array mba (ofs+1) (0x80 .|. ((cp `unsafeShiftR` 6)  .&. 0x3f))
  writeWord8Array mba (ofs+2) (0x80 .|.  (cp                    .&. 0x3f))

writeCodePoint4 :: MBA s -> B -> CP -> ST s ()
writeCodePoint4 mba ofs (CP cp) = do
  writeWord8Array mba  ofs    (0xf0 .|.  (cp `unsafeShiftR` 18))
  writeWord8Array mba (ofs+1) (0x80 .|. ((cp `unsafeShiftR` 12) .&. 0x3f))
  writeWord8Array mba (ofs+2) (0x80 .|. ((cp `unsafeShiftR` 6)  .&. 0x3f))
  writeWord8Array mba (ofs+3) (0x80 .|. (cp                     .&. 0x3f))

-- beware: UNSAFE!
readCodePoint :: ShortText -> B -> CP
readCodePoint st (csizeFromB -> ofs)
  = CP $ unsafeDupablePerformIO (c_text_short_ofs_cp (toByteArray# st) ofs)

foreign import ccall unsafe "hs_text_short_ofs_cp" c_text_short_ofs_cp :: ByteArray# -> CSize -> IO CCodePoint

readCodePointRev :: ShortText -> B -> CP
readCodePointRev st (csizeFromB -> ofs)
  = CP $ unsafeDupablePerformIO (c_text_short_ofs_cp_rev (toByteArray# st) ofs)

foreign import ccall unsafe "hs_text_short_ofs_cp_rev" c_text_short_ofs_cp_rev :: ByteArray# -> CSize -> IO CCodePoint

----------------------------------------------------------------------------
-- string & list literals

-- | __Note__: Surrogate pairs (@[U+D800 .. U+DFFF]@) character literals are replaced by U+FFFD.
--
-- @since 0.1.2
instance GHC.Exts.IsList ShortText where
    type (Item ShortText) = Char
    fromList = fromString
    toList   = toString

-- | __Note__: Surrogate pairs (@[U+D800 .. U+DFFF]@) in string literals are replaced by U+FFFD.
--
-- This matches the behaviour of 'S.IsString' instance for 'T.Text'.
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
  sz <- csizeToB `fmap` c_strlen ptr

  case sz `compare` 0 of
    EQ -> return mempty -- should not happen if rules fire correctly
    GT -> stToIO $ do
      mba <- newByteArray sz
      copyAddrToByteArray ptr mba 0 sz
      unsafeFreeze mba
    LT -> return (error "fromLitAsciiAddr#")
          -- NOTE: should never happen unless strlen(3) overflows (NB: CSize
          -- is unsigned; the overflow would occur when converting to
          -- 'B')

foreign import ccall unsafe "strlen" c_strlen :: CString -> IO CSize

-- GHC uses an encoding resembling Modified UTF-8 for non-ASCII string-literals
{-# NOINLINE fromLitMUtf8Addr# #-}
fromLitMUtf8Addr# :: Addr# -> ShortText
fromLitMUtf8Addr# (Ptr -> ptr) = unsafeDupablePerformIO $ do
  sz <- B `fmap` c_text_short_mutf8_strlen ptr

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
-- >>> import Data.Text.Short (pack, unpack, concat)
-- >>> import Text.Show.Functions ()
-- >>> import qualified Test.QuickCheck.Arbitrary as QC
-- >>> import Test.QuickCheck.Instances ()
-- >>> instance QC.Arbitrary ShortText where { arbitrary = fmap fromString QC.arbitrary }
