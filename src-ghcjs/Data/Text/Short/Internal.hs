{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

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
    ) where

import           Control.DeepSeq                (NFData)
import           Data.Bifunctor
import           Data.Binary
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Builder        as BB
import           Data.ByteString.Short          (ShortByteString)
import qualified Data.ByteString.Short          as BSS
import qualified Data.Char                      as Char
import           Data.Semigroup
import           Data.Hashable                  (Hashable(..))
import           Data.Maybe
import qualified Data.String                    as S
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified GHC.Exts                       (IsList(..))
import           Prelude                        hiding (all, any, break, concat,
                                                 drop, dropWhile, filter, foldl,
                                                 foldl1, foldr, foldr1, head,
                                                 init, last, length, null,
                                                 replicate, reverse, span,
                                                 splitAt, tail, take, takeWhile)

import qualified Data.JSString                  as JSS
import qualified Data.JSString.Text             as JSS
import qualified Data.JSString.Raw              as JSS
import           Text.Printf                    (PrintfArg)

-- | A newtype around 'JSString'.
newtype ShortText = ShortText { toJSString :: JSS.JSString }
                  deriving (Eq, Ord, Show, Read, PrintfArg, 
                            Monoid, Data.Semigroup.Semigroup, NFData)

instance Hashable ShortText where
  hashWithSalt salt = hashWithSalt salt . toString

instance Binary ShortText where
    put = put . toByteString
    get = fromByteStringUnsafe <$> get

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
null = JSS.null . toJSString

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
length = JSS.rawLength . toJSString

-- | \(\mathcal{O}(n)\) Test whether 'ShortText' contains only ASCII code-points (i.e. only U+0000 through U+007F).
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
isAscii = all Char.isAscii

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
all p = JSS.all p . toJSString

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
find p = JSS.find p . toJSString

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
findIndex p = JSS.findIndex p . toJSString

-- | \(\mathcal{O}(n)\) Split 'ShortText' into longest prefix satisfying the given predicate and the remaining suffix.
--
-- >>> span (< 'c') "abcdabcd"
-- ("ab","cdabcd")
--
-- prop> fst (span p t) <> snd (span p t) == t
--
-- @since 0.1.2
span :: (Char -> Bool) -> ShortText -> (ShortText,ShortText)
span p = bimap ShortText ShortText . JSS.span p . toJSString

-- | \(\mathcal{O}(n)\) Split 'ShortText' into longest suffix satisfying the given predicate and the preceding prefix.
--
-- >>> spanEnd (> 'c') "abcdabcd"
-- ("abcdabc","d")
--
-- prop> fst (spanEnd p t) <> snd (spanEnd p t) == t
--
-- @since 0.1.2
spanEnd :: (Char -> Bool) -> ShortText -> (ShortText,ShortText)
spanEnd p st = bimap reverse reverse $ span p (reverse st)

-- | \(\mathcal{O}(0)\) Converts to UTF-8 encoded 'ShortByteString'
--
-- This operation has effectively no overhead, as it's currently merely a @newtype@-cast.
--
-- @since 0.1
toShortByteString :: ShortText -> ShortByteString
toShortByteString = BSS.toShort . toByteString

-- | \(\mathcal{O}(n)\) Converts to UTF-8 encoded 'BS.ByteString'
--
-- @since 0.1
toByteString :: ShortText -> BS.ByteString
toByteString = T.encodeUtf8 . toText

-- | Construct a 'BB.Builder' that encodes 'ShortText' as UTF-8.
--
-- @since 0.1
toBuilder :: ShortText -> BB.Builder
toBuilder = BB.byteString . toByteString

-- | \(\mathcal{O}(n)\) Convert to 'String'
--
-- prop> (fromString . toString) t == t
--
-- __Note__: See documentation of 'fromString' for why @('toString' . 'fromString')@ is not an identity function.
--
-- @since 0.1
toString :: ShortText -> String
toString = JSS.unpack . toJSString

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
foldl f z = JSS.foldl f z . toJSString

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
foldl1 f = JSS.foldl1 f . toJSString

-- | \(\mathcal{O}(n)\) Strict version of 'foldl'.
--
-- @since 0.1.2
foldl' :: (a -> Char -> a) -> a -> ShortText -> a
foldl' f !z = JSS.foldl' f z . toJSString

-- | \(\mathcal{O}(n)\) Strict version of 'foldl1'.
--
-- @since 0.1.2
foldl1' :: (Char -> Char -> Char) -> ShortText -> Char
foldl1' f = JSS.foldl1' f . toJSString

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
foldr f z = JSS.foldr f z . toJSString

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
foldr1 f = JSS.foldr1 f . toJSString

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
toText = JSS.textFromJSString . toJSString

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
fromString = ShortText . JSS.pack

-- | \(\mathcal{O}(n)\) Construct 'ShortText' from 'T.Text'
--
-- This is currently not \(\mathcal{O}(1)\) because currently 'T.Text' uses UTF-16 as its internal representation.
-- In the event that 'T.Text' will change its internal representation to UTF-8 this operation will become \(\mathcal{O}(1)\).
--
-- @since 0.1
fromText :: T.Text -> ShortText
fromText = ShortText . JSS.textToJSString

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
fromShortByteString = fromByteString . BSS.fromShort

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
fromShortByteStringUnsafe =
    fromMaybe err . fromShortByteString
  where err = error "Data.Text.Short.fromShortByteStringUnsafe: invalid encoding"

-- | \(\mathcal{O}(n)\) Construct 'ShortText' from UTF-8 encoded 'BS.ByteString'
--
-- 'fromByteString' accepts (or rejects) the same input data as 'fromShortByteString'.
--
-- Returns 'Nothing' in case of invalid UTF-8 encoding.
--
-- @since 0.1
fromByteString :: BS.ByteString -> Maybe ShortText
fromByteString = either (const Nothing) (Just . fromText) . T.decodeUtf8'

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
fromByteStringUnsafe = 
    fromMaybe err . fromByteString
  where err = error "Data.Text.Short.fromByteStringUnsafe: invalid encoding"

----------------------------------------------------------------------------

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
  | i < 0          = Nothing
  | i >= length st = Nothing
  | otherwise      = Just $ JSS.index (toJSString st) i

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
  | i >= len   = Nothing
  | otherwise  = Just $ JSS.index (toJSString st) (len - i)
  where
    len = length st

-- | \(\mathcal{O}(n)\) Split 'ShortText' into two halves.
--
-- @'splitAtOfs n t@ returns a pair of 'ShortText' with the following properties:
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
splitAt i = bimap ShortText ShortText . JSS.splitAt i . toJSString

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
splitAtEnd i st = bimap ShortText ShortText $ JSS.splitAt (len - i) $ toJSString st
  where
    len = length st

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
uncons = fmap (bimap id ShortText) . JSS.uncons . toJSString

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
unsnoc = fmap (bimap ShortText id) . JSS.unsnoc . toJSString

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
isPrefixOf x y = JSS.isPrefixOf (toJSString x) (toJSString y)

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
stripPrefix pfx t = fmap ShortText $ JSS.stripPrefix (toJSString pfx) (toJSString t)

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
isSuffixOf x y = JSS.isSuffixOf (toJSString x) (toJSString y)

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
stripSuffix sfx t = fmap ShortText $ JSS.stripSuffix (toJSString sfx) (toJSString t)

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
intersperse c = ShortText . JSS.intersperse c . toJSString

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
intercalate t = ShortText . JSS.intercalate (toJSString t) . map toJSString

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
replicate n0 = ShortText . JSS.replicate n0 . toJSString

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
reverse = ShortText . JSS.reverse . toJSString


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
filter p = ShortText . JSS.filter p . toJSString

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
dropAround p = ShortText . JSS.dropAround p . toJSString

----------------------------------------------------------------------------

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
singleton = ShortText . JSS.singleton

-- | \(\mathcal{O}(n)\) Prepend a character to a 'ShortText'.
--
-- prop> cons c t == singleton c <> t
--
-- @since 0.1.2
cons :: Char -> ShortText -> ShortText
cons c = ShortText . JSS.cons c . toJSString

-- | \(\mathcal{O}(n)\) Append a character to the ond of a 'ShortText'.
--
-- prop> snoc t c == t <> singleton c
--
-- @since 0.1.2
snoc :: ShortText -> Char -> ShortText
snoc pfx c = ShortText $ JSS.snoc (toJSString pfx) c


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
-- This matches the behaviour of 'IsString' instance for 'T.Text'.
instance S.IsString ShortText where
    fromString = fromStringLit

-- i.e., don't inline before Phase 0
{-# INLINE [0] fromStringLit #-}
fromStringLit :: String -> ShortText
fromStringLit = fromString

empty :: ShortText
empty = ShortText JSS.empty

{-# RULES "ShortText empty literal" fromStringLit "" = empty #-}

-- TODO: this doesn't seem to fire
{-# RULES "ShortText singleton literal" forall c . fromStringLit [c] = singleton c #-}

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text.Short (pack, unpack, concat)
-- >>> import Text.Show.Functions ()
-- >>> import qualified Test.QuickCheck.Arbitrary as QC
-- >>> import Test.QuickCheck.Instances ()
-- >>> instance QC.Arbitrary ShortText where { arbitrary = fmap fromString QC.arbitrary }
