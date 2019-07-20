{-# LANGUAGE Trustworthy #-}
-- |
-- Module      : Data.Text.Short
-- Copyright   : © Herbert Valerio Riedel 2017
-- License     : BSD3
--
-- Maintainer  : hvr@gnu.org
-- Stability   : stable
--
-- Memory-efficient representation of Unicode text strings.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.Text.Short as TS
-- > import qualified Data.Text.Short (ShortText)
--
-- This modules deliberately omits (common) partial functions, which
-- can be found in "Data.Text.Short.Partial" instead.
--
-- @since 0.1
module Data.Text.Short
    ( -- * The 'ShortText' type
      ShortText

      -- * Basic operations
      -- ** Construction
    , empty
    , singleton
    , pack
    , append
    , concat
    , cons
    , snoc
    , replicate

      -- ** Deconstruction
    , unpack
    , uncons
    , unsnoc

      -- ** Querying & predicates
    , null
    , length
    , isAscii
    , all
    , any
    , find
    , isPrefixOf
    , isSuffixOf

      -- ** Lookup & indexing
    , (!?)
    , indexMaybe
    , indexEndMaybe
    , findIndex

      -- * Splitting 'ShortText's
      -- ** Basic functions
    , take
    , takeEnd
    , drop
    , dropEnd
    , takeWhile
    , takeWhileEnd
    , dropWhile
    , dropWhileEnd

    , dropAround

      -- ** Pair-valued functions
    , splitAt
    , splitAtEnd
    , span
    , break
    , spanEnd
    , breakEnd

      -- ** Breaking into many substrings
    , split

      -- ** Suffix & Prefix operations
    , stripPrefix
    , stripSuffix

      -- * Transformations
    , intersperse
    , intercalate
    , reverse
    , filter

      -- * Folds
    , foldl
    , foldl'
    , foldr

      -- * Conversions
      -- ** 'String'
    , fromString
    , toString

      -- ** 'Text'
    , fromText
    , toText

      -- ** 'ByteString'
    , fromShortByteString
    , toShortByteString

    , fromByteString
    , toByteString

    , toBuilder

    ) where

import           Data.Semigroup
import           Data.Text.Short.Internal
import           Prelude                  ()

-- | \(\mathcal{O}(n)\) Variant of 'span' with negated predicate.
--
-- >>> break (> 'c') "abcdabcd"
-- ("abc","dabcd")
--
-- prop> break p t == span (not . p) t
--
-- prop> fst (break p t) <> snd (break p t) == t
--
-- @since 0.1.2
break :: (Char -> Bool) -> ShortText -> (ShortText,ShortText)
break p st = span (not . p) st

-- | \(\mathcal{O}(n)\) Variant of 'spanEnd' with negated predicate.
--
-- >>> breakEnd (< 'c') "abcdabcd"
-- ("abcdab","cd")
--
-- prop> breakEnd p t == spanEnd (not . p) t
--
-- prop> fst (breakEnd p t) <> snd (breakEnd p t) == t
--
-- @since 0.1.2
breakEnd :: (Char -> Bool) -> ShortText -> (ShortText,ShortText)
breakEnd p st = spanEnd (not . p) st

-- | \(\mathcal{O}(n)\) Index /i/-th code-point in 'ShortText'.
--
-- Infix operator alias of 'indexMaybe'
--
-- >>> "abcdefg" !? 2
-- Just 'c'
--
-- @since 0.1.2
(!?) :: ShortText -> Int -> Maybe Char
(!?) = indexMaybe

-- | \(\mathcal{O}(n)\) Test whether /any/ code points in 'ShortText' satisfy a predicate.
--
-- >>> any (> 'c') "abcdabcd"
-- True
--
-- >>> any (const True) ""
-- False
--
-- >>> any (== 'c') "abdabd"
-- False
--
-- prop> any p t == not (all (not . p) t)
--
-- @since 0.1.2
any :: (Char -> Bool) -> ShortText -> Bool
any p st = case find p st of
             Nothing -> False
             Just _  -> True

-- | \(\mathcal{O}(n)\) Concatenate two 'ShortText's
--
-- This is a type-specialised alias of '<>'.
--
-- >>> append "foo" "bar"
-- "foobar"
--
-- prop> length (append t1 t2) == length t1 + length t2
--
-- @since 0.1.2
append :: ShortText -> ShortText -> ShortText
append = (<>)

-- | \(\mathcal{O}(n)\) Concatenate list of 'ShortText's
--
-- This is a type-specialised alias of 'mconcat'.
--
-- >>> concat []
-- ""
--
-- >>> concat ["foo","bar","doo"]
-- "foobardoo"
--
-- @since 0.1.2
concat :: [ShortText] -> ShortText
concat = mconcat

-- | \(\mathcal{O}(0)\) The /empty/ 'ShortText'.
--
-- This is a type-specialised alias of 'mempty'.
--
-- >>> empty
-- ""
--
-- >>> null empty
-- True
--
-- @since 0.1.2
empty :: ShortText
empty = mempty

-- | \(\mathcal{O}(n)\) Construct a 'ShortText' from a list of 'Char's.
--
-- This is an alias for 'fromString'.
--
-- @since 0.1.2
pack :: [Char] -> ShortText
pack = fromString

-- | \(\mathcal{O}(n)\) Convert 'ShortText' into a list of 'Char's.
--
-- This is an alias for 'toString'.
--
-- prop> (pack . unpack) t == t
--
-- @since 0.1.2
unpack :: ShortText -> [Char]
unpack = toString

-- | \(\mathcal{O}(n)\) Take prefix of given length or return whole 'ShortText' if too short.
--
-- >>> take 3 "abcdef"
-- "abc"
--
-- >>> take 3 "ab"
-- "ab"
--
-- @since 0.1.2
take :: Int -> ShortText -> ShortText
take n = fst . splitAt n

-- | \(\mathcal{O}(n)\) Take suffix of given length or return whole 'ShortText' if too short.
--
-- >>> takeEnd 3 "abcdefg"
-- "efg"
--
-- >>> takeEnd 3 "ab"
-- "ab"
--
-- @since 0.1.2
takeEnd :: Int -> ShortText -> ShortText
takeEnd n = snd . splitAtEnd n

-- | \(\mathcal{O}(n)\) Take remove prefix of given length from 'ShortText' or return 'empty' 'ShortText' if too short.
--
-- >>> drop 4 "abcdef"
-- "ef"
--
-- >>> drop 4 "ab"
-- ""
--
-- @since 0.1.2
drop :: Int -> ShortText -> ShortText
drop n = snd . splitAt n

-- | \(\mathcal{O}(n)\) Take remove suffix of given length from 'ShortText' or return 'empty' 'ShortText' if too short.
--
-- >>> drop 4 "abcdefghi"
-- "efghi"
--
-- >>> drop 4 "ab"
-- ""
--
-- @since 0.1.2
dropEnd :: Int -> ShortText -> ShortText
dropEnd n = fst . splitAtEnd n

-- | \(\mathcal{O}(n)\) Take longest prefix satisfying given predicate.
--
-- prop> takeWhile p t == fst (span p t)
--
-- >>> takeWhile (< 'c') "abcdabcd"
-- "ab"
--
-- @since 0.1.2
takeWhile :: (Char -> Bool) -> ShortText -> ShortText
takeWhile p = fst . span p

-- | \(\mathcal{O}(n)\) Take longest suffix satisfying given predicate.
--
-- prop> takeWhileEnd p t == snd (spanEnd p t)
--
-- >>> takeWhileEnd (>= 'c') "abcdabcd"
-- "cd"
--
-- @since 0.1.2
takeWhileEnd :: (Char -> Bool) -> ShortText -> ShortText
takeWhileEnd p = snd . spanEnd p

-- | \(\mathcal{O}(n)\) Remove longest prefix satisfying given predicate.
--
-- prop> dropWhile p t == snd (span p t)
--
-- >>> dropWhile (< 'c') "abcdabcd"
-- "cdabcd"
--
-- @since 0.1.2
dropWhile :: (Char -> Bool) -> ShortText -> ShortText
dropWhile p = snd . span p

-- | \(\mathcal{O}(n)\) Remove longest suffix satisfying given predicate.
--
-- prop> dropWhileEnd p t == fst (spanEnd p t)
--
-- >>> dropWhileEnd (>= 'c') "abcdabcd"
-- "abcdab"
--
-- @since 0.1.2
dropWhileEnd :: (Char -> Bool) -> ShortText -> ShortText
dropWhileEnd p = fst . spanEnd p


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Text.Show.Functions ()
-- >>> import qualified Test.QuickCheck.Arbitrary as QC
-- >>> instance QC.Arbitrary ShortText where { arbitrary = fmap fromString QC.arbitrary }
