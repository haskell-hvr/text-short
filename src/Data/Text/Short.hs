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
module Data.Text.Short
    ( -- * The 'ShortText' type
      ShortText

      -- * Basic operations
    , pack
    , unpack
    , append
    , concat
    , empty
    , null
    , length
    , isAscii
    , (!?)
    , indexMaybe
    , indexEndMaybe
    , splitAt
    , splitAtEnd
    , isPrefixOf
    , stripPrefix
    , isSuffixOf
    , stripSuffix

    , cons
    , uncons
    , snoc
    , unsnoc

    , findIndex
    , find
    , all
    , any

    , span
    , break
    , spanEnd
    , breakEnd

    , take
    , takeEnd
    , drop
    , dropEnd
    , takeWhile
    , takeWhileEnd
    , dropWhile
    , dropWhileEnd

    , intersperse
    , intercalate

      -- * Conversions
      -- ** 'Char'
    , singleton

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
import           Prelude                  (Bool (..), Char, Int, Maybe (..),
                                           fst, not, snd, (.))

-- | \(\mathcal{O}(n)\) Variant of 'span' with negated predicate.
--
-- > break p = span (not . p)
--
-- @since TBD
break :: (Char -> Bool) -> ShortText -> (ShortText,ShortText)
break p st = span (not . p) st

-- | \(\mathcal{O}(n)\) Variant of 'spanEnd' with negated predicate.
--
-- > breakEnd p = spanEnd (not . p)
--
-- @since TBD
breakEnd :: (Char -> Bool) -> ShortText -> (ShortText,ShortText)
breakEnd p st = spanEnd (not . p) st

-- | \(\mathcal{O}(n)\) Index /i/-th code-point in 'ShortText'.
--
-- Infix operator alias of 'indexMaybe'
--
-- @since TBD
(!?) :: ShortText -> Int -> Maybe Char
(!?) = indexMaybe

-- | \(\mathcal{O}(n)\) Test whether /any/ code points in 'ShortText' satisfy a predicate.
--
-- @since TBD
any :: (Char -> Bool) -> ShortText -> Bool
any p st = case find p st of
             Nothing -> False
             Just _  -> True

-- | \(\mathcal{O}(n)\) Concatenate two 'ShortText's
--
-- This is a type-specialised alias of '<>'.
--
-- @since TBD
append :: ShortText -> ShortText -> ShortText
append = (<>)

-- | \(\mathcal{O}(n)\) Concatenate list of 'ShortText's
--
-- This is a type-specialised alias of 'mconcat'.
--
-- @since TBD
concat :: [ShortText] -> ShortText
concat = mconcat

-- | \(\mathcal{O}(0)\) The /empty/ 'ShortText'.
--
-- This is a type-specialised alias of 'mempty'.
--
-- @since TBD
empty :: ShortText
empty = mempty

-- | \(\mathcal{O}(n)\) Construct a 'ShortText' from a list of 'Char's.
--
-- This is an alias for 'fromString'.
--
-- @since TBD
pack :: [Char] -> ShortText
pack = fromString

-- | \(\mathcal{O}(n)\) Convert 'ShortText' into a list of 'Char's.
--
-- This is an alias for 'toString'.
--
-- @since TBD
unpack :: ShortText -> [Char]
unpack = toString

-- | \(\mathcal{O}(n)\) Take prefix of given length or return whole 'ShortText' if too short.
--
-- @since TBD
take :: Int -> ShortText -> ShortText
take n = fst . splitAt n

-- | \(\mathcal{O}(n)\) Take suffix of given length or return whole 'ShortText' if too short.
--
-- @since TBD
takeEnd :: Int -> ShortText -> ShortText
takeEnd n = snd . splitAtEnd n

-- | \(\mathcal{O}(n)\) Take remove prefix of given length from 'ShortText' or return 'empty' 'ShortText' if too short.
--
-- @since TBD
drop :: Int -> ShortText -> ShortText
drop n = snd . splitAt n

-- | \(\mathcal{O}(n)\) Take remove suffix of given length from 'ShortText' or return 'empty' 'ShortText' if too short.
--
-- @since TBD
dropEnd :: Int -> ShortText -> ShortText
dropEnd n = fst . splitAtEnd n

-- | \(\mathcal{O}(n)\) Take longest prefix satisfying given predicate.
--
-- @since TBD
takeWhile :: (Char -> Bool) -> ShortText -> ShortText
takeWhile p = fst . span p

-- | \(\mathcal{O}(n)\) Take longest suffix satisfying given predicate.
--
-- @since TBD
takeWhileEnd :: (Char -> Bool) -> ShortText -> ShortText
takeWhileEnd p = snd . spanEnd p

-- | \(\mathcal{O}(n)\) Remove longest prefix satisfying given predicate.
--
-- @since TBD
dropWhile :: (Char -> Bool) -> ShortText -> ShortText
dropWhile p = snd . span p

-- | \(\mathcal{O}(n)\) Remove longest suffix satisfying given predicate.
--
-- @since TBD
dropWhileEnd :: (Char -> Bool) -> ShortText -> ShortText
dropWhileEnd p = fst . spanEnd p
