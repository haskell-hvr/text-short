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

import           Data.Text.Short.Internal
import           Prelude                  (Bool (..), Char, Int, Maybe (..),
                                           not, (.))

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
