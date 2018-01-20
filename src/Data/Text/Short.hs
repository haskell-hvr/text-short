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
import           Prelude                  ()
