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

      -- * Conversions
      -- ** 'String'
    , fromString
    , toString

      -- ** 'T.Text'
    , fromText
    , toText

      -- ** 'BS.ByteString'
    , fromShortByteString
    , toShortByteString

    , fromByteString
    , toByteString

    , toBuilder

    ) where

import Data.Text.Short.Internal
import Prelude ()
