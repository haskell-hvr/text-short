{-# LANGUAGE Unsafe #-}

-- |
-- Module      : Data.Text.Short.Unsafe
-- Copyright   : © Herbert Valerio Riedel 2017
-- License     : BSD3
--
-- Maintainer  : hvr@gnu.org
-- Stability   : stable
--
-- Unsafe API
--
-- This module provides unsafe conversion functions
module Data.Text.Short.Unsafe
    ( fromShortByteStringUnsafe
    , fromByteStringUnsafe
    ) where

import Data.Text.Short.Internal
import Prelude ()
