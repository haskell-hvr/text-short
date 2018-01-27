{-# LANGUAGE Trustworthy #-}

-- |
-- Module      : Data.Text.Short.Partial
-- Copyright   : © Herbert Valerio Riedel 2018
-- License     : BSD3
--
-- Maintainer  : hvr@gnu.org
-- Stability   : stable
--
-- Partial functions vocabulary
--
-- This module provides common partial functions for operating on 'ShortText'.
--
-- The use of these functions is discouraged as they tend to be error-prone.
--
-- @since 0.1.2
module Data.Text.Short.Partial
    ( head
    , tail
    , init
    , last
    , index

    , foldl1
    , foldl1'
    , foldr1
    ) where

import           Data.Text.Short
import           Data.Text.Short.Internal
import           Prelude                  ()

-- | \(\mathcal{O}(1)\) Returns first character of a non-empty 'ShortText'
--
-- >>> head "abcd"
-- 'a'
--
-- __Note__: Will throw an 'error' exception for empty 'ShortText's.
-- Consider using the total functions 'uncons' or 'indexMaybe'
-- instead.
--
-- @since 0.1.2
head :: ShortText -> Char
head = maybe (error "head: empty ShortText") fst . uncons

-- | \(\mathcal{O}(n)\) Drop first character from non-empty 'ShortText'.
--
-- >>> tail "abcd"
-- "bcd"
--
-- __Note__: Will throw an 'error' exception for empty 'ShortText's.
-- Consider using the total functions 'uncons' or 'drop' instead.
--
-- @since 0.1.2
tail :: ShortText -> ShortText
tail = maybe (error "tail: empty ShortText") snd . uncons

-- | \(\mathcal{O}(n)\) Drop last character from non-empty 'ShortText'.
--
-- >>> tail "abcd"
-- "bcd"
--
-- __Note__: Will throw an 'error' exception for empty 'ShortText's.
-- Consider using the total functions 'unsnoc' or 'dropEnd' instead.
--
-- @since 0.1.2
init :: ShortText -> ShortText
init = maybe (error "init: empty ShortText") fst . unsnoc

-- | \(\mathcal{O}(1)\) Return last character from non-empty 'ShortText'.
--
-- >>> last "abcd"
-- 'd'
--
-- __Note__: Will throw an 'error' exception for empty 'ShortText's.
-- Consider using the total functions 'unsnoc' or 'indexEndMaybe'
-- instead.
--
-- @since 0.1.2
last :: ShortText -> Char
last = maybe (error "last: empty ShortText") snd . unsnoc

-- | \(\mathcal{O}(n)\) Retrieve \(i\)-th character (code-point)
--
-- >>> index "abcd" 1
-- 'b'
--
-- __Note__: Will throw an 'error' exception if index is out of
-- bounds.  Consider using the total functions 'indexMaybe' or
-- 'indexEndMaybe' instead.
--
-- @since 0.1.2
index :: ShortText -> Int -> Char
index st i = case indexMaybe st i of
               Nothing -> error "index: not within ShortText"
               Just c  -> c

-- $setup
-- >>> :set -XOverloadedStrings
