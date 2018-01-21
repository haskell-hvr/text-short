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
-- @since TBD
module Data.Text.Short.Partial
    ( head
    , tail
    , last
    , init
    , index
    ) where

import           Data.Text.Short.Internal
import           Prelude                  (Int, Maybe (..), error, fst, maybe,
                                           snd, (.))

head :: ShortText -> Char
head = maybe (error "head: empty ShortText") fst . uncons

tail :: ShortText -> ShortText
tail = maybe (error "tail: empty ShortText") snd . uncons

init :: ShortText -> ShortText
init = maybe (error "init: empty ShortText") fst . unsnoc

last :: ShortText -> Char
last = maybe (error "last: empty ShortText") snd . unsnoc

index :: ShortText -> Int -> Char
index st i = case indexMaybe st i of
               Nothing -> error "index: not within ShortText"
               Just c  -> c
