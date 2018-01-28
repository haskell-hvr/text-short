module Main where

import           Criterion.Main

import           Control.Exception
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Short.Internal as IUT

-- The function we're benchmarking.
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

{-# NOINLINE allcharsBS1 #-}
allcharsBS1 = T.encodeUtf8 (T.pack ['\x0'..'\x7f'])

{-# NOINLINE allcharsST1 #-}
Just allcharsST1 = IUT.fromByteString allcharsBS1

{-# NOINLINE allcharsST1' #-}
Just allcharsST1' = IUT.fromByteString (IUT.toByteString allcharsST1)

{-# NOINLINE allcharsBS2 #-}
allcharsBS2 = T.encodeUtf8 (T.pack $ ['\x0'..'\xd7ff'] ++ ['\xe000'..'\x10ffff'])

{-# NOINLINE allcharsST2 #-}
Just allcharsST2 = IUT.fromByteString allcharsBS2

{-# NOINLINE allcharsST2' #-}
Just allcharsST2' = IUT.fromByteString (IUT.toByteString allcharsST2)

{-# NOINLINE allAscii128K #-}
allAscii128K = mconcat (replicate 1024 allcharsST1)

-- Our benchmark harness.
main = do
  evaluate allcharsST1
  evaluate allcharsST1'
  evaluate allcharsST2
  evaluate allcharsST2'
  evaluate allAscii128K

  defaultMain
    [ bgroup "singleton"
      [ bench "'a' :: ShortText" $ whnf IUT.singleton 'a'
      , bench "'a' :: Text" $ whnf (T.singleton) 'a'
      , bench "U+10FFFF :: ShortText" $ whnf (IUT.singleton) '\x10ffff'
      , bench "U+10FFFF :: Text" $ whnf (T.singleton) '\x10ffff'
      ]
    , bgroup "toString"
      [ bench "t1" $ nf IUT.toString allcharsST1
      , bench "t2" $ nf IUT.toString allcharsST2
      , bench "t3" $ nf IUT.toString allAscii128K
      , bench "t1 (Text)" $ nf T.unpack (IUT.toText allcharsST1)
      , bench "t2 (Text)" $ nf T.unpack (IUT.toText allcharsST2)
      , bench "t3 (Text)" $ nf T.unpack (IUT.toText allAscii128K)
      ]

    , bgroup "length"
      [ bench "1" $ whnf IUT.length allcharsST1
      , bench "2" $ whnf IUT.length allcharsST2
      , bench "3" $ whnf IUT.length allAscii128K
      ]

    , bgroup "=="
      [ bench "== 1a"  $ whnf (== allcharsST1) allcharsST1
      , bench "== 1b"  $ whnf (== allcharsST1) allcharsST1'
      , bench "== 2a"  $ whnf (== allcharsST2) allcharsST2
      , bench "== 2b"  $ whnf (== allcharsST2) allcharsST2'
      ]

    , bgroup "isAscii"
      [ bench "isAscii 1" $ whnf IUT.isAscii allcharsST1
      , bench "isAscii 2" $ whnf IUT.isAscii allcharsST2
      , bench "isAscii 3" $ whnf IUT.isAscii allAscii128K
      ]

    , bgroup "isValidUtf8"
      [ bench "isValidUtf8 1" $ whnf IUT.isValidUtf8 allcharsST1
      , bench "fromByteString 1" $ whnf IUT.fromByteString allcharsBS1

      , bench "isValidUtf8 2" $ whnf IUT.isValidUtf8 allcharsST2
      , bench "fromByteString 2" $ whnf IUT.fromByteString allcharsBS2
      ]
    ]

