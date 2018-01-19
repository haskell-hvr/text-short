{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Data.Binary
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import qualified Data.String               as D.S
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Short           as IUT
import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck     as QC

fromByteStringRef = either (const Nothing) (Just . IUT.fromText) . T.decodeUtf8'

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests,qcProps]

qcProps :: TestTree
qcProps = testGroup "Properties"
  [ QC.testProperty "length/fromText"   $ \t -> IUT.length (IUT.fromText t) == T.length t
  , QC.testProperty "length/fromString" $ \s -> IUT.length (IUT.fromString s) == length s
  , QC.testProperty "compare" $ \t1 t2 -> IUT.fromText t1 `compare` IUT.fromText t2  == t1 `compare` t2
  , QC.testProperty "(==)" $ \t1 t2 -> (IUT.fromText t1 == IUT.fromText t2)  == (t1 == t2)
  , QC.testProperty "(!?)" $ \t -> let t' = IUT.fromText t
                                   in mapMaybe (t' IUT.!?) [-5 .. 5+T.length t ] == T.unpack t
  , QC.testProperty "toText.fromText"   $ \t -> (IUT.toText . IUT.fromText) t == t
  , QC.testProperty "fromByteString"    $ \b -> IUT.fromByteString b == fromByteStringRef b
  , QC.testProperty "fromByteString.toByteString" $ \t -> let ts = IUT.fromText t in (IUT.fromByteString . IUT.toByteString) ts == Just ts
  , QC.testProperty "toString.fromString" $ \s -> (IUT.toString . IUT.fromString) s == s
  , QC.testProperty "isAscii"  $ \s -> IUT.isAscii (IUT.fromString s) == all isAscii s
  , QC.testProperty "isAscii2" $ \t -> IUT.isAscii (IUT.fromText t)   == T.all isAscii t
  , QC.testProperty "splitAt" $ \t -> let t' = IUT.fromText t
                                          mapBoth f (x,y) = (f x, f y)
                                      in and [ mapBoth IUT.toText (IUT.splitAt i t') == T.splitAt i t | i <- [-5 .. 5+T.length t ] ]

  , QC.testProperty "isSuffixOf" $ \t1 t2 -> IUT.fromText t1 `IUT.isSuffixOf` IUT.fromText t2  == t1 `T.isSuffixOf` t2
  , QC.testProperty "isPrefixOf" $ \t1 t2 -> IUT.fromText t1 `IUT.isPrefixOf` IUT.fromText t2  == t1 `T.isPrefixOf` t2

  , QC.testProperty "cons" $ \c t -> IUT.singleton c <> IUT.fromText t == IUT.cons c (IUT.fromText t)
  , QC.testProperty "snoc" $ \c t -> IUT.fromText t <> IUT.singleton c == IUT.snoc (IUT.fromText t) c

  , QC.testProperty "uncons" $ \c t -> IUT.uncons (IUT.singleton c <> IUT.fromText t) == Just (c, IUT.fromText t)

  , QC.testProperty "splitAt/isPrefixOf" $ \t ->
      let t' = IUT.fromText t
      in and [ IUT.isPrefixOf (fst (IUT.splitAt i t')) t' | i <- [-5 .. 5+T.length t ] ]
  , QC.testProperty "splitAt/isSuffixOf" $ \t ->
      let t' = IUT.fromText t
      in and [ IUT.isSuffixOf (snd (IUT.splitAt i t')) t' | i <- [-5 .. 5+T.length t ] ]
  ]

unitTests = testGroup "Unit-tests"
  [ testCase "fromText mempty" $ IUT.fromText mempty @?= mempty
  , testCase "fromShortByteString [0xc0,0x80]" $ IUT.fromShortByteString "\xc0\x80" @?= Nothing
  , testCase "fromByteString [0xc0,0x80]" $ IUT.fromByteString "\xc0\x80" @?= Nothing
  , testCase "IsString U+D800" $ "\xFFFD" @?= (IUT.fromString "\xD800")
--  , testCase "IsString U+D800" $ (IUT.fromString "\xD800") @?= IUT.fromText ("\xD800" :: T.Text)

  , testCase "Binary.encode" $ encode ("Hello \8364 & \171581!\NUL" :: IUT.ShortText) @?= "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC2Hello \226\130\172 & \240\169\184\189!\NUL"
  , testCase "Binary.decode" $ decode ("\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC2Hello \226\130\172 & \240\169\184\189!\NUL") @?= ("Hello \8364 & \171581!\NUL" :: IUT.ShortText)
  , testCase "singleton" $ [ c | c <- [minBound..maxBound], IUT.singleton c /= IUT.fromText (T.singleton c) ] @?= []
  ]

-- isScalar :: Char -> Bool
-- isScalar c = c < '\xD800' || c >= '\xE000'
