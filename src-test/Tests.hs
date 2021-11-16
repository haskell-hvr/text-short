{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifndef MIN_VERSION_GLASGOW_HASKELL
#define MIN_VERSION_GLASGOW_HASKELL(x,y,z,w) ((x*100 + y) >= __GLASGOW_HASKELL__)
#endif

module Main(main) where

import           Data.Binary
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import qualified Data.String               as D.S
import qualified Data.ByteString           as BS
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Short           as IUT
import qualified Data.Text.Short.Partial   as IUT
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck     as QC
import           Text.Show.Functions       ()

fromByteStringRef = either (const Nothing) (Just . IUT.fromText) . T.decodeUtf8'

main :: IO ()
main = defaultMain (adjustOption (QuickCheckTests 50000 `max`) $ tests)

tests :: TestTree
tests = testGroup "Tests" [unitTests,qcProps]

-- ShortText w/ in-bounds index
data STI = STI IUT.ShortText Int
         deriving (Eq,Show)

newtype ST = ST IUT.ShortText
           deriving (Eq,Show)

instance Arbitrary STI where
  arbitrary = do
    t <- arbitrary
    i <- choose (0, T.length t - 1)
    return $! STI (IUT.fromText t) i

instance Arbitrary ST where
  arbitrary = fmap (ST . IUT.fromText) arbitrary
  shrink (ST st) = map (ST . IUT.fromText) (shrink (IUT.toText st))

qcProps :: TestTree
qcProps = testGroup "Properties"
  [ QC.testProperty "length/fromText"   $ \t -> IUT.length (IUT.fromText t) == T.length t
  , QC.testProperty "length/fromString" $ \s -> IUT.length (IUT.fromString s) == length s
  , QC.testProperty "length/append"     $ \(ST t1) (ST t2) -> IUT.length t1 + IUT.length t2 == IUT.length (IUT.append t1 t2)
  , QC.testProperty "compare" $ \t1 t2 -> IUT.fromText t1 `compare` IUT.fromText t2  == t1 `compare` t2
  , QC.testProperty "(==)" $ \t1 t2 -> (IUT.fromText t1 == IUT.fromText t2)  == (t1 == t2)
  , QC.testProperty "(!?)" $ \t ->
      let t' = IUT.fromText t
      in and ([ mapMaybe (t' IUT.!?) ([0 .. T.length t -1 ] :: [Int]) == T.unpack t
              , mapMaybe (t' IUT.!?) [-5 .. -1] == []
              , mapMaybe (t' IUT.!?) [T.length t .. T.length t + 5] == []
              ] :: [Bool])
  , QC.testProperty "indexEndMaybe" $ \t ->
      let t' = IUT.fromText t
      in and ([ mapMaybe (IUT.indexEndMaybe t') [0 .. T.length t -1 ] == T.unpack (T.reverse t)
              , mapMaybe (IUT.indexEndMaybe t') [-5 .. -1] == []
              , mapMaybe (IUT.indexEndMaybe t') [T.length t .. T.length t + 5] == []
              ] :: [Bool])
  , QC.testProperty "toText.fromText"   $ \t -> (IUT.toText . IUT.fromText) t == t
  , QC.testProperty "fromByteString"    $ \b -> IUT.fromByteString b == fromByteStringRef b
  , QC.testProperty "fromByteString.toByteString" $ \t -> let ts = IUT.fromText t in (IUT.fromByteString . IUT.toByteString) ts == Just ts
  , QC.testProperty "toString.fromString" $ \s -> (IUT.toString . IUT.fromString) s == s
  , QC.testProperty "isAscii"  $ \s -> IUT.isAscii (IUT.fromString s) == all isAscii s
  , QC.testProperty "isAscii2" $ \t -> IUT.isAscii (IUT.fromText t)   == T.all isAscii t
  , QC.testProperty "splitAt" $ \t ->
      let t' = IUT.fromText t
          mapBoth f (x,y) = (f x, f y)
      in and [ mapBoth IUT.toText (IUT.splitAt i t') == T.splitAt i t | i <- [-5 .. 5+T.length t ] ]
  , QC.testProperty "intercalate/split" $ \t c ->
      let t' = IUT.fromText t
      in IUT.intercalate (IUT.singleton c) (IUT.split (== c) t') == t'

  , QC.testProperty "intersperse" $ \t c -> IUT.intersperse c (IUT.fromText t) == IUT.fromText (T.intersperse c t)
  , QC.testProperty "intercalate" $ \t1 t2 -> IUT.intercalate (IUT.fromText t1) (map IUT.fromText t2) == IUT.fromText (T.intercalate t1 t2)
  , QC.testProperty "reverse.singleton" $ \c -> IUT.reverse (IUT.singleton c) == IUT.singleton c
  , QC.testProperty "reverse"     $ \t -> IUT.reverse (IUT.fromText t) == IUT.fromText (T.reverse t)
  , QC.testProperty "filter"      $ \p t -> IUT.filter p (IUT.fromText t) == IUT.fromText (T.filter p t)
  , QC.testProperty "replicate"   $ \n t -> IUT.replicate n (IUT.fromText t) == IUT.fromText (T.replicate n t)
  , QC.testProperty "dropAround"  $ \p t -> IUT.dropAround p (IUT.fromText t) == IUT.fromText (T.dropAround p t)

  , QC.testProperty "foldl"       $ \f z t -> IUT.foldl f (z :: Char) (IUT.fromText t) == T.foldl f (z :: Char) t
  , QC.testProperty "foldl #2"    $ \t -> IUT.foldl (\n _ -> (n+1)) 0 (IUT.fromText t) == T.length t
  , QC.testProperty "foldl #3"    $ \t -> IUT.foldl (\s c -> c : s) [] (IUT.fromText t) == T.unpack (T.reverse t)

  , QC.testProperty "foldl'"      $ \f z t -> IUT.foldl' f (z :: Char) (IUT.fromText t) == T.foldl' f (z :: Char) t
  , QC.testProperty "foldl' #2"   $ \t -> IUT.foldl' (\n _ -> (n+1)) 0 (IUT.fromText t) == T.length t
  , QC.testProperty "foldl' #3"   $ \t -> IUT.foldl' (\s c -> c : s) [] (IUT.fromText t) == T.unpack (T.reverse t)

  , QC.testProperty "foldr"       $ \f z t -> IUT.foldr f (z :: Char) (IUT.fromText t) == T.foldr f (z :: Char) t
  , QC.testProperty "foldr #2"    $ \t -> IUT.foldr (\_ n -> (n+1)) 0 (IUT.fromText t) == T.length t
  , QC.testProperty "foldr #3"    $ \t -> IUT.foldr (:) [] (IUT.fromText t) == T.unpack t

  , QC.testProperty "foldr1"      $ \f t -> (not (T.null t)) ==> IUT.foldr1 f (IUT.fromText t) == T.foldr1 f t
  , QC.testProperty "foldl1"      $ \f t -> (not (T.null t)) ==> IUT.foldl1 f (IUT.fromText t) == T.foldl1 f t
  , QC.testProperty "foldl1'"     $ \f t -> (not (T.null t)) ==> IUT.foldl1' f (IUT.fromText t) == T.foldl1' f t

  , QC.testProperty "splitAtEnd" $ \t ->
      let t' = IUT.fromText t
          n' = IUT.length t'
      in and [ (IUT.splitAt (n'-i) t') == IUT.splitAtEnd i t' | i <- [-5 .. 5+n' ] ]

  , QC.testProperty "find" $ \t -> IUT.find Data.Char.isAscii (IUT.fromText t) == T.find Data.Char.isAscii t
  , QC.testProperty "findIndex" $ \t -> IUT.findIndex Data.Char.isAscii (IUT.fromText t) == T.findIndex Data.Char.isAscii t

  , QC.testProperty "isSuffixOf" $ \t1 t2 -> IUT.fromText t1 `IUT.isSuffixOf` IUT.fromText t2  == t1 `T.isSuffixOf` t2
  , QC.testProperty "isPrefixOf" $ \t1 t2 -> IUT.fromText t1 `IUT.isPrefixOf` IUT.fromText t2  == t1 `T.isPrefixOf` t2

  , QC.testProperty "stripPrefix" $ \t1 t2 -> IUT.stripPrefix (IUT.fromText t1) (IUT.fromText t2) ==
                                                fmap IUT.fromText (T.stripPrefix t1 t2)

  , QC.testProperty "stripSuffix" $ \t1 t2 -> IUT.stripSuffix (IUT.fromText t1) (IUT.fromText t2) ==
                                                fmap IUT.fromText (T.stripSuffix t1 t2)

  , QC.testProperty "stripPrefix 2" $ \(STI t i) ->
      let (pfx,sfx) = IUT.splitAt i t
      in IUT.stripPrefix pfx t == Just sfx

  , QC.testProperty "stripSuffix 2" $ \(STI t i) ->
      let (pfx,sfx) = IUT.splitAt i t
      in IUT.stripSuffix sfx t == Just pfx

  , QC.testProperty "cons" $ \c t -> IUT.singleton c <> IUT.fromText t == IUT.cons c (IUT.fromText t)
  , QC.testProperty "snoc" $ \c t -> IUT.fromText t <> IUT.singleton c == IUT.snoc (IUT.fromText t) c

  , QC.testProperty "uncons" $ \c t -> IUT.uncons (IUT.singleton c <> IUT.fromText t) == Just (c, IUT.fromText t)

  , QC.testProperty "unsnoc" $ \c t -> IUT.unsnoc (IUT.fromText t <> IUT.singleton c) == Just (IUT.fromText t, c)

  , QC.testProperty "break" $ \t -> let (l,r)   = IUT.break Data.Char.isAscii (IUT.fromText t)
                                    in  T.break Data.Char.isAscii t == (IUT.toText l,IUT.toText r)

  , QC.testProperty "span"  $ \t -> let (l,r)   = IUT.span Data.Char.isAscii (IUT.fromText t)
                                    in  T.span Data.Char.isAscii t == (IUT.toText l,IUT.toText r)

  , QC.testProperty "breakEnd" $ \t -> let (l,r)   = IUT.breakEnd Data.Char.isAscii (IUT.fromText t)
                                       in  t_breakEnd Data.Char.isAscii t == (IUT.toText l,IUT.toText r)

  , QC.testProperty "spanEnd"  $ \t -> let (l,r)   = IUT.spanEnd Data.Char.isAscii (IUT.fromText t)
                                       in  t_spanEnd Data.Char.isAscii t == (IUT.toText l,IUT.toText r)

  , QC.testProperty "splitAt/isPrefixOf" $ \t ->
      let t' = IUT.fromText t
      in and [ IUT.isPrefixOf (fst (IUT.splitAt i t')) t' | i <- [-5 .. 5+T.length t ] ]
  , QC.testProperty "splitAt/isSuffixOf" $ \t ->
      let t' = IUT.fromText t
      in and [ IUT.isSuffixOf (snd (IUT.splitAt i t')) t' | i <- [-5 .. 5+T.length t ] ]
  ]

t_breakEnd p t = t_spanEnd (not . p) t
t_spanEnd  p t = (T.dropWhileEnd p t, T.takeWhileEnd p t)

unitTests = testGroup "Unit-tests"
  [ testCase "fromText mempty" $ IUT.fromText mempty @?= mempty
  , testCase "fromShortByteString [0xc0,0x80]" $ IUT.fromShortByteString "\xc0\x80" @?= Nothing
  , testCase "fromByteString [0xc0,0x80]" $ IUT.fromByteString "\xc0\x80" @?= Nothing
  , testCase "fromByteString [0xf0,0x90,0x80,0x80]" $ IUT.fromByteString "\xf0\x90\x80\x80" @?= Just "\x10000"
  , testCase "fromByteString [0xf4,0x90,0x80,0x80]" $ IUT.fromByteString "\244\144\128\128" @?= Nothing
  , testCase "IsString U+D800" $ "\xFFFD" @?= (IUT.fromString "\xD800")
--  , testCase "IsString U+D800" $ (IUT.fromString "\xD800") @?= IUT.fromText ("\xD800" :: T.Text)

#if !(MIN_VERSION_bytestring(0,11,0) && MIN_VERSION_GLASGOW_HASKELL(9,0,1,0) && !MIN_VERSION_GLASGOW_HASKELL(9,0,2,0))
  -- https://gitlab.haskell.org/ghc/ghc/-/issues/19976
  , testCase "Binary.encode" $ encode ("Hello \8364 & \171581!\NUL" :: IUT.ShortText) @?= "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC2Hello \226\130\172 & \240\169\184\189!\NUL"
  , testCase "Binary.decode" $ decode ("\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC2Hello \226\130\172 & \240\169\184\189!\NUL") @?= ("Hello \8364 & \171581!\NUL" :: IUT.ShortText)
#endif
  , testCase "singleton" $ [ c | c <- [minBound..maxBound], IUT.singleton c /= IUT.fromText (T.singleton c) ] @?= []

  , testCase "splitAtEnd" $ IUT.splitAtEnd 1 "€€" @?= ("€","€")
  , testCase "split#1" $ IUT.split (== 'a') "aabbaca" @?= ["", "", "bb", "c", ""]
  , testCase "split#2" $ IUT.split (const False) "aabbaca" @?= ["aabbaca"]
  , testCase "split#3" $ IUT.split (const True) "abc" @?= ["","","",""]
  , testCase "split#4" $ IUT.split (const True) "" @?= [""]

  , testCase "literal0" $ IUT.unpack testLit0 @?= []
  , testCase "literal1" $ IUT.unpack testLit1 @?= ['€','\0','€','\0']
  , testCase "literal2" $ IUT.unpack testLit2 @?= ['\xFFFD','\xD7FF','\xFFFD','\xE000']
  , testCase "literal3" $ IUT.unpack testLit3 @?= ['\1'..'\x7f']
  , testCase "literal4" $ IUT.unpack testLit4 @?= map toEnum [0,1,126,127,128,129,130,256,2046,2047,2048,2049,2050,65530,65531,65532,65533,65534,65533,65535,65536,65537,65538,1114110,1114111]
  , testCase "literal5" $ IUT.unpack testLit5 @?= map toEnum [28961]
  , testCase "literal6" $ IUT.unpack testLit6 @?= map toEnum [0]
  , testCase "literal7" $ IUT.unpack testLit7 @?= map toEnum [66328]
  , testCase "literal8" $ IUT.unpack testLit8 @?= map toEnum [127]

    -- list literals
  , testCase "literal9"  $ [] @?= ("" :: IUT.ShortText)
  , testCase "literal10" $ ['¤','€','$'] @?= ("¤€$" :: IUT.ShortText)
  , testCase "literal12" $ IUT.unpack ['\xD800','\xD7FF','\xDFFF','\xE000'] @?= ['\xFFFD','\xD7FF','\xFFFD','\xE000']

    -- template haskell
  , testCase "TH.Lift" $ do
      testLit1 @?= $([| testLit1 |])
      testLit2 @?= $([| testLit2 |])
      testLit3 @?= $([| testLit3 |])
      testLit4 @?= $([| testLit4 |])
      testLit5 @?= $([| testLit5 |])
      testLit6 @?= $([| testLit6 |])
      testLit7 @?= $([| testLit7 |])
      testLit8 @?= $([| testLit8 |])

  , testCase "TTH.Lift" $ do
      testLit1 @?= $$([|| testLit1 ||])
      testLit2 @?= $$([|| testLit2 ||])
      testLit3 @?= $$([|| testLit3 ||])
      testLit4 @?= $$([|| testLit4 ||])
      testLit5 @?= $$([|| testLit5 ||])
      testLit6 @?= $$([|| testLit6 ||])
      testLit7 @?= $$([|| testLit7 ||])
      testLit8 @?= $$([|| testLit8 ||])
 ]

-- isScalar :: Char -> Bool
-- isScalar c = c < '\xD800' || c >= '\xE000'


{-# NOINLINE testLit0 #-}
testLit0 :: IUT.ShortText
testLit0 = ""

{-# NOINLINE testLit1 #-}
testLit1 :: IUT.ShortText
testLit1 = "€\NUL€\NUL"

{-# NOINLINE testLit2 #-}
testLit2 :: IUT.ShortText
testLit2 = "\xD800\xD7FF\xDFFF\xE000"

{-# NOINLINE testLit3 #-}
testLit3 :: IUT.ShortText
testLit3 = "\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\DEL"

{-# NOINLINE testLit4 #-}
testLit4 :: IUT.ShortText
testLit4 = "\NUL\SOH~\DEL\128\129\130\256\2046\2047\2048\2049\2050\65530\65531\65532\65533\65534\65533\65535\65536\65537\65538\1114110\1114111"

{-# NOINLINE testLit5 #-}
testLit5 :: IUT.ShortText
testLit5 = "無"

{-# NOINLINE testLit6 #-}
testLit6 :: IUT.ShortText
testLit6 = "\NUL"

{-# NOINLINE testLit7 #-}
testLit7 :: IUT.ShortText
testLit7 = "𐌘"

{-# NOINLINE testLit8 #-}
testLit8 :: IUT.ShortText
testLit8 = "\x7f"

-------------------------------------------------------------------------------
-- orphans
-------------------------------------------------------------------------------

-- orphan instances to not depend on quickcheck-instances
-- which would cause cycles

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack `fmap` arbitrary
    shrink xs = BS.pack `fmap` shrink (BS.unpack xs)

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary
    shrink xs = T.pack `fmap` shrink (T.unpack xs)
