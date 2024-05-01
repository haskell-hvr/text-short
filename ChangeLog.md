## 0.1.6

  * Drop support for GHC prior 8.6.5
  * Support GHC-9.10 (base-4.21)

## 0.1.5

  * text-2.0 support

## 0.1.4

  * Fix `fromString` for single character strings.
    https://github.com/haskell-hvr/text-short/issues/20
  * Add Template Haskell `Lift ShortText` instance.

## 0.1.3

  * Add `Data ShortText` instance
  * Define `Typeable ShortText` also for GHC 7.8 as well
    (NB: for GHC 7.10.3 and up `Typeable` instances are automatically
     defined even when not mentioned explicitly in a `deriving` clause)
  * Add equivalent verb `Data.Text.split` to `Data.Text.Short` API

        split :: (Char -> Bool) -> ShortText -> [ShortText]

## 0.1.2

  * Add `IsList ShortText` and `PrintfArg ShortText` instances
  * Expose partial functions via new `Data.Text.Short.Partial` module

        foldl1 :: (Char -> Char -> Char) -> ShortText -> Char
        foldl1' :: (Char -> Char -> Char) -> ShortText -> Char
        foldr1 :: (Char -> Char -> Char) -> ShortText -> Char
        head :: ShortText -> Char
        index :: ShortText -> Int -> Char
        init :: ShortText -> ShortText
        last :: ShortText -> Char
        tail :: ShortText -> ShortText

  * Add several `Data.Text` verbs to `Data.Text.Short` API

        (!?) :: ShortText -> Int -> Maybe Char
        all :: (Char -> Bool) -> ShortText -> Bool
        any :: (Char -> Bool) -> ShortText -> Bool
        append :: ShortText -> ShortText -> ShortText
        break :: (Char -> Bool) -> ShortText -> (ShortText, ShortText)
        breakEnd :: (Char -> Bool) -> ShortText -> (ShortText, ShortText)
        concat :: [ShortText] -> ShortText
        cons :: Char -> ShortText -> ShortText
        drop :: Int -> ShortText -> ShortText
        dropAround :: (Char -> Bool) -> ShortText -> ShortText
        dropEnd :: Int -> ShortText -> ShortText
        dropWhile :: (Char -> Bool) -> ShortText -> ShortText
        dropWhileEnd :: (Char -> Bool) -> ShortText -> ShortText
        empty :: ShortText
        filter :: (Char -> Bool) -> ShortText -> ShortText
        find :: (Char -> Bool) -> ShortText -> Maybe Char
        findIndex :: (Char -> Bool) -> ShortText -> Maybe Int
        foldl :: (a -> Char -> a) -> a -> ShortText -> a
        foldl' :: (a -> Char -> a) -> a -> ShortText -> a
        foldr :: (Char -> a -> a) -> a -> ShortText -> a
        indexEndMaybe :: ShortText -> Int -> Maybe Char
        indexMaybe :: ShortText -> Int -> Maybe Char
        intercalate :: ShortText -> [ShortText] -> ShortText
        intersperse :: Char -> ShortText -> ShortText
        isPrefixOf :: ShortText -> ShortText -> Bool
        isSuffixOf :: ShortText -> ShortText -> Bool
        pack :: [Char] -> ShortText
        replicate :: Int -> ShortText -> ShortText
        reverse :: ShortText -> ShortText
        singleton :: Char -> ShortText
        snoc :: ShortText -> Char -> ShortText
        span :: (Char -> Bool) -> ShortText -> (ShortText, ShortText)
        spanEnd :: (Char -> Bool) -> ShortText -> (ShortText, ShortText)
        splitAt :: Int -> ShortText -> (ShortText, ShortText)
        splitAtEnd :: Int -> ShortText -> (ShortText, ShortText)
        stripPrefix :: ShortText -> ShortText -> Maybe ShortText
        stripSuffix :: ShortText -> ShortText -> Maybe ShortText
        take :: Int -> ShortText -> ShortText
        takeEnd :: Int -> ShortText -> ShortText
        takeWhile :: (Char -> Bool) -> ShortText -> ShortText
        takeWhileEnd :: (Char -> Bool) -> ShortText -> ShortText
        uncons :: ShortText -> Maybe (Char, ShortText)
        unpack :: ShortText -> [Char]
        unsnoc :: ShortText -> Maybe (ShortText, Char)

  * Optimise low-level primitives
  * Add support for GHC 8.4

## 0.1.1

* Expose *unsafe* conversion API via `Data.Text.Short.Unsafe` module
* Minor documentation improvement

## 0.1

* First version. Released on an unsuspecting world.
