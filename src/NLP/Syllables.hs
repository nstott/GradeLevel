module NLP.Syllables where

--Counting Syllables

--To find the number of syllables in a word, use the following steps:

--Count the vowels in the word.
--Subtract any silent vowels, (like the silent e at the end of a word, or the second vowel when two vowels are together in a syllabl.e)
--Subtract one vowel from every diphthong (diphthongs only count as one vowel sound.)
--The number of vowels sounds left is the same as the number of syllables.

-- or..
-- check first letter (group),
--   if its a dipthone, add 1, jump to end of dipthong, repeat
--   if its a silent vowel, skip it, repeat
--   if it's a vowel then add 1


    main :: IO ()
    main =
        putStrLn "Hello"

    --
    -- a string zipper, we're processing a string letter by letter, keep track
    -- of letters that have been processed, and letters that haven't been processed
    --
    data StringZipper = StringZipper{prev :: String, next :: String} deriving (Show)

    newStringZipper :: String -> StringZipper
    newStringZipper s = StringZipper{prev = "", next = s}

    walkAhead :: Int -> StringZipper -> StringZipper
    walkAhead _ zipper@StringZipper{next = ""} = zipper
    walkAhead num StringZipper{prev = p, next = n} = StringZipper{prev = p ++ take num n, next = drop num n}

    walkBack :: Int -> StringZipper -> StringZipper
    walkBack _ zipper@(StringZipper{ prev = "" }) = zipper
    walkBack num StringZipper{prev = p, next = n} = StringZipper{prev = reverse (drop num (reverse p)), next = reverse (take num (reverse p)) ++ n}

    atEnd :: StringZipper -> Bool
    atEnd StringZipper{next = ""} = True
    atEnd _ = False


    type CountZipper = (Int, StringZipper)

    countVowels :: CountZipper -> Int
    countVowels (count, zipper)
        | atEnd zipper = count
        | otherwise = countVowels $ foldr testfn (count, zipper) [zipperIsDipthong, zipperIsVowel]
          where
            testfn = countZipperTest


    countZipperTest :: (StringZipper -> (Bool, StringZipper)) -> CountZipper -> CountZipper
    countZipperTest test (num, zipper) =
        case test zipper of
            (True, newzip) -> (num + 1, newzip)
            (False, newzip) -> (num, newzip)


    -- definitions for english letter forms
    dipthongs :: [String]
    dipthongs = ["ou", "oa", "ao", "oi", "io", "ui", "ea", "ei", "ie", "eo", "oe", "eu", "ue", "ai", "au"]

    isDipthong :: String -> Bool
    isDipthong (x:y:_) = [x,y] `elem` dipthongs
    isDipthong _ = False

    consonantDigraphs :: [String]
    consonantDigraphs = ["th", "sh", "ph", "th", "ch", "wh"]

    vowels :: String
    vowels = "aeiouy"

    isVowel :: String -> Bool
    isVowel [] = False
    isVowel (x:_) = x `elem` vowels

    isConsonant :: String -> Bool
    isConsonant = not . isVowel

    zipperTestHead :: (String -> Bool) -> Int -> StringZipper -> (Bool, StringZipper)
    zipperTestHead test skip z@StringZipper{next = n}
        | test n = (True, walkAhead skip z)
        | otherwise = (False, walkAhead skip z)

    zipperIsDipthong :: StringZipper -> (Bool, StringZipper)
    zipperIsDipthong = zipperTestHead isDipthong 2

    zipperIsVowel :: StringZipper -> (Bool, StringZipper)
    zipperIsVowel = zipperTestHead isVowel 1





    --indexOfDoubleCon :: String -> Maybe Int
    --indexOfDoubleCon word@(x:xs) = Just 1
        --where
        --    pair :: String -> String
        --    pair w
        --        | length w > 1 = take 2 w
        --        | length w == 1 = w ++ " "
        --        | otherwise = "  "


    -- are the first two characters doubles
    --doubled :: String -> Bool
    --doubled word
    --    | length word > 1 = head word == (word !! 1)
    --    | otherwise = False

    --splitOnDoubledCon :: String -> [String]
    --splitOnDoubledCon [] = []
    --splitOnDoubledCon word = reverse $ doit word [] []
    --  where
    --    doit :: String -> String -> [String] -> [String]
    --    doit "" pref acc = reverse pref:acc
    --    doit w@(x:xs) pref acc
    --        | isConsonant x && doubled w = doit xs "" (reverse (x:pref):acc)
    --        | otherwise = doit xs (x:pref) acc

