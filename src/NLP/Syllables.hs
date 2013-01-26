module NLP.Syllables (countSyllables) where

--Counting Syllables

    --
    -- a string zipper, we're processing a string letter by letter, keep track
    -- of letters that have been processed, and letters that haven't been processed
    --
    data StringZipper = StringZipper{prev :: String, next :: String} deriving (Show)

    walkAhead :: Int -> StringZipper -> StringZipper
    walkAhead _ zipper@StringZipper{next = ""} = zipper
    walkAhead num StringZipper{prev = p, next = n} = StringZipper{prev = p ++ take num n, next = drop num n}

    atEnd :: StringZipper -> Bool
    atEnd StringZipper{next = ""} = True
    atEnd _ = False

    countSyllables :: String -> Int
    countSyllables s = countZipperVowels (0, StringZipper{prev = "", next = s})

    countZipperVowels :: (Int, StringZipper) -> Int
    countZipperVowels (count, zipper)
        | atEnd zipper = count
        | otherwise = case zipperIsDipthong zipper of
            (True, newZip) -> countZipperVowels (count + 1, newZip)
            (False, _) -> case zipperIsDouble zipper of
                (True, newZip) -> countZipperVowels (count + 1, newZip)
                (False, _) -> case zipperIsVowel zipper of
                    (True, newZip) -> countZipperVowels (count + 1, newZip)
                    (False, _) -> countZipperVowels (count, walkAhead 1 zipper)


    -- definitions for english letter forms
    dipthongs :: [String]
    dipthongs = ["ou", "oa", "ao", "oi", "io", "ui", "ea", "ei", "ie", "eo", "oe", "eu", "ue", "ai", "au"]

    isDipthong :: String -> Bool
    isDipthong (x:y:_) = [x,y] `elem` dipthongs
    isDipthong _ = False

    vowels :: String
    vowels = "aeiouy"

    isVowel :: String -> Bool
    isVowel [] = False
    isVowel (x:_) = x `elem` vowels


    isDouble :: String -> Bool
    isDouble (x:y:_) = x `elem` vowels && x == y
    isDouble _ = False

    zipperTestHead :: (String -> Bool) -> Int -> StringZipper -> (Bool, StringZipper)
    zipperTestHead test skip z@StringZipper{next = n}
        | test n = (True, walkAhead skip z)
        | otherwise = (False, walkAhead skip z)

    zipperIsDipthong :: StringZipper -> (Bool, StringZipper)
    zipperIsDipthong = zipperTestHead isDipthong 2

    zipperIsVowel :: StringZipper -> (Bool, StringZipper)
    zipperIsVowel = zipperTestHead isVowel 1

    zipperIsDouble :: StringZipper -> (Bool, StringZipper)
    zipperIsDouble = zipperTestHead isDouble 2