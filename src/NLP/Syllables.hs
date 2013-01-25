module NLP.Syllables where

--Counting Syllables

--To find the number of syllables in a word, use the following steps:

--Count the vowels in the word.
--Subtract any silent vowels, (like the silent e at the end of a word, or the second vowel when two vowels are together in a syllabl.e)
--Subtract one vowel from every diphthong (diphthongs only count as one vowel sound.)
--The number of vowels sounds left is the same as the number of syllables.


    data StringZipper = StringZipper{prev :: String, next :: String} deriving (Show)

    newStringZipper :: String -> StringZipper
    newStringZipper s = StringZipper{prev = "", next = s}

    walkAhead :: StringZipper -> StringZipper
    walkAhead zip@(StringZipper{next = ""}) = zip
    walkAhead StringZipper{prev = p, next = (n:ns)} = StringZipper{prev = (reverse (n : (reverse p))), next = ns}

    walkBack :: StringZipper -> StringZipper
    walkBack zip@(StringZipper{prev = ""}) = zip
    walkBack StringZipper{prev = (p:ps), next = n} = StringZipper{prev = ps, next = (reverse (p : (reverse n)))}


    countVowels :: String -> Int
    countVowels = foldl d 0
      where
        d l r
            | isVowel r = 1 + l
            | otherwise = l

    split :: String -> String
    split x = x


    dipthongs :: [String]
    dipthongs = ["ou", "oa", "ao", "oi", "io", "ui", "ea", "ei", "ie", "eo", "oe", "eu", "ue", "ai", "au"]

    consonantDigraphs :: [String]
    consonantDigraphs = ["th", "sh", "ph", "th", "ch", "wh"]


    vowels :: [Char]
    vowels = ['a', 'e', 'i', 'o', 'u', 'y']

    isVowel :: Char -> Bool
    isVowel c = c `elem` vowels

    isConsonant :: Char -> Bool
    isConsonant = not . isVowel

    indexOfDoubleCon :: String -> Maybe Int
    indexOfDoubleCon word@(x:xs) = Just 1
      where
        pair :: String -> String
        pair word
            | length word > 1 = take 2 word
            | length word == 1 = word ++ " "
            | otherwise = "  "


    -- are the first two characters doubles
    doubled :: String -> Bool
    doubled word
        | length word > 1 = head word == (word !! 1)
        | otherwise = False

    splitOnDoubledCon :: String -> [String]
    splitOnDoubledCon [] = []
    splitOnDoubledCon word = reverse $ doit word [] []
      where
        doit :: String -> String -> [String] -> [String]
        doit "" pref acc = (reverse pref):acc
        doit w@(x:xs) pref acc
            | isConsonant x && doubled w = doit xs "" ((reverse $ x:pref):acc)
            | otherwise = doit xs (x:pref) acc

