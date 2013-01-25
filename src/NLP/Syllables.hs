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

    walkAhead :: StringZipper -> StringZipper
    walkAhead zipper@StringZipper{next = ""} = zipper
    walkAhead StringZipper{prev = p, next = (n:ns)} = StringZipper{prev = reverse (n : reverse p), next = ns}

    walkBack :: StringZipper -> StringZipper
    walkBack zipper@(StringZipper{ prev = "" }) = zipper
    walkBack StringZipper{prev = (p:ps), next = n} = StringZipper{prev = ps, next = reverse (p : reverse n)}


    countVowels :: String -> Int
    countVowels = foldl d 0
      where
        d l r
            | isVowel r = 1 + l
            | otherwise = l

    -- definitions for english letter forms
    dipthongs :: [String]
    dipthongs = ["ou", "oa", "ao", "oi", "io", "ui", "ea", "ei", "ie", "eo", "oe", "eu", "ue", "ai", "au"]

    isDipthong :: String -> Bool
    isDipthong (x:y:_) = [x,y] `elem` dipthongs
    isDipthong _ = False


    -- this can be made more general i think, 
    -- instead of hard coding '2', we should take / drop the length of the matched element
    -- 
    zipperTestHead :: (String -> Bool) -> StringZipper -> (Bool, StringZipper)
    zipperTestHead test z@StringZipper{prev = p, next = n} 
        | test n = (True, StringZipper{prev = p ++ take 2 n, next = drop 2 n})
        | otherwise = (False, z)
    zipperTestHead _ z = (False, z)


    consonantDigraphs :: [String]
    consonantDigraphs = ["th", "sh", "ph", "th", "ch", "wh"]

    vowels :: String
    vowels = "aeiouy"

    isVowel :: Char -> Bool
    isVowel c = c `elem` vowels

    isConsonant :: Char -> Bool
    isConsonant = not . isVowel







    --indexOfDoubleCon :: String -> Maybe Int
    --indexOfDoubleCon word@(x:xs) = Just 1
        --where
        --    pair :: String -> String
        --    pair w 
        --        | length w > 1 = take 2 w
        --        | length w == 1 = w ++ " "
        --        | otherwise = "  "


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
        doit "" pref acc = reverse pref:acc
        doit w@(x:xs) pref acc
            | isConsonant x && doubled w = doit xs "" (reverse (x:pref):acc)
            | otherwise = doit xs (x:pref) acc

