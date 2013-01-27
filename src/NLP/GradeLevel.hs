module NLP.ReadingLevel where 

	import NLP.Syllables
	import Data.List.Split (splitOneOf)
	import Data.Char (isAlphaNum)



	-- http://en.wikipedia.org/wiki/Coleman-Liau_Index
	-- http://en.wikipedia.org/wiki/Flesch%E2%80%93Kincaid_readability_test

	fleschReadingEase :: String -> Float
	fleschReadingEase s = 206.835 - ( 1.015 * (fromIntegral w / fromIntegral sent)) - (84.6 * (fromIntegral syl / fromIntegral w))
	  where
	  	w = totalWords s
	  	sent = totalSentences s
	  	syl = sum $ map countSyllables $ words s 

	fleschKincaidGradeLevel :: String -> Float
	fleschKincaidGradeLevel s = 0.39 * (fromIntegral w / fromIntegral sent) + 11.8 * (fromIntegral  syl / fromIntegral w) - 15.59
	  where
	  	w = totalWords s
	  	sent = totalSentences s
	  	syl = sum $ map countSyllables $ words s 

	colemanLiauIndex :: String -> Float
	colemanLiauIndex str = (0.0588 * l) - (0.296 * s) - 15.8
	  where
	  	l = 100.0 * (fromIntegral (length (filter isAlphaNum str)) / fromIntegral (totalWords str))
	  	s = 100.0 * (fromIntegral (totalSentences str) / fromIntegral (totalWords str))

	totalWords :: String -> Int
	totalWords = length . words 

	totalSentences :: String -> Int
	totalSentences s = length options
	   where 
	     options = filter (\x -> len x > 2) $ splitOneOf ".!?" s
	     len s = length $ filter isAlphaNum s


